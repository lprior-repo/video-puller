#!/usr/bin/env python3
"""Build, verify and transactionally redeploy this Mac's existing installation."""
import argparse
from contextlib import closing
import datetime
import fcntl
import hashlib
import json
import os
from pathlib import Path
import shutil
import sqlite3
import sys
import time

from redeploy_checks import command, read_installation, require_ci, verify_plex, verify_storage, wait_health


def stop_service(installation, *, run=command, pause=time.sleep):
    """bootout returns before launchd has necessarily removed the old job."""
    run(['launchctl', 'bootout', installation['target']], timeout=45)
    deadline = time.monotonic() + 45
    while time.monotonic() < deadline:
        try: run(['launchctl', 'print', installation['target']], timeout=5)
        except RuntimeError: return
        pause(.5)
    raise RuntimeError('LaunchAgent did not finish unloading; release unchanged')


def start_service(installation, *, run=command, pause=time.sleep):
    """Retry launchd's transient EIO while a removed label is being released."""
    deadline = time.monotonic() + 45
    while time.monotonic() < deadline:
        try:
            run(['launchctl', 'bootstrap', 'gui/' + str(os.getuid()), str(installation['agent'])], timeout=10)
            return
        except RuntimeError:
            try: run(['launchctl', 'print', installation['target']], timeout=5)
            except RuntimeError: pause(.5)
            else: return  # A successful load can race a retry; health is checked next.
    raise RuntimeError('LaunchAgent could not restart after bounded retries')


def write_json(path, value):
    temporary = path.with_suffix(path.suffix + '.tmp')
    temporary.write_text(json.dumps(value, indent=2) + '\n')
    os.chmod(temporary, 0o600)
    os.replace(temporary, path)


def stage_release(source, stage, revision):
    stage.mkdir(mode=0o700)
    shutil.copytree(source / 'build/erlang-shipment', stage / 'shipment')
    shutil.copytree(source / 'priv', stage / 'priv')
    (stage / 'deploy-tools').mkdir(mode=0o700)
    for name in ('redeploy.py', 'redeploy_checks.py', 'auto_redeploy.py'):
        shutil.copy2(source / 'deploy/macos' / name, stage / 'deploy-tools' / name)
    for name in ('README.md', 'gleam.toml'):
        shutil.copy2(source / name, stage / name)
    shutil.copy2(source / 'deploy/macos/export_youtube_session.py', stage / 'helper')
    os.chmod(stage / 'helper', 0o700)
    hashes = {str(file.relative_to(stage / 'shipment')): hashlib.sha256(file.read_bytes()).hexdigest()
              for file in (stage / 'shipment').rglob('*') if file.is_file()}
    write_json(stage / 'RELEASE.json', {
        'commit': revision, 'branch': 'main',
        'deployed_at': datetime.datetime.now(datetime.timezone.utc).isoformat(),
        'shipment_sha256': hashes,
        'helper_sha256': hashlib.sha256((stage / 'helper').read_bytes()).hexdigest(),
    })


def activate(installation, stage, revision, *, run=command,
             health_check=wait_health, plex_check=verify_plex):
    """Rollback code and SQLite if startup or Plex verification fails."""
    app, env = installation['app'], installation['env']
    backup = stage.parent / 'rollback'
    backup.mkdir(mode=0o700)
    agent_bytes = installation['agent'].read_bytes()
    (backup / 'agent.plist').write_bytes(agent_bytes)
    os.chmod(backup / 'agent.plist', 0o600)
    db = Path(env['DB_PATH'])
    entries = [('shipment', app / 'build/erlang-shipment'),
               ('priv', app / 'priv'), ('deploy-tools', app / 'bin/deploy-tools'), ('helper', app / 'bin/export-youtube-session.py'),
               ('README.md', app / 'README.md'), ('gleam.toml', app / 'gleam.toml'),
               ('RELEASE.json', app / 'RELEASE.json')]
    moved, installed = [], []
    stop_service(installation, run=run)
    try:
        if db.is_file():
            # A closed WAL database may need to recreate its shared-memory files.
            # rw permits that recovery but never creates a missing source database.
            with closing(sqlite3.connect('file:' + str(db) + '?mode=rw', uri=True)) as current:
                with closing(sqlite3.connect(backup / '.database.tmp')) as saved:
                    current.backup(saved)
                    if saved.execute('pragma quick_check').fetchone() != ('ok',):
                        raise RuntimeError('Pre-deployment database backup failed validation')
            (backup / '.database.tmp').rename(backup / 'database.sqlite3')
            os.chmod(backup / 'database.sqlite3', 0o600)
        for name, destination in entries:
            destination.parent.mkdir(parents=True, exist_ok=True)
            if destination.exists():
                destination.rename(backup / name)
                moved.append((name, destination))
            (stage / name).rename(destination)
            installed.append(destination)
        start_service(installation, run=run)
        health = health_check(env)
        plex = plex_check(env)
        if installation['agent'].read_bytes() != agent_bytes:
            raise RuntimeError('LaunchAgent configuration changed during deployment')
        return {'deployed': True, 'commit': revision, 'health': health, 'plex': plex,
                'configuration_preserved': True, 'rollback_directory': str(backup)}
    except BaseException:
        # Stop the candidate before restoring its database or executable files.
        try:
            stop_service(installation, run=run)
        except Exception:
            try: run(['launchctl', 'print', installation['target']], timeout=10)
            except Exception: pass  # Bootstrap may have failed before loading the job.
            else: raise RuntimeError('Candidate could not stop; rollback backup preserved')
        failed = stage.parent / 'failed-candidate'
        failed.mkdir(mode=0o700, exist_ok=True)
        for destination in reversed(installed):
            destination.rename(failed / ('shipment' if destination.name == 'erlang-shipment' else destination.name))
        for name, destination in reversed(moved):
            (backup / name).rename(destination)
        if (backup / 'database.sqlite3').is_file():
            for suffix in ('', '-wal', '-shm'):
                live = Path(str(db) + suffix)
                if live.exists(): live.rename(failed / ('database.sqlite3' + suffix))
            shutil.copy2(backup / 'database.sqlite3', db)
        installation['agent'].write_bytes(agent_bytes)
        os.chmod(installation['agent'], 0o600)
        start_service(installation, run=run)
        health_check(env)
        raise


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--source', type=Path, default=Path(__file__).resolve().parents[2])
    parser.add_argument('--agent', type=Path, default=Path.home() / 'Library/LaunchAgents/local.video-puller.plist')
    parser.add_argument('--check', action='store_true', help='Verify the current installation without building or changing it')
    parser.add_argument('--wait-idle', action='store_true', help='Defer if a subscription poll starts while building')
    args = parser.parse_args()
    installation = read_installation(args.agent)
    app, env = installation['app'], installation['env']
    directory = app / 'deployments'
    directory.mkdir(mode=0o700, exist_ok=True)
    with (directory / '.lock').open('a') as lock:
        try: fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
        except BlockingIOError: raise RuntimeError('A deployment is already running')
        verify_storage(env)
        command(['launchctl', 'print', installation['target']])
        if args.check:
            print(json.dumps({'health': wait_health(env), 'plex': verify_plex(env)}, indent=2))
            return
        source = args.source.expanduser().resolve()
        if command(['git', 'status', '--porcelain'], cwd=source):
            raise RuntimeError('Commit or stash source changes before redeploying')
        revision = command(['git', 'rev-parse', 'HEAD'], cwd=source)
        ci_url = require_ci(source, revision)
        stamp = datetime.datetime.now(datetime.timezone.utc).strftime('%Y%m%dT%H%M%S.%fZ')
        transaction = directory / (stamp + '-' + revision[:8])
        transaction.mkdir(mode=0o700)
        log = transaction / 'build.log'
        log.touch(mode=0o600)
        print('Building and testing ' + revision[:8], flush=True)
        command(['just', 'ci'], cwd=source, log=log, timeout=600)
        command([sys.executable, '-m', 'unittest', 'discover', '-s', 'deploy/tests', '-v'], cwd=source, log=log)
        command(['gleam', 'export', 'erlang-shipment'], cwd=source, log=log, timeout=600)
        if command(['git', 'status', '--porcelain'], cwd=source) or command(['git', 'rev-parse', 'HEAD'], cwd=source) != revision:
            raise RuntimeError('Source changed during build; installation unchanged')
        stage = transaction / 'staged'
        stage_release(source, stage, revision)
        verify_storage(env)
        # Refuse to take a healthy service offline while Plex verification is unavailable.
        verify_plex(env)
        if args.wait_idle and wait_health(env)['downloads']['state'] == 'running':
            raise RuntimeError('Subscription poll became active; installation unchanged')
        print('Activating tested release; preserving every LaunchAgent setting', flush=True)
        try:
            result = activate(installation, stage, revision)
        except BaseException as error:
            write_json(transaction / 'result.json', {'commit': revision, 'deployed': False,
                'rollback_attempted': True, 'error_type': type(error).__name__, 'error': str(error)})
            raise
        result['ci_url'] = ci_url
        write_json(transaction / 'result.json', result)
        print(json.dumps(result, indent=2))


if __name__ == '__main__':
    try: main()
    except Exception as error:
        print('Redeployment failed: ' + str(error), file=sys.stderr)
        raise SystemExit(1)
