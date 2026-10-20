#!/usr/bin/env python3
"""Optional local CI-gated deployment trigger. Never accepts PR builds."""
import argparse
import json
import os
from pathlib import Path
import plistlib
import shutil
import subprocess
import sys

from redeploy_checks import command, read_installation, require_ci, wait_health

REPOSITORY = 'https://github.com/lprior-repo/video-puller.git'
LABEL = 'local.video-puller.updater'
# A separate BEAM process keeps the updater alive while the application restarts,
# and uses the same approved Erlang executable for macOS permission attribution.
RUNNER = '''Port = open_port({spawn_executable, os:getenv("UPDATER_PYTHON")},
[binary,exit_status,stderr_to_stdout,{args,[os:getenv("UPDATER_SCRIPT"),"--once","--agent",os:getenv("UPDATER_AGENT")]}]),
Loop = fun F() -> receive {Port,{data,Bytes}} -> io:put_chars(Bytes), F(); {Port,{exit_status,Code}} -> halt(Code) end end,
Loop().'''


def once(installation, *, run=command, ci_check=require_ci, health_check=wait_health):
    app = installation['app']
    run(['launchctl', 'print', installation['target']], timeout=10)
    health = health_check(installation['env'], timeout=5)
    if health.get('downloads', {}).get('state') == 'running':
        return {'status': 'deferred', 'reason': 'subscription poll is active'}
    source = app / 'deployment-source'
    if not source.exists():
        run(['git', 'clone', '--branch', 'main', '--single-branch', REPOSITORY, str(source)])
    run(['git', 'fetch', 'origin', 'main'], cwd=source)
    revision = run(['git', 'rev-parse', 'FETCH_HEAD'], cwd=source)
    current = json.loads((app / 'RELEASE.json').read_text())['commit']
    if revision == current:
        return {'status': 'current', 'commit': current}
    ci_check(source, revision)
    run(['git', 'checkout', '--detach', revision], cwd=source)
    logs = app / 'deployments'
    logs.mkdir(mode=0o700, exist_ok=True)
    log = logs / 'updater-deploy.log'
    log.touch(mode=0o600, exist_ok=True)
    os.chmod(log, 0o600)
    run([sys.executable, str(source / 'deploy/macos/redeploy.py'), '--source', str(source),
         '--agent', str(installation['agent']), '--wait-idle'], timeout=1800, log=log)
    return {'status': 'deployed', 'commit': revision}


def install(installation):
    app = installation['app']
    tools = app / 'bin/deploy-tools'
    tools.mkdir(mode=0o700, parents=True, exist_ok=True)
    source = Path(__file__).resolve().parent
    for name in ('auto_redeploy.py', 'redeploy.py', 'redeploy_checks.py'):
        target = tools / name
        if (source / name) != target:
            shutil.copy2(source / name, target)
        os.chmod(target, 0o700)
    logs = app / 'deployments'
    logs.mkdir(mode=0o700, exist_ok=True)
    for name in ('updater.log', 'updater-error.log'):
        path = logs / name
        path.touch(mode=0o600, exist_ok=True)
    path = Path.home() / ('Library/LaunchAgents/' + LABEL + '.plist')
    if path.exists():
        subprocess.run(['launchctl', 'bootout', 'gui/' + str(os.getuid()) + '/' + LABEL],
                       stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    config = {'Label': LABEL, 'ProgramArguments': [shutil.which('erl') or '/opt/homebrew/bin/erl',
              '+S', '1:1', '+SDcpu', '1', '+SDio', '1', '-noshell', '-eval', RUNNER],
              'WorkingDirectory': str(app), 'RunAtLoad': True, 'StartInterval': 300,
              'EnvironmentVariables': {'PATH': installation['env'].get('PATH', '/opt/homebrew/bin:/usr/bin:/bin'), 'HOME': str(Path.home()),
                 'ERL_CRASH_DUMP': '/dev/null', 'UPDATER_PYTHON': sys.executable,
                 'UPDATER_SCRIPT': str(tools / 'auto_redeploy.py'), 'UPDATER_AGENT': str(installation['agent'])},
              'StandardOutPath': str(logs / 'updater.log'), 'StandardErrorPath': str(logs / 'updater-error.log')}
    temporary = path.with_suffix('.plist.tmp')
    temporary.write_bytes(plistlib.dumps(config));os.chmod(temporary, 0o600);os.replace(temporary, path)
    command(['launchctl', 'bootstrap', 'gui/' + str(os.getuid()), str(path)], timeout=30)
    return {'installed': True, 'label': LABEL, 'check_interval_seconds': 300,
            'waits_for_active_poll': True, 'requires_exact_main_ci': True}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    action = parser.add_mutually_exclusive_group(required=True)
    action.add_argument('--once', action='store_true')
    action.add_argument('--install', action='store_true', help='Opt in to automatic deployment every five minutes after CI passes')
    parser.add_argument('--agent', type=Path, default=Path.home() / 'Library/LaunchAgents/local.video-puller.plist')
    args = parser.parse_args()
    installation = read_installation(args.agent)
    result = install(installation) if args.install else once(installation)
    print(json.dumps(result, indent=2))


if __name__ == '__main__':
    try: main()
    except Exception as error:
        print('Automatic deployment deferred: ' + str(error), file=sys.stderr)
        raise SystemExit(1)
