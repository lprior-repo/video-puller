"""Transactional deployment tests. No test invokes real launchctl or a browser."""
import json
from contextlib import closing
from pathlib import Path
import plistlib
import sqlite3
import sys
import tempfile
import unittest
from unittest.mock import patch

MACOS = Path(__file__).resolve().parents[1] / 'macos'
sys.path.insert(0, str(MACOS))
import redeploy
import redeploy_checks


class RedeployTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix='redeploy-test-')
        self.addCleanup(self.temp.cleanup)
        root = Path(self.temp.name).resolve()
        self.app = root / 'app'
        self.agent = root / 'agent.plist'
        self.data = root / 'media'
        self.data.mkdir()
        self.db = self.data / 'video.db'
        with closing(sqlite3.connect(self.db)) as conn, conn:
            conn.execute('create table settings (enabled integer)')
            conn.execute('insert into settings values (1)')
        self.env = {'DB_PATH': str(self.db), 'DATA_DIR': str(self.data),
                    'YOUTUBE_COOKIE_FILE': str(root / 'private.cookies.txt'),
                    'YOUTUBE_SESSION_BROWSER': 'chrome', 'SUBSCRIPTION_BACKFILL': 'false',
                    'STORAGE_VOLUME_UUID': 'keep-current-uuid', 'CUSTOM_SETTING': 'must survive'}
        self.agent.write_bytes(plistlib.dumps({'Label': 'local.video-puller',
            'WorkingDirectory': str(self.app), 'KeepAlive': True,
            'ProgramArguments': [str(self.app / 'build/erlang-shipment/entrypoint.sh'), 'run'],
            'EnvironmentVariables': self.env}))
        self.agent_bytes = self.agent.read_bytes()
        self.installation = redeploy_checks.read_installation(self.agent)
        self.stage = root / 'transaction/staged'
        for directory in ('shipment', 'priv', 'deploy-tools'):
            (self.stage / directory).mkdir(parents=True)
            (self.stage / directory / 'version.txt').write_text('new')
        for name in ('helper', 'README.md', 'gleam.toml', 'RELEASE.json'):
            (self.stage / name).write_text('new')
        self.destinations = {'shipment': self.app / 'build/erlang-shipment',
                             'priv': self.app / 'priv', 'deploy-tools': self.app / 'bin/deploy-tools', 'helper': self.app / 'bin/export-youtube-session.py',
                             'README.md': self.app / 'README.md', 'gleam.toml': self.app / 'gleam.toml',
                             'RELEASE.json': self.app / 'RELEASE.json'}
        for name, path in self.destinations.items():
            if name in ('shipment', 'priv', 'deploy-tools'):
                path.mkdir(parents=True)
                (path / 'version.txt').write_text('old')
            else:
                path.parent.mkdir(parents=True, exist_ok=True)
                path.write_text('old')
        self.actions = []
        self.loaded = True

    def run_fake(self, args, **kwargs):
        self.actions.append(args)
        self.assertEqual(args[0], 'launchctl')
        if args[1] == 'bootout': self.loaded = False
        if args[1] == 'bootstrap': self.loaded = True
        if args[1] == 'print' and not self.loaded: raise RuntimeError('No such service')
        return ''

    def assert_old_restored(self):
        for name, path in self.destinations.items():
            self.assertEqual((path / 'version.txt' if path.is_dir() else path).read_text(), 'old')
        self.assertEqual(self.agent.read_bytes(), self.agent_bytes)
        with sqlite3.connect(self.db) as conn:
            self.assertEqual(conn.execute('select enabled from settings').fetchall(), [(1,)])

    def test_success_preserves_every_machine_setting_and_database(self):
        result = redeploy.activate(self.installation, self.stage, 'new-sha', run=self.run_fake,
                                   health_check=lambda _: {'status': 'ok'}, plex_check=lambda _: {'reachable': True})
        self.assertTrue(result['deployed'])
        self.assertEqual(self.agent.read_bytes(), self.agent_bytes)
        self.assertEqual((self.destinations['shipment'] / 'version.txt').read_text(), 'new')
        self.assertEqual([action[1] for action in self.actions if action[1] != 'print'], ['bootout', 'bootstrap'])
        backup = Path(result['rollback_directory']) / 'database.sqlite3'
        self.assertEqual(backup.stat().st_mode & 0o777, 0o600)
        with sqlite3.connect(backup) as conn:
            self.assertEqual(conn.execute('select enabled from settings').fetchall(), [(1,)])

    def test_wal_backup_captures_committed_rows_as_a_standalone_database(self):
        writer = sqlite3.connect(self.db)
        self.addCleanup(writer.close)
        writer.execute('pragma journal_mode=wal')
        writer.execute('update settings set enabled=7')
        writer.commit()
        result = redeploy.activate(self.installation, self.stage, 'new-sha', run=self.run_fake,
                                   health_check=lambda _: {}, plex_check=lambda _: {})
        copied = self.data / 'portable-backup.db'
        import shutil
        shutil.copyfile(Path(result['rollback_directory']) / 'database.sqlite3', copied)
        with sqlite3.connect(copied) as check:
            self.assertEqual(check.execute('select enabled from settings').fetchall(), [(7,)])
            self.assertEqual(check.execute('pragma quick_check').fetchone(), ('ok',))

    def test_closed_wal_backup_can_initialize_shared_memory(self):
        with sqlite3.connect(self.db) as writer:
            writer.execute('pragma journal_mode=wal')
            writer.execute('update settings set enabled=9')
        writer.execute('pragma wal_checkpoint(TRUNCATE)')
        writer.close()
        # Reproduce BEAM's final-close cleanup, including SQLite builds that
        # otherwise retain empty WAL files after the last writer closes.
        for suffix in ('-wal', '-shm'):
            Path(str(self.db) + suffix).unlink(missing_ok=True)
        self.assertFalse(Path(str(self.db) + '-wal').exists())
        connect = sqlite3.connect
        def cold_wal_connect(database, *args, **kwargs):
            # Reproduce macOS SQLite rejecting a cold WAL in read-only mode.
            if isinstance(database, str) and database.startswith('file:') and 'mode=ro' in database:
                raise sqlite3.OperationalError('unable to open database file')
            return connect(database, *args, **kwargs)
        with patch.object(redeploy.sqlite3, 'connect', side_effect=cold_wal_connect):
            result = redeploy.activate(self.installation, self.stage, 'new-sha', run=self.run_fake,
                                       health_check=lambda _: {}, plex_check=lambda _: {})
        with connect(Path(result['rollback_directory']) / 'database.sqlite3') as saved:
            self.assertEqual(saved.execute('select enabled from settings').fetchall(), [(9,)])
            self.assertEqual(saved.execute('pragma quick_check').fetchone(), ('ok',))
        with connect(self.db) as current:
            self.assertEqual(current.execute('select enabled from settings').fetchall(), [(9,)])

    def test_plex_failure_restores_old_code_database_and_agent(self):
        def fail_plex(_):
            with sqlite3.connect(self.db) as conn: conn.execute('update settings set enabled=0')
            raise RuntimeError('Plex stream failed')
        with self.assertRaisesRegex(RuntimeError, 'Plex stream failed'):
            redeploy.activate(self.installation, self.stage, 'new-sha', run=self.run_fake,
                              health_check=lambda _: {'status': 'ok'}, plex_check=fail_plex)
        self.assert_old_restored()
        self.assertEqual([action[1] for action in self.actions if action[1] != 'print'], ['bootout', 'bootstrap', 'bootout', 'bootstrap'])

    def test_failed_startup_rolls_back_and_checks_restored_service(self):
        health = iter([False, True])
        def check(_):
            if not next(health): raise RuntimeError('candidate unhealthy')
            return {'status': 'ok'}
        with self.assertRaisesRegex(RuntimeError, 'candidate unhealthy'):
            redeploy.activate(self.installation, self.stage, 'new-sha', run=self.run_fake,
                              health_check=check, plex_check=lambda _: {})
        self.assert_old_restored()
        self.assertEqual(list(health), [])

    def test_failed_stop_never_replaces_code_or_database(self):
        def cannot_stop(*args, **kwargs): raise RuntimeError('job did not stop')
        with self.assertRaisesRegex(RuntimeError, 'job did not stop'):
            redeploy.activate(self.installation, self.stage, 'new-sha', run=cannot_stop)
        self.assert_old_restored()

    def test_missing_candidate_asset_restores_assets_already_swapped(self):
        (self.stage / 'priv/version.txt').unlink()
        (self.stage / 'priv').rmdir()
        with self.assertRaises(FileNotFoundError):
            redeploy.activate(self.installation, self.stage, 'new-sha', run=self.run_fake,
                              health_check=lambda _: {}, plex_check=lambda _: {})
        self.assert_old_restored()

    def test_incompatible_launchagent_is_rejected_before_commands(self):
        config = plistlib.loads(self.agent.read_bytes())
        config['ProgramArguments'][0] = '/unrelated/program'
        self.agent.write_bytes(plistlib.dumps(config))
        with self.assertRaisesRegex(RuntimeError, 'Unexpected LaunchAgent'):
            redeploy_checks.read_installation(self.agent)
        self.assertEqual(self.actions, [])

    def test_stop_waits_for_launchd_to_release_the_job(self):
        probes = iter([True, True, False])
        pauses = []
        def run(args, **kwargs):
            if args[1] == 'print' and not next(probes):
                raise RuntimeError('No such service')
        redeploy.stop_service(self.installation, run=run, pause=pauses.append)
        self.assertEqual(pauses, [.5, .5])
        self.assertEqual(list(probes), [])

    def test_bootstrap_retries_transient_eio_after_unload(self):
        attempts = []
        pauses = []
        def run(args, **kwargs):
            if args[1] == 'bootstrap':
                attempts.append(True)
                if len(attempts) < 3: raise RuntimeError('Bootstrap failed: 5')
            else: raise RuntimeError('No such service')
        redeploy.start_service(self.installation, run=run, pause=pauses.append)
        self.assertEqual(len(attempts), 3)
        self.assertEqual(pauses, [.5, .5])

    def test_start_timeout_is_bounded(self):
        with patch.object(redeploy.time, 'monotonic', side_effect=[0, 46]):
            with self.assertRaisesRegex(RuntimeError, 'bounded retries'):
                redeploy.start_service(self.installation, run=lambda *a, **k: self.fail('deadline expired'))

    def test_already_loaded_retry_continues_to_caller_health_check(self):
        def run(args, **kwargs):
            if args[1] == 'bootstrap': raise RuntimeError('already loaded')
            return 'loaded'
        redeploy.start_service(self.installation, run=run, pause=lambda _: self.fail('no retry needed'))

    def test_stop_timeout_never_swaps_release_assets(self):
        with patch.object(redeploy.time, 'monotonic', side_effect=[0, 46]):
            with self.assertRaisesRegex(RuntimeError, 'did not finish unloading'):
                redeploy.activate(self.installation, self.stage, 'new-sha', run=lambda *a, **k: '')
        self.assert_old_restored()

    def test_ci_gate_requires_exact_main_revision_and_latest_successful_run(self):
        good = {'id': 1, 'head_sha': 'expected', 'head_branch': 'main', 'event': 'push',
                'path': '.github/workflows/test.yml', 'status': 'completed', 'conclusion': 'success', 'html_url': 'https://example.invalid/run'}
        with patch.object(redeploy_checks, 'command', return_value=json.dumps({'workflow_runs': [good]})):
            self.assertEqual(redeploy_checks.require_ci(self.app, 'expected'), good['html_url'])
            with self.assertRaises(RuntimeError): redeploy_checks.require_ci(self.app, 'different')
        pending = {**good, 'id': 2, 'status': 'in_progress', 'conclusion': None}
        with patch.object(redeploy_checks, 'command', return_value=json.dumps({'workflow_runs': [good, pending]})):
            with self.assertRaisesRegex(RuntimeError, 'not passed'): redeploy_checks.require_ci(self.app, 'expected')


if __name__ == '__main__': unittest.main()
