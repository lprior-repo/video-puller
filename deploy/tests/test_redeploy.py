"""Transactional deployment tests. No test invokes real launchctl or a browser."""
import json
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
        with sqlite3.connect(self.db) as conn:
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

    def run_fake(self, args, **kwargs):
        self.actions.append(args)
        self.assertEqual(args[0], 'launchctl')
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
        self.assertEqual([action[1] for action in self.actions], ['bootout', 'bootstrap'])
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

    def test_plex_failure_restores_old_code_database_and_agent(self):
        def fail_plex(_):
            with sqlite3.connect(self.db) as conn: conn.execute('update settings set enabled=0')
            raise RuntimeError('Plex stream failed')
        with self.assertRaisesRegex(RuntimeError, 'Plex stream failed'):
            redeploy.activate(self.installation, self.stage, 'new-sha', run=self.run_fake,
                              health_check=lambda _: {'status': 'ok'}, plex_check=fail_plex)
        self.assert_old_restored()
        self.assertEqual([action[1] for action in self.actions], ['bootout', 'bootstrap', 'bootout', 'bootstrap'])

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
