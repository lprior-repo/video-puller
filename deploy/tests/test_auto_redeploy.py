"""Local updater policy tests without networks or launchd changes."""
import json
from pathlib import Path
import sys
import tempfile
import unittest
from unittest.mock import patch

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / 'macos'))
import auto_redeploy


class AutoRedeployTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix='updater-policy-')
        self.addCleanup(self.temp.cleanup)
        self.app = Path(self.temp.name)
        (self.app / 'deployment-source').mkdir()
        (self.app / 'RELEASE.json').write_text(json.dumps({'commit': 'old'}))
        self.installation = {'app': self.app, 'env': {}, 'agent': self.app / 'agent.plist', 'target': 'test-only'}
        self.actions = []

    def run_fake(self, args, **kwargs):
        self.actions.append(args)
        return 'new' if args[:2] == ['git', 'rev-parse'] else ''

    def test_active_poll_defers_without_fetch_or_restart(self):
        result = auto_redeploy.once(self.installation, run=self.run_fake,
            health_check=lambda *a, **kw: {'downloads': {'state': 'running'}},
            ci_check=lambda *a: self.fail('CI must not run during a poll'))
        self.assertEqual(result['status'], 'deferred')
        self.assertEqual(self.actions, [['launchctl', 'print', 'test-only']])

    def test_failed_or_pending_ci_never_checks_out_or_executes_candidate(self):
        def pending(*args): raise RuntimeError('CI pending')
        with self.assertRaisesRegex(RuntimeError, 'CI pending'):
            auto_redeploy.once(self.installation, run=self.run_fake, ci_check=pending,
                health_check=lambda *a, **kw: {'downloads': {'state': 'retrying'}})
        self.assertFalse(any(action[:2] == ['git', 'checkout'] for action in self.actions))
        self.assertFalse(any('redeploy.py' in item for action in self.actions for item in action))

    def test_ci_approved_exact_commit_is_checked_out_before_deployer_runs(self):
        checked = []
        result = auto_redeploy.once(self.installation, run=self.run_fake,
            ci_check=lambda source, revision: checked.append(revision),
            health_check=lambda *a, **kw: {'downloads': {'state': 'healthy'}})
        self.assertEqual(checked, ['new'])
        self.assertEqual(result, {'status': 'deployed', 'commit': 'new'})
        self.assertEqual(self.actions[-2], ['git', 'checkout', '--detach', 'new'])
        self.assertTrue(self.actions[-1][1].endswith('/deploy/macos/redeploy.py'))

    def test_unchanged_commit_has_no_deployment(self):
        (self.app / 'RELEASE.json').write_text(json.dumps({'commit': 'new'}))
        result = auto_redeploy.once(self.installation, run=self.run_fake,
            ci_check=lambda *a: self.fail('No deployment required'),
            health_check=lambda *a, **kw: {})
        self.assertEqual(result['status'], 'current')
        self.assertEqual(len(self.actions), 3)


if __name__ == '__main__': unittest.main()
