"""Session export regressions using fake cookies; never read a real browser."""
from contextlib import redirect_stdout
from http.cookiejar import Cookie, MozillaCookieJar
import importlib.util
import io
import os
from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch


HELPER = Path(__file__).resolve().parents[1] / "macos/export_youtube_session.py"
SPEC = importlib.util.spec_from_file_location("export_youtube_session", HELPER)
MODULE = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(MODULE)


def cookie(domain, name="SID", value="fake-session-value"):
    return Cookie(0, name, value, None, False, domain, True, domain.startswith("."),
                  "/", True, True, None, True, None, None, {})


class YouTubeSessionTests(unittest.TestCase):
    def test_exports_only_youtube_domains_with_private_permissions(self):
        with tempfile.TemporaryDirectory() as directory:
            output = Path(directory) / "auth/youtube.cookies.txt"
            source = [cookie(".youtube.com"), cookie("music.youtube.com", "MUSIC"),
                      cookie(".google.com"), cookie("evilyoutube.com"),
                      cookie("youtube.com.example.org"), cookie(".unrelated.example")]
            result = MODULE.export_session(output, extractor=lambda *a, **kw: source)
            jar = MozillaCookieJar(str(output))
            jar.load(ignore_discard=True, ignore_expires=True)
            self.assertEqual({item.domain for item in jar}, {".youtube.com", "music.youtube.com"})
            self.assertEqual(result["cookie_count"], 2)
            self.assertEqual(output.stat().st_mode & 0o777, 0o600)
            self.assertEqual(output.parent.stat().st_mode & 0o777, 0o700)
            self.assertNotIn("fake-session-value", repr(result))

    def test_missing_youtube_session_preserves_existing_export(self):
        with tempfile.TemporaryDirectory() as directory:
            output = Path(directory) / "youtube.cookies.txt"
            output.write_text("previous export")
            with self.assertRaises(RuntimeError):
                MODULE.export_session(output, extractor=lambda *a, **kw: [cookie("google.com")])
            self.assertEqual(output.read_text(), "previous export")

    def test_custom_output_does_not_change_existing_directory_permissions(self):
        with tempfile.TemporaryDirectory() as directory:
            parent = Path(directory)
            parent.chmod(0o755)
            output = parent / "youtube.cookies.txt"
            MODULE.export_session(output, extractor=lambda *a, **kw: [cookie("youtube.com")])
            self.assertEqual(parent.stat().st_mode & 0o777, 0o755)
            self.assertEqual(output.stat().st_mode & 0o777, 0o600)

    def test_failed_save_keeps_existing_file_and_removes_temporary_file(self):
        with tempfile.TemporaryDirectory() as directory:
            output = Path(directory) / "youtube.cookies.txt"
            output.write_text("previous export")
            with patch.object(MODULE.MozillaCookieJar, "save", side_effect=OSError("disk full")):
                with self.assertRaises(OSError):
                    MODULE.export_session(output, extractor=lambda *a, **kw: [cookie("youtube.com")])
            self.assertEqual(output.read_text(), "previous export")
            self.assertEqual(list(Path(directory).iterdir()), [output])

    def test_cli_error_does_not_print_sensitive_exception(self):
        output = io.StringIO()
        with patch("sys.argv", [str(HELPER)]), patch.object(MODULE, "export_session", side_effect=RuntimeError("fake-sensitive-value")), redirect_stdout(output):
            self.assertEqual(MODULE.main(), 1)
        self.assertNotIn("fake-sensitive-value", output.getvalue())


if __name__ == "__main__":
    unittest.main()
