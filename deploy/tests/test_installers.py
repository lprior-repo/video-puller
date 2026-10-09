"""Filesystem-only installer regressions; never install tools or start services.

Run from the repository root with:
    python3 -m unittest discover -s deploy/tests -v

System/package-manager commands are replaced by local stubs. The Linux copy
also redirects its fixed install paths and bypasses only the root-user guard.
This checks shell control flow and generated files, not a real systemd/launchd
installation. The rsync stub implements only the installer's selected trees;
its exact filter arguments are checked separately.
"""

import json
import os
from pathlib import Path
import plistlib
import re
import shlex
import shutil
import sqlite3
import stat
import subprocess
import sys
import tempfile
import unittest


DEPLOY = Path(__file__).resolve().parents[1]
BASH = shutil.which("bash")
STUB = r'''
import json
import os
from pathlib import Path
import plistlib
import shutil
import sys

name = Path(sys.argv[0]).name
args = sys.argv[1:]
with open(os.environ["COMMAND_LOG"], "a") as log:
    log.write(json.dumps([name, *args]) + "\n")

if name == "brew":
    if args == ["--prefix"]:
        print(os.environ["BREW_PREFIX"])
    elif args[:2] == ["list", "--formula"]:
        sys.exit(1 if args[-1] == "yt-dlp" else 0)
    elif args == ["install", "yt-dlp"]:
        if os.environ.get("MISSING_YT_DLP") != "1":
            path = Path(os.environ["BREW_PREFIX"]) / "bin" / "yt-dlp"
            path.write_text("#!/bin/sh\nexit 0\n")
            path.chmod(0o755)
    else:
        raise AssertionError((name, args))
elif name == "pipx":
    assert args == ["install", "ytdl-sub"], args
    path = Path(os.environ["HOME"]) / ".local/bin/ytdl-sub"
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text("#!/bin/sh\nexit 0\n")
    path.chmod(0o755)
elif name == "erl":
    print("28")
elif name == "uname":
    print("Linux")
elif name == "curl":
    pass  # Successful local health check, without contacting a network.
elif name == "plutil":
    assert args[0] == "-lint", args
    with open(args[1], "rb") as source:
        plistlib.load(source)
    if os.environ.get("FAIL_PLUTIL") == "1":
        sys.exit(1)
elif name == "rsync":
    assert args[:-2] == [
        "-a", "--delete", "--include", "build/", "--include",
        "build/erlang-shipment/***", "--include", "priv/***",
        "--include", "gleam.toml", "--exclude", "*",
    ], args
    source, target = map(Path, args[-2:])
    for relative in ("build/erlang-shipment", "priv"):
        destination = target / relative
        if destination.exists():
            shutil.rmtree(destination)
        shutil.copytree(source / relative, destination)
    shutil.copy2(source / "gleam.toml", target / "gleam.toml")
elif name == "sudo":
    assert args == [
        "-u", "video-puller", "env", "HOME=" + os.environ["TEST_DATA_DIR"],
        "XDG_CACHE_HOME=" + os.environ["TEST_DATA_DIR"] + "/.cache",
        "gleam", "export", "erlang-shipment",
    ], args
    commands = [json.loads(line) for line in Path(os.environ["COMMAND_LOG"]).read_text().splitlines()]
    assert ["chown", "-R", "video-puller:video-puller", os.getcwd()] in commands
    assert Path("gleam.toml").is_file()
elif name in ("id", "chown", "systemctl", "gleam"):
    assert os.environ.get("TEST_LINUX") == "1", (name, args)
elif name in ("git", "launchctl", "useradd"):
    raise AssertionError("Unexpected external action: " + name + " " + repr(args))
else:
    raise AssertionError((name, args))
'''


class InstallerTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix="video-puller-deploy-")
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        self.bin = self.root / "brew & <runtime>" / "bin"
        self.bin.mkdir(parents=True)
        for command in (
            "brew", "pipx", "erl", "uname", "curl", "plutil", "rsync", "git",
            "launchctl", "sudo", "id", "chown", "systemctl", "gleam", "useradd",
        ):
            path = self.bin / command
            path.write_text("#!" + sys.executable + "\n" + STUB)
            path.chmod(0o755)
        self.home = self.root / "home & <user>"
        self.home.mkdir()
        self.source = self.root / "checkout"
        self.source.mkdir()
        (self.source / "gleam.toml").write_text('name = "test"\n')
        (self.source / "build/erlang-shipment").mkdir(parents=True)
        (self.source / "build/erlang-shipment/entrypoint.sh").write_text("new release\n")
        (self.source / "priv/static").mkdir(parents=True)
        (self.source / "priv/static/app.css").write_text("new static asset\n")
        self.install = self.root / "install & <app>"
        self.data = self.root / "data & <media>"
        self.plists = self.root / "agents"
        self.plist = self.plists / "local.video-puller.plist"
        self.log = self.root / "commands.jsonl"
        self.env = os.environ.copy()
        for key in ("BASH_ENV", "ENV", "SHELLOPTS", "BASHOPTS"):
            self.env.pop(key, None)
        self.env.update({
            "HOME": str(self.home), "PATH": str(self.bin) + ":/usr/bin:/bin",
            "COMMAND_LOG": str(self.log), "BREW_PREFIX": str(self.bin.parent),
            "INSTALL_DIR": str(self.install), "DATA_DIR": str(self.data),
            "PLIST_DIR": str(self.plists), "ALLOW_NON_MAC": "1", "PORT": "8197",
        })

    def commands(self):
        return [json.loads(line) for line in self.log.read_text().splitlines()]

    def run_macos(self, *, replacement=True):
        # Bash 3.2 lacks this option; Bash 5.2 can test both replacement rules.
        option = "shopt " + ("-s" if replacement else "-u") + " patsub_replacement 2>/dev/null || true"
        # The LaunchAgent PATH is built from absolute directories, so it must be
        # confined to the stubs: a host yt-dlp under /usr/bin would otherwise
        # satisfy the dependency check that MISSING_YT_DLP removes.
        script = (DEPLOY / "macos/install.sh").read_text()
        launch_path = os.pathsep.join((str(self.bin), str(self.home / ".local/bin")))
        script, count = re.subn(r"^LAUNCH_PATH=.*$", "LAUNCH_PATH=" + shlex.quote(launch_path), script, count=1, flags=re.M)
        self.assertEqual(count, 1)
        installer = self.root / "macos-install.sh"
        installer.write_text(script)
        return subprocess.run(
            [BASH, "-c", option + '; source "$1"', "installer-test", str(installer)],
            cwd=self.source, env=self.env, text=True, capture_output=True, timeout=15,
        )

    def run_linux(self):
        script = (DEPLOY / "install.sh").read_text()
        paths = {"INSTALL_DIR": self.install, "DATA_DIR": self.data, "SYSTEMD_DIR": self.root / "systemd"}
        for variable, path in paths.items():
            script, count = re.subn(r"^" + variable + r"=.*$", variable + "=" + shlex.quote(str(path)), script, count=1, flags=re.M)
            self.assertEqual(count, 1)
        guard = 'if [[ $EUID -ne 0 ]]; then\n   error "This script must be run as root (use sudo)"\nfi'
        self.assertIn(guard, script)
        script = script.replace(guard, "# Root check bypassed only in this sandboxed test copy.", 1)
        installer = self.root / "linux-install.sh"
        installer.write_text(script)
        (self.root / "systemd").mkdir()
        (self.source / "deploy").mkdir()
        shutil.copy2(DEPLOY / "video-puller.service", self.source / "deploy/video-puller.service")
        self.env.update({"TEST_LINUX": "1", "TEST_DATA_DIR": str(self.data)})
        result = subprocess.run([BASH, str(installer)], cwd=self.source, env=self.env, text=True, capture_output=True, timeout=15)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)

    def assert_mode(self, path, expected):
        self.assertEqual(stat.S_IMODE(path.stat().st_mode), expected, str(path))

    def test_macos_escapes_all_plist_paths_with_modern_bash(self):
        result = self.run_macos()
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        with self.plist.open("rb") as source:
            plist = plistlib.load(source)
        self.assertEqual(plist["WorkingDirectory"], str(self.install))
        self.assertEqual(plist["ProgramArguments"], [str(self.install / "build/erlang-shipment/entrypoint.sh"), "run"])
        self.assertEqual(plist["EnvironmentVariables"]["DATA_DIR"], str(self.data))
        self.assertEqual(plist["EnvironmentVariables"]["DB_PATH"], str(self.data / "video_eater.db"))
        self.assertEqual(plist["StandardOutPath"], str(self.data / "logs/video-puller.log"))
        self.assertIn(["brew", "install", "yt-dlp"], self.commands())
        self.assertIn(["pipx", "install", "ytdl-sub"], self.commands())

    def test_macos_escape_also_works_without_replacement_expansion(self):
        result = self.run_macos(replacement=False)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        with self.plist.open("rb") as source:
            self.assertEqual(plistlib.load(source)["WorkingDirectory"], str(self.install))

    def test_macos_prebuilt_upgrade_uses_existing_release_without_git_or_build(self):
        (self.install / "build/erlang-shipment").mkdir(parents=True)
        (self.install / "build/erlang-shipment/stale.beam").write_text("old release")
        (self.install / "data").mkdir()
        (self.install / "data/keep.txt").write_text("preserved")
        result = self.run_macos()
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        commands = self.commands()
        self.assertTrue(any(command[0] == "rsync" for command in commands))
        self.assertFalse(any(command[0] in ("git", "gleam") for command in commands))
        self.assertEqual((self.install / "data/keep.txt").read_text(), "preserved")
        self.assertEqual((self.install / "build/erlang-shipment/entrypoint.sh").read_text(), "new release\n")

    def test_macos_missing_homebrew_reports_required_dependency(self):
        (self.bin / "brew").unlink()
        result = self.run_macos()
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("Homebrew is required: https://brew.sh", result.stdout)
        self.assertFalse(self.plist.exists())

    def test_macos_missing_yt_dlp_does_not_replace_existing_plist(self):
        self.plists.mkdir()
        self.plist.write_text("existing launch agent")
        self.env["MISSING_YT_DLP"] = "1"
        result = self.run_macos()
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("yt-dlp is not available on the LaunchAgent PATH", result.stdout)
        self.assertEqual(self.plist.read_text(), "existing launch agent")
        self.assertFalse(any(command[0] == "rsync" for command in self.commands()))

    def test_macos_invalid_plist_keeps_previous_agent_and_removes_temporary_file(self):
        self.plists.mkdir()
        self.plist.write_text("existing launch agent")
        self.env["FAIL_PLUTIL"] = "1"
        result = self.run_macos()
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(self.plist.read_text(), "existing launch agent")
        self.assertEqual(list(self.plists.iterdir()), [self.plist])

    def test_linux_fresh_database_and_sqlite_sidecars_are_private(self):
        self.run_linux()
        self.assert_mode(self.data, 0o711)
        self.assert_mode(self.data / "library", 0o755)
        self.assert_mode(self.data / ".cache", 0o700)
        self.assert_mode(self.data / "video_eater.db", 0o600)
        old_mask = os.umask(0o022)
        try:
            with sqlite3.connect(self.data / "video_eater.db") as database:
                database.execute("pragma journal_mode=wal")
                database.execute("create table test (value text)")
                for suffix in ("", "-wal", "-shm"):
                    self.assert_mode(self.data / ("video_eater.db" + suffix), 0o600)
        finally:
            os.umask(old_mask)

    def test_linux_upgrade_preserves_database_and_normalizes_library_modes(self):
        library = self.data / "library/Channel/Season 2026"
        library.mkdir(parents=True)
        media = library / "video.mp4"
        media.write_bytes(b"video")
        media.chmod(0o700)
        database = self.data / "video_eater.db"
        database.write_bytes(b"existing database contents")
        database.chmod(0o644)
        self.run_linux()
        self.assertEqual(database.read_bytes(), b"existing database contents")
        self.assert_mode(database, 0o600)
        self.assert_mode(library, 0o755)
        self.assert_mode(media, 0o644)
        commands = self.commands()
        own_install = ["chown", "-R", "video-puller:video-puller", str(self.install)]
        own_data = ["chown", "-R", "video-puller:video-puller", str(self.data)]
        build = next(command for command in commands if command[0] == "sudo")
        self.assertLess(commands.index(own_install), commands.index(build))
        self.assertLess(commands.index(own_data), commands.index(build))

    def test_system_unit_keeps_writable_runtime_paths_inside_data_directory(self):
        unit = (DEPLOY / "video-puller.service").read_text()
        settings = dict(re.findall(r'^Environment="([^=]+)=(.*)"$', unit, flags=re.M))
        writable = re.search(r"^ReadWritePaths=(.*)$", unit, flags=re.M).group(1)
        for key in ("DB_PATH", "DATA_DIR", "HOME", "XDG_CACHE_HOME", "ERL_CRASH_DUMP"):
            self.assertEqual(os.path.commonpath([settings[key], writable]), writable, key)
        self.assertIn("ProtectSystem=strict", unit)
        self.assertIn("ProtectHome=true", unit)
        self.assertIn("UMask=0022", unit)


if __name__ == "__main__":
    unittest.main()
