"""Local deployment checks. Credentials stay in memory and out of process arguments."""
import json
import os
from pathlib import Path
import plistlib
import subprocess
import time
import urllib.request
import xml.etree.ElementTree as ET


def command(args, *, cwd=None, log=None, timeout=300):
    result = subprocess.run(args, cwd=cwd, stdout=subprocess.PIPE,
                            stderr=subprocess.STDOUT, text=True, timeout=timeout)
    if log:
        with Path(log).open("a") as stream:
            stream.write(result.stdout)
    if result.returncode:
        raise RuntimeError("Command failed: " + Path(args[0]).name)
    return result.stdout.strip()


def read_installation(agent):
    agent = Path(agent).expanduser().resolve()
    if agent.stat().st_uid != os.getuid():
        raise RuntimeError("LaunchAgent must belong to the current user")
    config = plistlib.loads(agent.read_bytes())
    app = Path(config["WorkingDirectory"]).resolve()
    expected = str(app / "build/erlang-shipment/entrypoint.sh")
    if config["Label"] != "local.video-puller" or config["ProgramArguments"] != [expected, "run"]:
        raise RuntimeError("Unexpected LaunchAgent executable; installation unchanged")
    if not config.get("KeepAlive"):
        raise RuntimeError("KeepAlive must remain enabled")
    return {"agent": agent, "app": app, "env": config["EnvironmentVariables"],
            "target": "gui/" + str(os.getuid()) + "/" + config["Label"]}


def verify_storage(env):
    data = Path(env["DATA_DIR"])
    if not data.is_absolute() or not data.is_dir():
        raise RuntimeError("Configured data volume is unavailable")
    if env.get("STORAGE_VOLUME"):
        raw = subprocess.check_output(["diskutil", "info", "-plist", env["STORAGE_VOLUME"]])
        info = plistlib.loads(raw)
        if info.get("MountPoint") != env["STORAGE_VOLUME"] or info.get("VolumeUUID") != env.get("STORAGE_VOLUME_UUID"):
            raise RuntimeError("External volume identity changed; installation unchanged")
    import shutil
    if shutil.disk_usage(data).free < max(int(env.get("MIN_FREE_GB", "50")), 1) * 1024**3:
        raise RuntimeError("External volume is below its free-space reserve")


def wait_health(env, timeout=45):
    deadline = time.monotonic() + timeout
    url = "http://127.0.0.1:" + env.get("PORT", "8080") + "/health"
    while time.monotonic() < deadline:
        try:
            with urllib.request.urlopen(url, timeout=2) as response:
                value = json.load(response)
            if value.get("status") == "ok" and value.get("database") == "connected" and value.get("scheduler") == "responsive":
                return value
        except (OSError, ValueError):
            pass
        time.sleep(.5)
    raise RuntimeError("Deployed database/scheduler health check failed")


def verify_plex(env):
    if not env.get("PLEX_URL") or not env.get("PLEX_SECTION_ID"):
        return {"configured": False}
    url = env["PLEX_URL"].rstrip("/")
    token = env.get("PLEX_TOKEN")
    if not token:
        pref = Path.home() / "Library/Preferences/com.plexapp.plexmediaserver.plist"
        token = plistlib.loads(pref.read_bytes()).get("PlexOnlineToken")
    if not token:
        raise RuntimeError("Plex verification requires the existing local Plex session")

    def fetch(path, headers=None):
        return urllib.request.urlopen(urllib.request.Request(url + path,
            headers={"X-Plex-Token": token, **(headers or {})}), timeout=15)

    with fetch("/identity") as response:
        identity = ET.fromstring(response.read())
    with fetch("/library/sections/" + env["PLEX_SECTION_ID"] + "/all?type=4",
               {"X-Plex-Container-Start": "0", "X-Plex-Container-Size": "1"}) as response:
        listing = ET.fromstring(response.read())
    part = listing.find(".//Media/Part")
    if part is None:
        return {"configured": True, "reachable": True, "stream_test": "empty_library"}
    file = Path(part.attrib["file"])
    library = (Path(env["DATA_DIR"]) / "library").resolve()
    if not file.resolve().is_relative_to(library):
        raise RuntimeError("Plex sample is outside the managed library")
    with fetch(part.attrib["key"], {"Range": "bytes=0-65535"}) as response:
        payload = response.read(65536)
        status = response.status
    with file.open("rb") as source:
        matches = payload == source.read(65536)
    if status != 206 or not payload or not matches:
        raise RuntimeError("Plex streaming verification failed")
    return {"configured": True, "reachable": True, "server_version": identity.attrib.get("version"),
            "range_http_status": status, "range_bytes": len(payload), "matches_media": True}


def require_ci(source, revision):
    runs = json.loads(command(["gh", "api", "repos/lprior-repo/video-puller/actions/runs?head_sha=" + revision + "&per_page=30"], cwd=source))
    eligible = [run for run in runs["workflow_runs"]
                if run["head_sha"] == revision and run["head_branch"] == "main"
                and run["event"] == "push" and run["path"] == ".github/workflows/test.yml"]
    latest = max(eligible, key=lambda run: run["id"], default=None)
    if not latest or latest["status"] != "completed" or latest["conclusion"] != "success":
        raise RuntimeError("The exact main commit has not passed GitHub CI")
    return latest["html_url"]
