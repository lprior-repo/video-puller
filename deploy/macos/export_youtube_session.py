#!/usr/bin/env python3
"""Export an explicitly authorized local browser session for YouTube only.

Run using the Python interpreter in the ytdl-sub environment. No cookie values
are printed. The service invokes this only after an explicit local opt-in via
YOUTUBE_SESSION_BROWSER, YOUTUBE_SESSION_PYTHON and YOUTUBE_COOKIE_FILE.
"""
import argparse
from http.cookiejar import MozillaCookieJar
import json
import os
from pathlib import Path
import tempfile


class Quiet:
    def debug(self, *args, **kwargs): pass
    def info(self, *args, **kwargs): pass
    def warning(self, *args, **kwargs): pass
    def error(self, *args, **kwargs): pass


def export_session(destination, browser="chrome", extractor=None):
    if extractor is None:
        from yt_dlp.cookies import extract_cookies_from_browser
        extractor = extract_cookies_from_browser
    source = extractor(browser, logger=Quiet())
    cookies = [cookie for cookie in source
               if cookie.domain.lstrip(".").lower() == "youtube.com"
               or cookie.domain.lstrip(".").lower().endswith(".youtube.com")]
    if not cookies:
        raise RuntimeError("No YouTube session cookies available")
    destination = Path(destination)
    destination.parent.mkdir(mode=0o700, parents=True, exist_ok=True)
    descriptor, temporary = tempfile.mkstemp(prefix=".youtube-session-", dir=destination.parent)
    os.close(descriptor)
    try:
        target = MozillaCookieJar(temporary)
        for cookie in cookies:
            target.set_cookie(cookie)
        target.save(ignore_discard=True, ignore_expires=True)
        os.chmod(temporary, 0o600)
        os.replace(temporary, destination)
    finally:
        Path(temporary).unlink(missing_ok=True)
    return {"saved": True, "youtube_only": True, "cookie_count": len(cookies), "mode": "0600"}


def main():
    default = Path.home() / "Library/Application Support/video-puller/auth/youtube.cookies.txt"
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path, default=os.environ.get("YOUTUBE_COOKIE_FILE") or default)
    parser.add_argument("--browser", choices=["chrome", "firefox", "safari"], default="chrome")
    args = parser.parse_args()
    try:
        print(json.dumps(export_session(args.output, args.browser)))
        return 0
    except Exception:
        print(json.dumps({"saved": False, "message": "Browser cookie access unavailable; check the local browser and macOS permissions."}))
        return 1


if __name__ == "__main__":
    raise SystemExit(main())
