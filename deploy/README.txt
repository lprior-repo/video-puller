FractalVideoEater - Systemd Deployment
========================================

This directory contains files for deploying video-puller as a systemd service
on a Linux machine (typically the same box as your Plex server).

Files:
------
- video-puller.service  : Systemd unit file
- install.sh            : Automated installation script
- uninstall.sh          : Automated uninstallation script

Quick Start:
------------
1. Run the installation script as root:
   sudo ./deploy/install.sh

2. Start the service:
   sudo systemctl start video-puller

3. Check status:
   sudo systemctl status video-puller

4. View logs:
   sudo journalctl -u video-puller -f

User-Level Install (no root, single user):
------------------------------------------
On a workstation the same release runs from a systemd user unit, which needs
no root and can use a user-installed toolchain (mise shims provide `erl` and
`ytdl-sub` on PATH):

1. Build the release in the checkout:
     gleam export erlang-shipment

2. Install the unit, adjusting the paths for your checkout:
     mkdir -p ~/.config/systemd/user
     cat > ~/.config/systemd/user/video-puller.service <<'EOF'
     [Unit]
     Description=video-puller (YouTube subscription downloads, no cookies)
     After=network-online.target
     Wants=network-online.target

     [Service]
     Type=simple
     WorkingDirectory=%h/src/video-puller
     Environment=PATH=%h/.local/share/mise/shims:/usr/local/bin:/usr/bin:/bin
     Environment=DATA_DIR=%h/.local/share/video-puller
     Environment=DB_PATH=%h/.local/share/video-puller/video_eater.db
     Environment=STATIC_DIR=%h/src/video-puller/priv/static
     Environment=CHANNELS_TEMPLATE=%h/src/video-puller/priv/ytdl-sub/channels.txt
     Environment=POLL_TIMEOUT_MINUTES=240
     Environment=PORT=8080
     ExecStart=%h/src/video-puller/build/erlang-shipment/entrypoint.sh run
     Restart=on-failure
     RestartSec=15

     [Install]
     WantedBy=default.target
     EOF

3. Enable it, and let it run without an active login:
     systemctl --user daemon-reload
     systemctl --user enable --now video-puller
     loginctl enable-linger "$USER"

4. Status and logs:
     systemctl --user status video-puller
     journalctl --user -u video-puller -f

The data directory is ~/.local/share/video-puller, with the same layout as
below (library/ in place of /var/lib/video-puller/library). A home directory
of mode 700 is not readable by Plex under another account: point DATA_DIR at a
shared path, or use the system install, when Plex must read the library.

Runtime Dependencies:
---------------------
Install these before starting; the unit runs with PATH=/usr/local/bin:/usr/bin
:/bin and only the data directory writable.

- Erlang/OTP >= 25.0 (tested with OTP 26+/28.x)
  - The release uses erlang-shipment which bundles the BEAM files
  - Only the Erlang runtime is needed, not the full development environment
- ytdl-sub (subscription pull engine). Install it system-wide, e.g.
      sudo PIPX_HOME=/opt/pipx PIPX_BIN_DIR=/usr/local/bin pipx install ytdl-sub
  or via your package manager, and keep the binary on the unit's PATH.
- yt-dlp (single-video jobs, channel-identity resolution)
      sudo PIPX_HOME=/opt/pipx PIPX_BIN_DIR=/usr/local/bin pipx install yt-dlp
  or via your package manager (apt install yt-dlp / pacman -S yt-dlp).
- FFmpeg (mp4 conversion, thumbnails, embedded metadata)
      apt install ffmpeg / pacman -S ffmpeg
- deno (JS runtime yt-dlp uses for YouTube challenge handling)
      apt install deno / pacman -S deno

Data and library layout:
------------------------
The unit sets DATA_DIR=/var/lib/video-puller, so everything the app owns lives
there:

- /var/lib/video-puller/.cache/
      private tool caches (Gleam at build time; yt-dlp / Deno at runtime)
- /var/lib/video-puller/.cache/erl_crash.dump
      Erlang crash diagnostics, when generated
- /var/lib/video-puller/video_eater.db
      application database (jobs, settings, subscription state, seen videos)
- /var/lib/video-puller/ytdl-sub/
      engine layout: config.yaml, channels.txt, subscriptions.yaml,
      channel_ids.txt (resolved channel identities), working/ (download cache)
- /var/lib/video-puller/downloads/
      manual URL-box jobs (a relative output directory is resolved against
      DATA_DIR)
- /var/lib/video-puller/library/<Channel>/Season <Year>/
      the Plex library (see below)

DATA_DIR is resolved to an absolute path at startup, so a relative value cannot
produce relative library paths in the engine's output.

Subscription pulls (no cookies):
-------------------------------
Subscriptions use public channel URLs only - no browser profile, no cookies, no
account access.

1. Open http://<host>:8080/subscriptions
2. Add channels to /var/lib/video-puller/ytdl-sub/channels.txt, one per line:
       https://www.youtube.com/@SomeChannel
       Nice Display Name = https://www.youtube.com/channel/UCxxxxxxxx
   A Google Takeout export works too: drop the `subscriptions.csv` into
   /var/lib/video-puller/ytdl-sub/ and restart; the channel titles are carried
   over as display names and the CSV is renamed `.imported`.
3. Enable subscription pulls on the Subscriptions page and pick a cadence, then
   use "Refresh Now" (POST /subscriptions/poll) for an immediate poll.
4. Each poll runs the engine twice: a recent pass that checks every channel's
   newest uploads (RECENT_VIDEOS, default 5; capped by RECENT_PHASE_MINUTES,
   default 30), then a backfill pass that works through the full upload
   history. New uploads therefore arrive even while a long first backfill is
   still running. Set POLL_TIMEOUT_MINUTES high enough for the backfill; a pass
   that hits its deadline is killed, whatever already downloaded is kept and
   recorded, and the backfill simply continues on the next poll. The log shows
   both passes with their file/error counts:
       Poll pass 1/2 - recent uploads: up to 30 min
       Poll pass 2/2 - backfill: up to 330 min

Plex:
-----
Add /var/lib/video-puller/library as a **TV Shows** library in Plex. One
channel becomes one show:

    library/
    └── Fireship/
        ├── poster.jpg                                 (show poster)
        ├── fanart.jpg                                 (show background)
        ├── .ytdl-sub-Fireship-download-archive.json   (engine dedupe archive)
        └── Season 2021/
            ├── s2021.e032001 - Title.mp4              (mp4/h264 + embedded tags)
            ├── s2021.e032001 - Title-thumb.jpg        (episode thumbnail)
            └── s2021.e032001 - Title.info.json        (engine metadata)

Plex reads the `Season <year>` folders, poster.jpg/fanart.jpg and the episode
-thumb.jpg files. No NFO files are written (Plex ignores them); .info.json
sidecars are ytdl-sub's own metadata.

Permissions: install.sh leaves the data root mode 711 (traversable, not
listable), library directories at 755 and media files at 644. It creates a
fresh database at 600 before the service starts and hardens existing
video_eater.db* files to 600; SQLite also uses those private permissions for
its WAL and shared-memory sidecars. Tool caches live in a private .cache/
directory (700) under the data root, since the service has no login home and
ProtectHome hides /home. The unit explicitly uses umask 022 so new Plex
content is readable without a shared login.

Plex picks new episodes up on a library scan; the app does not call Plex. Polls
update the library in place, so rescanning (or Plex's periodic scan) after a
poll is enough.

Installation Details:
---------------------
The installation script will:
- Create a system user 'video-puller'
- Install the application to /opt/video-puller
- Create data directory at /var/lib/video-puller (library + engine layout)
- Install and enable the systemd service
- Build the optimized release (erlang-shipment) as the service user

Manual Installation:
--------------------
If you prefer to install manually:

1. Create system user:
   sudo useradd --system --no-create-home --shell /usr/sbin/nologin video-puller

2. Create directories:
   sudo mkdir -p /opt/video-puller /var/lib/video-puller/library /var/lib/video-puller/.cache
   sudo sh -c 'umask 077; touch /var/lib/video-puller/video_eater.db'

3. Copy project files, then set ownership (copy first so build artifacts are
   not left root-owned):
   sudo cp -r . /opt/video-puller/
   sudo chown -R video-puller:video-puller /opt/video-puller
   sudo chown -R video-puller:video-puller /var/lib/video-puller
   sudo chmod 711 /var/lib/video-puller
   sudo chmod 700 /var/lib/video-puller/.cache
   sudo find /var/lib/video-puller -maxdepth 1 -name 'video_eater.db*' -exec chmod 600 {} +
   sudo find /var/lib/video-puller/library -type d -exec chmod 755 {} +
   sudo find /var/lib/video-puller/library -type f -exec chmod 644 {} +

4. Build the project release:
   cd /opt/video-puller
   sudo -u video-puller env HOME=/var/lib/video-puller \
     XDG_CACHE_HOME=/var/lib/video-puller/.cache gleam export erlang-shipment

5. Install systemd service:
   sudo cp deploy/video-puller.service /etc/systemd/system/
   sudo systemctl daemon-reload
   sudo systemctl enable video-puller
   sudo systemctl start video-puller

Configuration:
--------------
Edit the service file to customize:
- PORT (default: 8080)
- DB_PATH (default: /var/lib/video-puller/video_eater.db)
- DATA_DIR (default: /var/lib/video-puller; engine layout + library root)
  If changing it, also update HOME, XDG_CACHE_HOME, ERL_CRASH_DUMP and
  ReadWritePaths to stay within the new writable tree.
- STATIC_DIR (default: /opt/video-puller/priv/static)
- POLL_TIMEOUT_MINUTES (default: 360; total deadline for one poll, both passes)
- RECENT_VIDEOS (default: 5; newest uploads the recent pass checks per channel)
- RECENT_PHASE_MINUTES (default: 30; budget cap for the recent pass, never more
  than a quarter of the poll)
- SECRET_KEY (optional, auto-generated if not set)

Service file location: /etc/systemd/system/video-puller.service

After editing, reload and restart:
  sudo systemctl daemon-reload
  sudo systemctl restart video-puller

Logs:
-----
View all logs:
  sudo journalctl -u video-puller

Follow logs in real-time:
  sudo journalctl -u video-puller -f

View recent logs:
  sudo journalctl -u video-puller -n 100

Service Management:
-------------------
Start:    sudo systemctl start video-puller
Stop:     sudo systemctl stop video-puller
Restart:  sudo systemctl restart video-puller
Status:   sudo systemctl status video-puller
Enable:   sudo systemctl enable video-puller
Disable:  sudo systemctl disable video-puller

Uninstall:
----------
Run the uninstall script:
  sudo ./deploy/uninstall.sh

This will remove the service and optionally remove data and installation directories.

Security Notes:
---------------
The service is configured with security hardening:
- Runs as non-root user (video-puller)
- NoNewPrivileges enabled
- PrivateTmp enabled
- ProtectSystem=strict (read-only system directories)
- ProtectHome enabled (no access to home directories)
- Only /var/lib/video-puller is writable
- ytdl-sub must therefore be installed system-wide (not in a user's home)

The web UI itself has no authentication or CSRF protection: bind it to
localhost/LAN and put an authenticating reverse proxy in front of it before
exposing it publicly.

Troubleshooting:
----------------
1. Service won't start:
   - Check logs: sudo journalctl -u video-puller -n 50
   - Verify Gleam is installed: which gleam
   - Check permissions on /var/lib/video-puller
   - Verify database can be created/accessed

2. Port already in use:
   - Check what's using port 8080: sudo lsof -i :8080
   - Change PORT in service file

3. Subscriptions never download anything:
   - Check the log for "ytdl-sub layout setup failed" or a channels error
   - Confirm the engine is on the unit's PATH:
       sudo -u video-puller env PATH=/usr/local/bin:/usr/bin:/bin ytdl-sub --version
   - Confirm DATA_DIR is writable by the service user
   - Confirm channels.txt has at least one channel URL
   - Raise POLL_TIMEOUT_MINUTES if the log shows "Command timeout exceeded"
     during a first backfill
   - The log names each pass ("Poll pass 1/2 - recent uploads", "Poll pass 2/2
     - backfill") with the files and errors it produced, so a poll that only
     covered part of the list says so

4. Plex shows nothing:
   - Confirm Plex can read the tree: sudo -u <plexuser> ls /var/lib/video-puller/library
   - Check the library dir modes (711 on the data root, 755 on library)
   - Trigger a library scan after a poll; the app never refreshes Plex itself

Installer Regression Checks:
----------------------------
From the project root, run:
  bash -n deploy/install.sh deploy/macos/install.sh deploy/uninstall.sh
  python3 -m unittest discover -s deploy/tests -v

These tests use temporary directories and stub package-manager, build,
privileged and service commands. They cover generated macOS plist escaping
(including Bash 5.2), dependency checks, prebuilt upgrades, preserving an
existing plist after validation failure, Linux copy/ownership ordering,
fresh and existing database permissions, and runtime writable paths. They
never install software, contact download sites, or start services; native
macOS launchd and actual systemd startup still require platform validation.
