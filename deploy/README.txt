FractalVideoEater - Systemd Deployment
========================================

This directory contains files for deploying video-puller as a systemd service.

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

Runtime Dependencies:
---------------------
The following must be installed on the target system:
- Erlang/OTP >= 25.0 (tested with OTP 26.x)
  - The release uses erlang-shipment which bundles the BEAM files
  - Only the Erlang runtime is needed, not the full development environment
- yt-dlp (for video downloading functionality)
  - Install via: pip install yt-dlp
  - Or package manager: apt install yt-dlp / pacman -S yt-dlp
- FFmpeg (optional, recommended for post-processing)
  - Install via package manager: apt install ffmpeg / pacman -S ffmpeg

Installation Details:
---------------------
The installation script will:
- Create a system user 'video-puller'
- Install the application to /opt/video-puller
- Create data directory at /var/lib/video-puller
- Install and enable the systemd service
- Build the optimized release (erlang-shipment)

Manual Installation:
--------------------
If you prefer to install manually:

1. Create system user:
   sudo useradd --system --no-create-home --shell /usr/sbin/nologin video-puller

2. Create directories:
   sudo mkdir -p /opt/video-puller
   sudo mkdir -p /var/lib/video-puller

3. Copy project files:
   sudo cp -r . /opt/video-puller/
   sudo chown -R video-puller:video-puller /opt/video-puller
   sudo chown -R video-puller:video-puller /var/lib/video-puller

4. Build the project release:
   cd /opt/video-puller
   sudo -u video-puller gleam export erlang-shipment

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
- STATIC_DIR (default: /opt/video-puller/priv/static)
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

3. Permission errors:
   - Verify ownership: ls -la /var/lib/video-puller
   - Fix ownership: sudo chown -R video-puller:video-puller /var/lib/video-puller
