# video_puller

A Gleam project scaffold with comprehensive tooling for modern development workflows.

[![Package Version](https://img.shields.io/hexpm/v/video_puller)](https://hex.pm/packages/video_puller)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/video_puller/)

## Features

- 🎯 Idiomatic Gleam project structure
- 🔧 Task runner with `just`
- 🤖 Claude Code integration with custom slash commands
- 📬 MCP (Model Context Protocol) server support
- ✅ CI/CD ready with GitHub Actions
- 📝 Comprehensive documentation

## Setup and Run

### Prerequisites

Runtime (needed by both the dev run and a deployed instance):

- Erlang/OTP >= 25 (tested on OTP 28)
- [`ytdl-sub`](https://github.com/jmbannon/ytdl-sub) — the subscription pull
  engine (`pipx install ytdl-sub`)
- `yt-dlp` — single-video jobs, on-demand channel checks and channel-identity
  resolution for the subscription list
- `ffmpeg` — mp4 conversion, thumbnails, embedded metadata
- `deno` — the JS runtime yt-dlp uses for YouTube signature/challenge handling

Development adds:

- [Gleam](https://gleam.run/getting-started/installing/) >= 1.19

### Get the code

```sh
git clone https://github.com/lprior-repo/video-puller.git
cd video-puller
just setup          # deps download + build (or: gleam deps download && gleam build)
just ci             # format check + type check + tests
```

### Run it (development)

```sh
DATA_DIR=./data \
DB_PATH=./data/video_eater.db \
PORT=8080 \
POLL_TIMEOUT_MINUTES=360 \
gleam run
```

Then open:

- <http://localhost:8080/> — download queue
- <http://localhost:8080/settings> — format, size and output settings for
  single-video jobs
- <http://localhost:8080/subscriptions> — subscription pulls: enable them, set
  the cadence, import channels, trigger a poll and see what arrived

### Run it (service)

- Linux: `sudo ./deploy/install.sh` — systemd unit, data in
  `/var/lib/video-puller`
- macOS + Plex: `./deploy/macos/install.sh` — LaunchAgent, data in
  `${DATA_DIR:-$HOME/PlexMedia/YouTube}`

Both installers install the runtime dependencies, drop the release in place and
wire the service; `deploy/README.txt` has the long form (dependencies,
permissions, logs, uninstall).

## Development

### Using Just (Recommended)

```sh
just --list         # Show all available commands
just setup          # Initial project setup
just dev            # Run in watch mode
just test           # Run tests
just ci             # Run all CI checks
```

### Using Gleam Directly

```sh
gleam run           # Run the project
gleam test          # Run the tests
gleam format        # Format code
gleam check         # Type check
```

## Project Structure

```
video-puller/
├── .claude/              # Claude Code configuration
│   ├── commands/         # Custom slash commands
│   ├── mcp/             # MCP server configuration
│   └── CLAUDE.md        # Project instructions for Claude
├── src/                 # Source code
├── test/                # Tests
├── justfile            # Task runner configuration
└── gleam.toml          # Project manifest
```

## Claude Code Integration

This project is optimized for use with Claude Code. Available slash commands:

### Development Commands
- `/test` - Run tests and fix failures
- `/build` - Build and fix compilation errors
- `/format` - Format all code
- `/check` - Type check and fix errors
- `/add-test` - Add comprehensive tests
- `/refactor` - Refactor to idiomatic Gleam
- `/deps` - Analyze dependencies
- `/ci` - Run all CI checks

### Setup & Integration Commands
- `/setup-beads` - Research and set up Steve Yegge's beads system
- `/setup-mcp-agent` - Research and register project with MCP agent-mail server
- `/research` - Conduct thorough research on any topic

See `.claude/commands/` for all available commands.

## MCP Server Support

This project supports Model Context Protocol for agent coordination:

- **Agent Mail**: Coordinate between multiple AI agents
- **File Reservations**: Prevent edit conflicts
- **Message Threading**: Organize agent communications
- **Build Slots**: Manage concurrent build operations

See `.claude/docs/mcp-agent-setup.md` for comprehensive setup instructions.

## Beads Integration

This project uses [beads](https://github.com/steveyegge/beads) - a lightweight, distributed issue tracker designed for AI coding agents.

### Quick Start

```bash
# Show ready work (unblocked issues)
just beads-ready

# Create a new issue
just beads-create "Issue title" task 1

# Update issue status
just beads-update bd-a1b2 in_progress

# Show database status
just beads-status

# Sync with git
just beads-sync
```

### Features

- 🎯 **Zero-Setup Distributed Database** - Git-backed, no server required
- 🔗 **Four Dependency Types** - blocks, related, parent-child, discovered-from
- 🆔 **Hash-Based IDs** - Collision-resistant (e.g., `bd-a1b2`)
- ✅ **Ready Work Detection** - Automatically finds unblocked issues
- 📊 **JSON Output** - All commands support `--json` for agents

### Essential Commands

```bash
bd ready                        # Show unblocked issues
bd create "Task" -t feature    # Create new issue
bd update bd-a1b2 --status in_progress  # Claim work
bd close bd-a1b2 --reason "Done"       # Complete issue
bd sync                        # Force git sync
```

See `.claude/docs/beads-setup.md` for comprehensive documentation and AGENTS.md for workflow guidelines.

## Testing

```sh
# Run all tests
just test

# Watch mode
just test-watch
```

## Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Run `just ci` to ensure all checks pass
5. Submit a pull request

## Code Style

This project follows idiomatic Gleam conventions:

- Use pattern matching over conditionals
- Prefer `Result` and `Option` types
- Keep functions pure when possible
- Write comprehensive tests
- Document public APIs

See `.claude/CLAUDE.md` for detailed guidelines.

## Documentation

Generate and view documentation:

```sh
just docs
```

Further documentation can be found at <https://hexdocs.pm/video_puller>.

## License

This project is licensed under the Apache License 2.0.

## Environment Variables

The application can be configured using the following environment variables:

### Core Application Settings

| Variable | Description | Default | Required |
|----------|-------------|---------|----------|
| `DB_PATH` | Path to the SQLite database file | `./data/video_eater.db` | No |
| `PORT` | HTTP server port | `8080` | No |
| `SECRET_KEY` | Secret key for Wisp sessions (auto-generated if not set) | (auto-generated) | No |
| `STATIC_DIR` | Directory for static web assets | `./priv/static` | No |
| `OUTPUT_DIR` | Directory for downloaded videos | `./downloads` | No |

### Download Configuration

| Variable | Description | Default | Required |
|----------|-------------|---------|----------|
| `YTDLP_FORMAT` | yt-dlp format string for video quality | `bestvideo[ext=mp4]+bestaudio[ext=m4a]/best[ext=mp4]/best` | No |
| `MAX_FILESIZE` | Maximum file size limit (e.g., "2G", "500M") | `2G` | No |
| `AUDIO_ONLY` | Download audio only (true/false/1/0/yes/no) | `false` | No |
| `AUDIO_FORMAT` | Audio format for audio-only downloads (mp3/aac/opus/best) | `best` | No |
| `ALLOW_PLAYLIST` | Allow playlist downloads (true/false/1/0/yes/no) | `false` | No |

### Performance Settings

| Variable | Description | Default | Required |
|----------|-------------|---------|----------|
| `POLL_INTERVAL_MS` | Polling interval for download queue in milliseconds | `5000` | No |
| `MAX_CONCURRENCY` | Maximum concurrent downloads | `10` | No |

### Subscription Pulls

Subscriptions pull public channel URLs straight from YouTube with `ytdl-sub`; they
never read browser cookies and run one channel at a time.

| Variable | Description | Default | Required |
|----------|-------------|---------|----------|
| `DATA_DIR` | Root for the engine layout and the download library | `./data` | No |
| `CHANNELS_TEMPLATE` | Channel list copied into place on first start | `./priv/ytdl-sub/channels.txt` | No |
| `POLL_TIMEOUT_MINUTES` | Timeout for a single engine pull | `360` | No |

```bash
# One public channel URL per line; blank lines and # comments are ignored
$EDITOR "${DATA_DIR:-./data}/ytdl-sub/channels.txt"

# Enable auto-download and set the cadence on the Subscriptions page
open http://localhost:8080/subscriptions
```

Channel lists can be filled from a Google Takeout export instead of typing
URLs: download "YouTube and YouTube Music → subscriptions", drop the
`subscriptions.csv` into `${DATA_DIR:-./data}/ytdl-sub/`, and the missing
channels are merged into `channels.txt` on the next start or poll (the CSV is
renamed to `subscriptions.csv.imported`). No account access or cookies are
involved.

Videos land in `${DATA_DIR:-./data}/library/<Channel>/Season <Year>/`. The first
poll pulls the channel's full upload history; ytdl-sub's per-channel download
archive under the library root keeps later polls incremental, so only new
uploads are fetched. Give large channels a matching `POLL_TIMEOUT_MINUTES`,
since the backfill runs inside one poll. Subscription pulls use ytdl-sub's
"Plex TV Show by Date" preset with its resolution assert disabled: the assert
aborts on any download below 361p, which false-positives on genuinely low-res
uploads rather than throttling. Throttle protection's request pacing stays on,
and a poll that exits non-zero still records the files it did download while
reporting the per-subscription errors. The format and size limits on the
Settings page apply to the yt-dlp job path, not to subscription pulls.

repeat polls are idempotent: ytdl-sub's download archive skips episodes already
on disk, and seen-video rows are keyed by video id (`INSERT OR REPLACE`), so
nothing duplicates. To populate the channel list without signing in, drop a
Google Takeout `subscriptions.csv` into `${DATA_DIR}/ytdl-sub/`: missing URLs
are merged into `channels.txt` (as `Channel Title = URL`, so the title becomes
the Plex show name) on the next start or poll, and the CSV is renamed
`.imported`. Alongside the video, the Plex preset writes Plex-style metadata —
`poster.jpg` and `fanart.jpg` at the show root, an episode `-thumb.jpg` beside
each video, and an mp4/h264 file with embedded tags. Plex does not read NFO
sidecars, so none are generated (the Jellyfin/Emby presets add those); the
`.info.json` files beside each episode are the engine's own metadata and are
ignored by Plex.

The same channel can be listed in several URL forms (`@handle`, `@handle/videos`,
`@handle/shorts`, `channel/UC…`, with or without a trailing slash). Channel
identity collapses those to one subscription, so a channel never gets two Plex
shows or two download archives; the channel id is resolved once through
`yt-dlp` and cached in `${DATA_DIR}/ytdl-sub/channel_ids.txt`. When two entries
merge, the label that already has a `library/<label>/` directory wins, which
keeps an existing archive (and Plex show) intact.

An interrupted poll is safe too: the engine writes into
`${DATA_DIR}/ytdl-sub/working` until a file is complete, and `seen_videos`
only gets rows from the engine's "Files created" report — so a timed-out poll
records nothing and the next poll re-checks the channel, skipping episodes
already in the download archive.
`POLL_TIMEOUT_MINUTES` is a total deadline for one engine run; streaming
output cannot extend it.

## Plex

Subscription pulls land directly in a Plex-shaped TV library — one show per
channel, one season per upload year:

```
${DATA_DIR}/library/
└── Fireship/
    ├── poster.jpg                                # show poster
    ├── fanart.jpg                                # show background
    ├── .ytdl-sub-Fireship-download-archive.json  # engine's dedupe archive
    └── Season 2021/
        ├── s2021.e032001 - ７ Linux Things You Say WRONG #Shorts.mp4
        ├── s2021.e032001 - ７ Linux Things You Say WRONG #Shorts-thumb.jpg
        └── s2021.e032001 - ７ Linux Things You Say WRONG #Shorts.info.json
```

Ready-for-Plex checklist:

1. Add `${DATA_DIR}/library` to Plex as a **TV Shows** library. Plex reads the
   `Season <year>` folders, the show-level `poster.jpg`/`fanart.jpg` and each
   episode's `-thumb.jpg`. There are no NFO files (Plex ignores them) and the
   `.info.json` sidecars are ytdl-sub's own metadata.
2. Every episode is an mp4/h264 file with embedded tags (`title`, `date`,
   `genre`, `synopsis`, `show`), so Plex has metadata even before it fetches
   anything online; chapters are embedded when the source provides them.
3. Plex must be able to read the tree. The bundled Linux installer leaves the
   data root mode 711 (traversable, not listable) with `library/` at 755, so a
   Plex server running as another user can read the shows while the database
   stays private. On macOS the LaunchAgent runs as your user, so nothing extra
   is needed.
4. Plex picks new episodes up on a library scan; the app never talks to Plex.
   Because polls update the library in place — and a poll that times out still
   records the files it downloaded — scanning after a poll is enough.

### macOS (Plex server)

`deploy/macos/install.sh` installs the runtime with Homebrew (Erlang, ffmpeg,
pipx, deno, `yt-dlp`), installs `ytdl-sub`, installs the release into
`${INSTALL_DIR:-$HOME/video-puller}`, and runs it as a LaunchAgent with data in
`${DATA_DIR:-$HOME/PlexMedia/YouTube}`:

```sh
./deploy/macos/install.sh
# then add $DATA_DIR/library as a TV Shows library in Plex
```

### Security note

The web UI binds to localhost and ships without authentication or CSRF
protection of its own: keep it on localhost or a trusted LAN, and put an
authenticating reverse proxy in front of it before exposing it publicly.

### Example Configuration

```bash
# Basic configuration
export DB_PATH="./data/videos.db"
export PORT=3000
export OUTPUT_DIR="/mnt/videos"

# High-performance setup
export MAX_CONCURRENCY=50
export POLL_INTERVAL_MS=2000

# Audio-only podcast downloader
export AUDIO_ONLY=true
export AUDIO_FORMAT=mp3
export MAX_FILESIZE=500M
```

## Production Deployment

### System Requirements

Before deploying to production, ensure the following requirements are met:

- **Gleam**: Version 1.0.0 or higher
- **Erlang/OTP**: Version 24 or higher (included in release build)
- **yt-dlp**: Latest version installed and accessible in PATH
  ```bash
  # Install yt-dlp
  pip install -U yt-dlp
  # OR
  curl -L https://github.com/yt-dlp/yt-dlp/releases/latest/download/yt-dlp -o /usr/local/bin/yt-dlp
  chmod a+rx /usr/local/bin/yt-dlp
  ```
- **ytdl-sub**: Subscription engine, required only for subscription pulls
  ```bash
  pipx install ytdl-sub
  ```
- **SQLite**: Version 3.35.0 or higher (for WAL mode support)
- **Disk Space**: Sufficient storage for downloaded videos (depends on usage)
- **Memory**: Minimum 512MB RAM, recommended 2GB+ for high concurrency

### Production Deployment Checklist

#### 1. Build Release

```bash
# Create optimized release build
just release

# The release will be in build/erlang-shipment/
# This includes the Erlang runtime - no external dependencies needed
```

#### 2. Configuration

- [ ] Set `DB_PATH` to a persistent location outside the application directory
- [ ] Set `OUTPUT_DIR` to a location with sufficient disk space
- [ ] Generate a secure `SECRET_KEY` (64+ character random string)
- [ ] Configure `MAX_CONCURRENCY` based on server capacity
- [ ] Set appropriate `PORT` (default 8080)
- [ ] Verify yt-dlp is installed and accessible in PATH

#### 3. Database Setup

- [ ] Ensure the database directory exists and is writable
- [ ] Configure database backups (see Backup Procedures below)
- [ ] Verify SQLite supports WAL mode: `sqlite3 :memory: "PRAGMA journal_mode=WAL;"`

#### 4. Security Considerations

- [ ] **Run as non-root user**: Create a dedicated user for the application
  ```bash
  sudo useradd -r -s /bin/false video-puller
  ```
- [ ] **File permissions**: Ensure database and output directories are only accessible by the application user
  ```bash
  chown -R video-puller:video-puller /path/to/data /path/to/downloads
  chmod 700 /path/to/data /path/to/downloads
  ```
- [ ] **Firewall**: Restrict access to the application port
  ```bash
  # Example with ufw
  sudo ufw allow from trusted_ip to any port 8080
  ```
- [ ] **Secret key**: Use a cryptographically secure random string for `SECRET_KEY`
  ```bash
  export SECRET_KEY=$(openssl rand -hex 32)
  ```
- [ ] **Input validation**: The application validates all URLs, but ensure network isolation if processing untrusted input
- [ ] **Network access**: Consider using a reverse proxy (nginx/caddy) for TLS termination
- [ ] **Rate limiting**: Implement rate limiting at the reverse proxy level
- [ ] **Environment variables**: Store sensitive configuration in a secure location (not in version control)

#### 5. Process Management

Set up a systemd service for automatic startup and restart:

```bash
# /etc/systemd/system/video-puller.service
[Unit]
Description=FractalVideoEater - BEAM-Optimized Video Download System
After=network.target

[Service]
Type=simple
User=video-puller
Group=video-puller
WorkingDirectory=/opt/video-puller
Environment="DB_PATH=/var/lib/video-puller/video_eater.db"
Environment="OUTPUT_DIR=/var/lib/video-puller/downloads"
Environment="PORT=8080"
Environment="SECRET_KEY=your-secure-secret-key-here"
ExecStart=/opt/video-puller/build/erlang-shipment/entrypoint.sh run
Restart=always
RestartSec=10

[Install]
WantedBy=multi-user.target
```

Enable and start:
```bash
sudo systemctl daemon-reload
sudo systemctl enable video-puller
sudo systemctl start video-puller
sudo systemctl status video-puller
```

#### 6. Monitoring Recommendations

- [ ] **Application logs**: Monitor stdout/stderr via journalctl
  ```bash
  journalctl -u video-puller -f
  ```
- [ ] **Disk space**: Monitor download directory and database size
  ```bash
  df -h /var/lib/video-puller
  du -sh /var/lib/video-puller/*
  ```
- [ ] **Database health**: Monitor WAL file size and checkpoint frequency
  ```bash
  sqlite3 /var/lib/video-puller/video_eater.db "PRAGMA wal_checkpoint(FULL);"
  ```
- [ ] **Process health**: Monitor memory and CPU usage
  ```bash
  ps aux | grep beam.smp
  ```
- [ ] **Download success rate**: Track failed vs. successful downloads via the web UI
- [ ] **Concurrent workers**: Monitor active worker pool size through application logs
- [ ] **HTTP endpoint health**: Setup health check endpoint monitoring
  ```bash
  curl http://localhost:8080/
  ```

#### 7. Backup Procedures

**Database Backup (Recommended)**

```bash
# Hot backup using SQLite backup API (safe during operation)
sqlite3 /var/lib/video-puller/video_eater.db ".backup /backup/video_eater_$(date +%Y%m%d_%H%M%S).db"

# Daily backup cron job
# /etc/cron.daily/video-puller-backup
#!/bin/bash
BACKUP_DIR="/backup/video-puller"
DB_PATH="/var/lib/video-puller/video_eater.db"
DATE=$(date +%Y%m%d_%H%M%S)

mkdir -p "$BACKUP_DIR"
sqlite3 "$DB_PATH" ".backup $BACKUP_DIR/video_eater_$DATE.db"

# Keep only last 30 days of backups
find "$BACKUP_DIR" -name "video_eater_*.db" -mtime +30 -delete

# Verify backup integrity
sqlite3 "$BACKUP_DIR/video_eater_$DATE.db" "PRAGMA integrity_check;"
```

**What to Backup:**
- SQLite database file (`video_eater.db`)
- Database WAL file (`video_eater.db-wal`) if present
- Application configuration files
- (Optional) Downloaded videos if retention is required

**Backup Frequency:**
- Database: Daily minimum, hourly for high-value data
- Configuration: After any changes
- Videos: Depends on retention policy

**Restore Procedure:**
```bash
# Stop the application
sudo systemctl stop video-puller

# Restore database
cp /backup/video_eater_YYYYMMDD_HHMMSS.db /var/lib/video-puller/video_eater.db

# Fix permissions
chown video-puller:video-puller /var/lib/video-puller/video_eater.db

# Start the application
sudo systemctl start video-puller
```

#### 8. Performance Tuning

- [ ] Adjust `MAX_CONCURRENCY` based on available bandwidth and system resources
- [ ] Monitor BEAM VM metrics for memory and scheduler usage
- [ ] Configure WAL checkpoint intervals for database performance
- [ ] Consider SSD storage for the database for better I/O performance
- [ ] Use `POLL_INTERVAL_MS` to balance responsiveness vs. CPU usage

#### 9. Operational Verification

After deployment, verify:
- [ ] Web UI is accessible at configured port
- [ ] Test video download succeeds
- [ ] Database writes are working (check WAL file)
- [ ] Logs show no errors
- [ ] Worker pool is operating (check for "Manager started" message)
- [ ] Subscription feature works if enabled

### Quick Production Start

```bash
# 1. Build release
just release

# 2. Set environment variables
export DB_PATH="/var/lib/video-puller/video_eater.db"
export OUTPUT_DIR="/var/lib/video-puller/downloads"
export PORT=8080
export SECRET_KEY=$(openssl rand -hex 32)
export MAX_CONCURRENCY=20

# 3. Create directories
mkdir -p /var/lib/video-puller/downloads

# 4. Run
./build/erlang-shipment/entrypoint.sh run
```

## Resources

- [Gleam Language](https://gleam.run/)
- [Gleam Standard Library](https://hexdocs.pm/gleam_stdlib/)
- [Claude Code](https://claude.ai/claude-code)
- [Just Task Runner](https://just.systems/)
