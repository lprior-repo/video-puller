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

### Automated Plex subscriptions

The Gleam subscription worker runs `ytdl-sub` directly in a bounded BEAM pool.
The default `SUBSCRIPTION_WORKERS=1` downloads one channel at a time. Set
`SUBSCRIPTION_DAYS=30` to limit uploads to the last 30 days. Change these values on the Subscriptions
page; they persist under `DATA_DIR/ytdl-sub/runtime.json` and apply to the next
poll. Set `POLL_ON_START=true` to resume polling immediately after service
startup. Each channel has its own working directory, while its Plex archive stays
inside the existing show directory, so retries do not download it twice.

“Sync channel list” treats pasted YouTube subscriptions as the complete desired
set. Gleam saves it atomically in `ytdl-sub/desired-channels.txt`, stops the
current poll, and reconciles immediately and again before every poll and at
startup. Missing channels are added; removed channels stop downloading and
leave the feed. Their managed media and archives move to `retired-library/`
on the same data disk, outside Plex's library. Readding a channel restores its
files and archive. Empty imports and conflicting destinations are rejected.
“Add channel” appends to this desired set without removing existing channels.
The coordinator checks persisted desired state every 30 seconds. A separate
Gleam loop restores original Plex titles every 30 seconds after asynchronous
Plex scans finish. It detects completed files missing from Plex and retries
scanning after Plex becomes available, logging failures and recovery without
printing credentials. All scan requests share a monotonic five-minute cooldown,
including startup, completed-download callbacks and failed requests, so a slow
Plex scanner cannot trigger a new full scan every 30 seconds. A compact metadata cache avoids repeatedly parsing
large sidecars; it invalidates when the source size or modification time changes.

At startup and before each scheduled poll, rolling retention deletes channel
episodes older than `SUBSCRIPTION_DAYS` and their episode sidecars, then removes
expired feed records and refreshes Plex. It keeps download archives, artwork,
unknown-date files, and explicit single-video requests. Zero days disables
retention. The cutoff is midnight UTC minus the selected number of days.

Polling uses Erlang timers with a cadence measured from poll start; work is
capped below the interval so a backlog cannot postpone the next cycle.
Recoverable errors, including partial download failures and worker crashes, retry
automatically after five minutes, doubling up to one hour. A clean poll resets
the delay. The retry deadline and last successful poll survive BEAM restarts;
startup honors the deadline. Completed files and download archives survive
retries. Permanently unavailable individual videos are reported and skipped
without accelerating the whole schedule. `/health` exposes `downloads` with
state, consecutive failures, next retry and last success, plus `youtube_session`
and `auto_download_enabled`, independently of process health.

Global download history lives in `ytdl-sub/download-history/archive.txt`,
separate from disposable caches. It records completed YouTube IDs durably and
imports existing per-show archives, including retired shows. Each downloader
receives a private snapshot because ytdl-sub restores its supplied raw archive
after a URL completes. Explicit videos, playlists, and unresolved channel
identities run serially before the independent channel pool, preventing overlap
races. Manual YouTube requests join the same coordinator; equivalent watch,
short, live, embed and youtu.be URLs share one video identity. A malformed
manifest or unreadable history stops work instead of bypassing deduplication.

At startup and before each six-hour poll, Plex episodes with a positive
`viewCount` are removed from the managed library with their episode sidecars.
Completion history is synced before unlinking, so cleanup cannot cause a later
redownload. Partially watched videos and active/paused playback sessions remain. Missing or unreachable Plex causes
watched cleanup to skip and retry later. Age retention also preserves explicit
requests found in ordinary channel folders. Cleanup can run below the free-space
reserve on the UUID-verified external drive; downloads remain blocked until
the reserve is recovered.

If ordinary age/watched cleanup cannot recover space, low-space cleanup removes
the oldest completed, date-known regular-channel YouTube episodes first. It
starts below `MIN_FREE_GB` and aims for an additional 5 GiB of headroom. Explicit
video requests, unknown dates, unrelated movies, symlinks, active/paused Plex
sessions, and partially watched episodes are protected. Plex state and drive
identity are rechecked before each removal; unavailable or malformed Plex state
blocks this cleanup. Completion IDs are synced before media deletion and remain
in the download archive. If protected files or other applications still consume
the reserve, downloads remain stopped. At restart this cleanup runs before
SQLite is opened, so a full disk need not prevent space recovery.

SQLite starts in WAL mode with `synchronous=FULL`, a five-second busy timeout,
and a required successful `quick_check`. Corrupt or unreadable databases stop
startup; the application never silently replaces them with an empty database.
An optional supervised Gleam worker takes a consistent SQLite `VACUUM INTO`
snapshot and saves the durable download archive and subscription files to
`STATE_BACKUP_DIR` daily, retrying failures hourly. It keeps three completed
daily snapshots and refuses to prune unfamiliar files. The macOS installer
defaults this directory to `~/Library/Application Support/video-puller/state-backups`
on the internal drive. These snapshots back up application state, not video
media; recovery is deliberate rather than silently overwriting a bad database.

The separate native macOS hang watchdog's portable Gleam/Erlang source and
tests are in `deploy/macos/watchdog`. It validates process ownership and job
configuration, observes bounded grace/retry/cooldown periods, and lets launchd
restart a frozen or repeatedly unhealthy VM. Its state and credentials are not
part of the repository.

“Skip members-only / premium videos” defaults to enabled and persists with
the other subscription settings. Its non-breaking availability filter excludes
premium-only and subscriber-only entries while permitting public, unlisted,
and unknown availability; extraction-time restrictions may still be reported.
The subscription grid uses original thumbnails, titles, channel/date/duration,
saved descriptions and view counts, with direct links to each indexed Plex
episode. View counts reflect the saved metadata, not a live YouTube counter.
The page loads the rich video grid separately from its four-second status and
progress refresh. Grid requests never overlap and refresh after new completions
or once per minute to pick up later Plex links and metadata updates.

The application runs under an OTP `RestForOne` supervisor, ordered as storage
guard, poll cleanup guardian, download manager and pool, named subscription
actor, Mist HTTP supervisor, Plex synchronizer, and state backup worker.
Storage, Plex sync, and backup work processes are direct supervisor children.
Manual download workers belong to a factory supervisor under the pool, with
temporary child specifications and pool-owned monitors. A worker crash fails
its assigned job once and replaces that worker without restarting the HTTP
server or subscription scheduler. Normal download errors reuse healthy workers;
timeout replacement waits for worker death. The default manual pool honors
`MAX_CONCURRENCY` (10), instead of silently allocating up to 500 workers.
Pool deadlines and rate limiting use monotonic time and native BEAM timers.
An upstream child failure restarts its
dependent services; the web context resolves the replacement subscription
actor through its stable name. Poll-worker monitors detect crashes immediately
and schedule an automatic retry with persisted backoff. The download manager has a bounded mailbox
health check, and `/health` returns 503 if the subscription actor is unresponsive.
Restart intensity is bounded to ten restarts per minute; launchd restarts the
VM if the supervisor gives up. Startup reaps only fingerprint-verified orphan
download processes before starting new work. The macOS LaunchAgent starts at
login; FileVault still requires unlocking the Mac after a cold boot.

The poll cleanup guardian is supervised and retains a bounded ownership journal
within the VM, rebuilding its monitors after a guardian crash. A poll cannot
start external work until ownership registration is acknowledged. Restarts
wait for old worker and downloader cleanup before allowing another poll.

Schema migrations and their version records commit together in one transaction,
so failed or interrupted DDL cannot leave a partially applied tracked migration.
The initial historical migration cannot downgrade `synchronous=FULL`.
Completion-history writes sync the file and directory entries, including on
retries where the ID is already visible; backup publication also syncs its
directory before and after the final rename.

The supervision integration test starts the actual application tree and kills
the subscription actor, actual HTTP supervisor, backup worker, and poll
guardian. It verifies replacement processes, recovered HTTP routes, and which
upstream services remain running. Further tests lose both a poll guardian and
manager while real fixture downloader processes run, and verify those processes
are stopped before a replacement manager starts.

The Plex preset saves H.264/AAC MP4, the original video title, upload date,
description, show/genre tags, chapter markers, artwork, episode thumbnails,
full `.info.json` metadata, and available English SRT captions
(both embedded and as sidecars). Completed channels update the database and
refresh Plex during the poll. Configure `PLEX_URL=http://127.0.0.1:32400` and
`PLEX_SECTION_ID` to your YouTube library's section ID. The local Plex credential
is used only inside Erlang and is never displayed in the UI.

For an external drive, set `STORAGE_VOLUME` to its mount path,
`STORAGE_VOLUME_UUID` to its actual volume UUID, and `MIN_FREE_GB=50`.
Gleam checks the disk before startup and every five seconds; it stops downloader
processes if the disk disappears, changes identity, or falls below the reserve.
Store `DATA_DIR`, `DB_PATH`, `OUTPUT_DIR`, and `TMPDIR` on that external drive.
When Movies or TV libraries scan the entire volume, opt into
`PLEX_PARENT_SCAN_EXCLUSION=true`: Gleam adds a rule excluding only `DATA_DIR`
from those parent scans. Point the dedicated YouTube library directly at
`DATA_DIR/library`; it remains visible because the ignore file is above its
library root. Existing ignore rules are preserved and symlinked files refused.

Mount identity and free-space checks do not establish filesystem or SSD health.
If ordinary directory listings or Plex media reads hang, stop writers and
preserve download history before checking the disk. An APFS verification reporting
I/O errors or an invalid fsroot tree requires storage recovery, not repeated
service restarts. Verify offline after stopping Plex and the puller; keep the
puller disabled until the filesystem and media reads pass. Back up data before
repair or replacement. A normal unmount is required; never force-eject to clear
an active filesystem operation.

To move storage, stop the service, back up the database, copy the complete data
tree (including download-history archives), and verify the copy before switching
the mount. Run the Gleam relocation command while both separate trees are
accessible:

```sh
gleam run -m storage_relocation_cli -- '/old/YouTube' '/new/YouTube'
```

It updates paths only in the copied database, preserves completed-video IDs and
scheduler settings, and invalidates generated downloader configs for regeneration.
Rerunning it is safe. It refuses overlapping roots, symlinked roots or databases,
and databases that identify the same physical file. Update the service's volume
UUID and data/output/temp paths and the dedicated Plex library location before
restarting. Keep the source until file checksums and playback are verified.

The LaunchAgent starts the Gleam shipment directly; no Python coordinator is
needed (`ytdl-sub` itself retains its normal Python runtime).

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
use anonymous requests by default. An explicitly configured `YOUTUBE_COOKIE_FILE`
can supply a local YouTube session. Requests pause one second between extraction
requests and a randomized five to ten seconds before each download. Keep the
cookie file private (mode 0600), outside the repository, and never upload it.
Gleam schedules a bounded pool of channel workers,
owns both polling passes, records downloads, and refreshes Plex.

On macOS, the optional `deploy/macos/export_youtube_session.py` helper exports
only YouTube-domain cookies from a browser you authorize. Run it with the Python
interpreter from your `ytdl-sub` environment. The installer also copies it to
`INSTALL_DIR/bin/export-youtube-session.py`. It accepts `--browser` (default:
Chrome) and `--output`; the default destination is
`~/Library/Application Support/video-puller/auth/youtube.cookies.txt`. The
default is manual export. To authorize automatic refresh before every poll, set
`YOUTUBE_SESSION_BROWSER=chrome` (or `firefox`/`safari`),
`YOUTUBE_SESSION_PYTHON` to the Python interpreter that has `yt_dlp`, and
`YOUTUBE_COOKIE_FILE` to the private export path. The installer supplies
`YOUTUBE_SESSION_HELPER` pointing to the installed helper and preserves these
settings on upgrades. An explicit empty browser setting disables refresh.
Refresh has a 30-second deadline and atomically replaces the export only on
success. A locked or inaccessible browser keeps the saved session available;
the next poll retries refresh. If no export exists, polling reports the local
access failure and backs off. macOS browser permissions must cover the runtime
interpreter. Account challenges can still require signing in locally; the
service reports the failure and keeps retrying instead of silently stopping.

By default each poll runs the engine twice over the same channel list: a **recent pass**
that inspects only the newest uploads of each channel, then a **backfill pass**
that works through remaining uploads within the configured age limit. A fresh upload is therefore picked up on
the next poll even while a long first backfill is still running. Set
`SUBSCRIPTION_BACKFILL=false` for recent uploads only. Each channel has a
10-minute deadline (configurable from 1 to 60 minutes). A shared authentication
or rate-limit failure stops pending channel requests and skips backfill, allowing
the scheduler to refresh the session and retry with backoff.

| Variable | Description | Default | Required |
|----------|-------------|---------|----------|
| `DATA_DIR` | Root for the engine layout and the download library | `./data` | No |
| `CHANNELS_TEMPLATE` | Channel list copied into place on first start | `./priv/ytdl-sub/channels.txt` | No |
| `POLL_TIMEOUT_MINUTES` | Total deadline for one poll, both passes included | `360` | No |
| `RECENT_VIDEOS` | Newest uploads the recent pass checks per channel | `5` | No |
| `RECENT_PHASE_MINUTES` | Budget cap for the recent pass (never more than a quarter of the poll) | `30` | No |
| `SUBSCRIPTION_WORKERS` | Simultaneous channel workers, configurable from 1 to 16 in the UI | `1` | No |
| `YOUTUBE_COOKIE_FILE` | Private local YouTube session file | Unset (anonymous) | No |
| `YOUTUBE_SESSION_BROWSER` | Explicit opt-in to refresh local browser cookies before polling | Unset (disabled) | No |
| `YOUTUBE_SESSION_PYTHON` | Python interpreter containing yt_dlp, required for automatic refresh | Unset | No |
| `YOUTUBE_SESSION_HELPER` | Local export helper path | Set by macOS installer | No |
| `CHANNEL_TIMEOUT_MINUTES` | Per-channel deadline, clamped to 1–60 minutes | `10` | No |
| `SUBSCRIPTION_BACKFILL` | Run the historical pass after recent uploads | `true` | No |
| `SUBSCRIPTION_DAYS` | Upload age limit for both passes; 0 means full history | `0` (`30` in macOS installer) | No |
| `SUBSCRIPTION_SKIP_RESTRICTED` | Skip videos marked premium-only or subscriber-only by yt-dlp; missing availability metadata passes | `true` | No |
| `POLL_ON_START` | Start an enabled subscription poll immediately at startup | `false` (`true` in macOS installer) | No |
| `STORAGE_VOLUME` | Optional mounted volume required before startup and during downloads | unset | No |
| `STORAGE_VOLUME_UUID` | Expected volume identity, preventing writes to a different disk | unset | No |
| `MIN_FREE_GB` | Stop downloads when the required volume drops below this free-space reserve | `50` | No |
| `PLEX_URL` | Optional local Plex server URL | unset | No |
| `PLEX_SECTION_ID` | Plex library section to refresh and enrich | unset | No |
| `PLEX_MANAGED_TRASH_CLEANUP` | Clear stale metadata from the exact healthy managed YouTube section only | `false` | No |
| `PLEX_SCAN_COOLDOWN_SECONDS` | Shared minimum spacing between scan attempts, clamped to 30–3600 seconds | `300` | No |
| `PLEX_PARENT_SCAN_EXCLUSION` | Exclude DATA_DIR from volume-root Plex libraries; dedicated library must use DATA_DIR/library | `false` | No |

The Subscriptions UI saves worker, age and restricted-video settings in
`${DATA_DIR}/ytdl-sub/runtime.json`; changes apply to the next poll. Older
runtime files without `skip_restricted` default to `true`. Native
Gleam callbacks publish live channel and transfer status, and completed files
appear in the feed after each worker finishes. Media, archives, working files,
and caches remain under the configured data volume. The storage guard checks
the mounted disk identity and free space every five seconds and stops the
downloader process trees if the required disk becomes unavailable.

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

Lines accept an optional label (`Display Name = URL`) and channel, playlist or
single-video URLs. Channel forms (`@handle`, its `/videos`, `/shorts`,
`/streams` tabs, or `channel/UC…`, with or without a trailing slash) collapse
to one subscription; a query that selects the content (`?v=`, `?list=`) is
preserved while decorative queries and fragments are dropped.

Videos land in `${DATA_DIR:-./data}/library/<Channel>/Season <Year>/`. The first
poll pulls uploads within `SUBSCRIPTION_DAYS` (full history when zero); ytdl-sub's per-channel download
archive under the library root keeps later polls incremental, so only new
uploads are fetched. A long first backfill does not hold up new uploads: each
poll's recent pass inspects the newest uploads before the backfill resumes
where its own deadline stopped it, so set `POLL_TIMEOUT_MINUTES` to the time
you are happy to have the engine running per poll.

Each channel downloads one video at a time; `SUBSCRIPTION_WORKERS` bounds how
many channels run concurrently. Source request pacing remains enabled, and
video conversion can limit throughput even on a fast connection. Files can be
large; the storage guard preserves `MIN_FREE_GB` when a volume is configured.
Subscription pulls use ytdl-sub's
"Plex TV Show by Date" preset with its resolution assert disabled: the assert
aborts on any download below 361p, which false-positives on genuinely low-res
uploads rather than throttling. Throttle protection's request pacing stays on,
and a poll that exits non-zero still records the files it did download while
reporting the per-subscription errors.

Polling is incremental and repeat-safe: ytdl-sub's download archive skips
episodes already on disk, and every seen-video row carries the video's YouTube
id from its `.info.json` sidecar (falling back to the file's library-relative
path), so re-recording updates the row instead of duplicating it. Alongside
the video, the Plex preset writes Plex-style metadata — `poster.jpg` and
`fanart.jpg` at the show root, an episode `-thumb.jpg` beside each video, and
an mp4/h264 file with embedded tags. Plex does not read NFO sidecars, so none
are generated (the Jellyfin/Emby presets add those); the `.info.json` files
beside each episode are the engine's own metadata and are ignored by Plex.

The same channel can be listed in several URL forms (`@handle`, `@handle/videos`,
`@handle/shorts`, `channel/UC…`, with or without a trailing slash). Channel
identity collapses those to one subscription, so a channel never gets two Plex
shows or two download archives; the channel id is resolved once through
`yt-dlp` and cached in `${DATA_DIR}/ytdl-sub/channel_ids.txt`. When two entries
merge, the label that already has a `library/<label>/` directory wins, which
keeps an existing archive (and Plex show) intact.

An interrupted poll is safe too: the engine writes into
`${DATA_DIR}/ytdl-sub/working` until a file is complete, a deadline kills the
engine's whole process tree, and after every poll — successful or timed out —
the app reconciles the library, recording media files that have no row yet and
ignoring thumbnails, sidecars and archives. A poll that times out mid-backfill
therefore keeps what it already downloaded, and the next poll skips those
episodes through the download archive.
`POLL_TIMEOUT_MINUTES` is a total deadline for one poll: the recent pass takes
at most its configured slice (and never more than a quarter), the backfill pass
gets the rest, streaming output cannot extend either, and a timed-out pass
reports the tail of its engine output so a stalled enumeration (a very large
channel) is diagnosable. Each pass reports its own file and error counts, so a
poll says plainly how much of the list it covered.

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
   Source video titles are retained, alongside full info JSON, artwork,
   thumbnails, and available English captions as SRT sidecars
   and embedded subtitles.
3. Plex must be able to read the tree. The bundled Linux installer leaves the
   data root mode 711 (traversable, not listable) with `library/` at 755, so a
   Plex server running as another user can read the shows while the database
   stays private. On macOS the LaunchAgent runs as your user, so nothing extra
   is needed.
4. With `PLEX_URL` and `PLEX_SECTION_ID` configured, Gleam requests a library
   refresh and restores source video titles for indexed episodes after each
   channel and at startup. The local macOS Plex token is read in memory from
   Plex preferences. Enable Plex's automatic scans to index files promptly.
   Without the optional connection, Plex can scan the same library normally.
   `PLEX_MANAGED_TRASH_CLEANUP=true` is an optional, disabled-by-default
   setting that clears missing metadata only from the configured YouTube
   section. It runs only when Plex reports an idle TV section whose sole
   location exactly matches `DATA_DIR/library`, the configured storage volume
   UUID is mounted, every existing managed video is indexed, and each missing
   item's nearest existing ancestor inside the library is readable. It uses the section-specific
   `emptyTrash` endpoint and never clears server-wide or other-section trash.

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
- **ffmpeg**: mp4/h264 conversion, thumbnails and embedded metadata for both
  subscription pulls and manual jobs
  ```bash
  apt install ffmpeg     # or: pacman -S ffmpeg / brew install ffmpeg
  ```
- **deno**: JS runtime yt-dlp uses for YouTube challenge handling
  ```bash
  apt install deno       # or: pacman -S deno / brew install deno
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
- [ ] Set `DATA_DIR` (engine layout + Plex library) and seed
      `DATA_DIR/ytdl-sub/channels.txt`, or drop a Takeout `subscriptions.csv`
      beside it
- [ ] Set `POLL_TIMEOUT_MINUTES` above the expected first-backfill time for
      your largest channel
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
