# AI Agent Instructions for video-puller

This document contains instructions for AI coding agents working on this Gleam project.

## Gleam Development Guidelines

See `.claude/CLAUDE.md` for detailed Gleam development guidelines, code style, and workflow instructions.

## Subscription Engine (ytdl-sub, Takeout, Plex)

Subscriptions are pulled by `ytdl-sub` from public channel URLs — no browser
cookies and no account access at any point.

- **Channel list**: `${DATA_DIR:-./data}/ytdl-sub/channels.txt`, one channel per
  line — either a URL or `Display Name = URL` — `#` comments allowed; seeded
  from `priv/ytdl-sub/channels.txt`. Channel, playlist and single-video URLs
  are all accepted; a query that selects the content (`?v=`, `?list=`) is
  preserved, decorative queries and fragments are stripped.
- **Channel identity**: `@handle`, `@handle/videos`, `@handle/shorts`,
  `@handle/streams` and `channel/UC…` (with or without a trailing slash) all
  collapse to one subscription, so a channel can never get two Plex shows or
  two download archives. The channel id is resolved once through `yt-dlp` and
  cached in `${DATA_DIR}/ytdl-sub/channel_ids.txt`; when `yt-dlp` is missing or
  the lookup fails, the channel stays usable and dedupe falls back to the
  canonical URL. Merging entries keeps the label that already has a
  `library/<label>/` directory, which preserves the existing archive.
- **Takeout import (no sign-in)**: drop a Google Takeout `subscriptions.csv`
  at `${DATA_DIR}/ytdl-sub/subscriptions.csv`; the next startup or poll merges
  missing channels into `channels.txt` as `Channel Title = URL` (the CSV is
  parsed quote-aware, rows without a YouTube channel URL are rejected and
  reported), then renames the CSV to `subscriptions.csv.imported`.
- **Generated config**: `${DATA_DIR}/ytdl-sub/subscriptions.yaml` uses
  ytdl-sub's `Plex TV Show by Date` preset with `enable_resolution_assert:
  False`. Plex-style metadata comes from that preset: `poster.jpg` +
  `fanart.jpg` at the show root, an episode `-thumb.jpg` beside each video,
  mp4/h264 conversion, Plex-safe title sanitization, embedded video tags
  (`title`, `date`, `genre`, `synopsis`, `show`) and chapters where the source
  has them. No NFO sidecars — Plex does not consume NFO (that is
  Jellyfin/Emby/Kodi territory; only the Jellyfin preset chain adds them).
- **Library layout**: `${DATA_DIR}/library/<Channel>/Season <Year>/` with the
  per-show archive `.ytdl-sub-<Label>-download-archive.json` inside the show
  directory.
- **Polling**: DB-backed config (`poll_interval_minutes`); manual trigger is
  `POST /subscriptions/poll` ("Refresh Now" on `/subscriptions`);
  `POLL_TIMEOUT_MINUTES` (default 360) caps one poll.
- **Two passes per poll**: `subscriptions-recent.yaml` runs first, with every
  channel capped to its `RECENT_VIDEOS` (default 5) newest uploads, so fresh
  uploads are fetched while a backlog is still being worked through; then
  `subscriptions.yaml` (full history) runs on the remaining budget. The recent
  pass takes at most `RECENT_PHASE_MINUTES` (default 30) and never more than a
  quarter of the poll; the backfill always keeps at least a minute.
  `phase_timeouts/2` owns the split, and `merge_summaries/2` combines the
  outcomes — files from a surviving pass are kept, a failed pass is reported,
  and a file both passes reported is counted once. A persisted `poll_cursor.txt`
  rotates the starting channel each poll, including after timeouts/restarts.
  Cold-cache channel lookups share a bounded preparation budget. Duplicate
  display labels receive unique suffixes before rotation; missing lookup IDs
  never merge unrelated subscriptions.

### Idempotency guarantees

Repeat runs are safe by construction; keep these invariants when refactoring:

- **Engine**: `maintain_download_archive` prevents re-downloading archived
  episodes. The recent pass keeps `break_on_existing`; the full-history pass
  explicitly disables it so recent downloads and interrupted runs cannot hide
  older episodes behind the first archived video.
- **DB**: `seen_videos.video_id` is the PRIMARY KEY and rows are written via
  `INSERT OR REPLACE` (`mark_seen`), so re-recording updates instead of
  duplicating.
- **Takeout**: the export is renamed after import, and `merge_channels` appends
  only URLs not already listed — a second import is a no-op returning `None`.
- **Migrations**: tracked in `schema_migrations`; re-running is a no-op.
- **Partial runs**: the engine writes only under
  `${DATA_DIR}/ytdl-sub/working` until files are complete, and `seen_videos`
  gets rows solely from the engine's "Files created" report — a killed or
  timed-out poll records nothing directly, and the next poll re-checks the
  channel and skips episodes already in ytdl-sub's download archive.
- **Reconciliation**: because the archive also hides files a cut-short poll
  already downloaded, `reconcile_library` walks the library after every poll
  (ok or error) and at startup, records media files with no row, ignores
  thumbnails/sidecars/archives, and reports rows that could not be written as
  poll errors instead of dropping them.
- **Record identity**: `seen_videos.video_id` comes from the engine's
  `.info.json` sidecar (`id`, plus `channel_id`) when one is present, and falls
  back to the library-relative path otherwise. The poll report and
  `reconcile_library` use the same mapping, so a file is never recorded twice
  under two identities.
- **Timeouts**: `POLL_TIMEOUT_MINUTES` is a total deadline for one poll, split
  across its two passes (`shell.run_with_timeout` per pass); streaming output
  cannot extend either, and EOF never
  fabricates a zero exit — the child's real exit status is used. A timed-out
  process tree is killed explicitly: closing the port alone does not stop
  ytdl-sub, which keeps downloading in the background and holds its
  working-directory lock (which would block the next poll). The timeout error
  carries the tail of the command's output, so a stalled poll (for example a
  first enumeration of a very large channel) is diagnosable from the poll row.
- **Polls**: a poll that is already running is skipped ("Poll already in
  progress, skipping").
- **Schedule**: exactly one timer chain exists. Arming cancels the pending
  timer and bumps a generation, stale `ScheduledPoll` messages are ignored, and
  manual polls / settings saves re-arm rather than adding chains. A watchdog
  fires if a poll worker never reports back, so `is_polling` cannot stick.

### Manual download jobs

The URL box on `/` submits a job to the worker pool (the engine is only used
for subscriptions):

- Each job carries a `DownloadConfig` snapshot taken when it is submitted, so
  a Settings save cannot change a queued or running job's format/limits. The
  manager loads the persisted config at startup (`repo.get_download_config`)
  and falls back to the environment/default config only when no row exists.
  A relative `output_directory` (the default is `./downloads`) is resolved
  against `DATA_DIR` when the config is loaded: the systemd unit runs with a
  read-only working directory, so a relative directory would land outside the
  writable data root.
- `yt-dlp` is asked for `--print after_move:filepath`, so the job records the
  file the post-processor actually moved into place (nested channel folders
  included) instead of guessing `<job id>.<ext>` under the configured
  directory. The reported path is verified with `simplifile.is_file` before
  the job completes, the NFO sidecar is written beside that exact path, and
  the manager persists it (`repo.update_path`) for the UI.

### Verify the pipeline locally

```sh
gleam format --check src test && gleam check && gleam test

DATA_DIR=/tmp/vp-check DB_PATH=/tmp/vp-check/video_eater.db PORT=8097 \
  POLL_TIMEOUT_MINUTES=2 gleam run   # pick a free port; a stale instance EADDRINUSEs
curl -X POST -d "enabled=1&poll_interval_minutes=60" \
  http://127.0.0.1:8097/subscriptions/settings
curl -X POST http://127.0.0.1:8097/subscriptions/poll
# Watch the log: POLL_START -> "Files created:" -> POLL_END found=/new=/errors=
# A repeat poll of the same channels downloads nothing (archive) and reports found=0.
# A timed-out poll still ends with "Reconciled: ..." lines for what it committed:
sqlite3 /tmp/vp-check/video_eater.db 'select count(*) from seen_videos'
find /tmp/vp-check/library -name '*.mp4' | wc -l          # counts must agree
ffprobe -v error -show_entries format_tags "…/Season 2021/"*.mp4 | head
# Plex-facing files per show: poster.jpg, fanart.jpg, Season <year>/<episode>.mp4,
# <episode>-thumb.jpg, <episode>.info.json, .ytdl-sub-<label>-download-archive.json
```

## Project Resources

For more information:
- **README.md** - Project overview and quick start
- **justfile** - Available just commands
- **.claude/docs/mcp-agent-setup.md** - MCP agent coordination setup
