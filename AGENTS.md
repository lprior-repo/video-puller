# AI Agent Instructions for video-puller

This document contains instructions for AI coding agents working on this Gleam project.

## Issue Tracking with bd (beads)

**IMPORTANT**: This project uses **bd (beads)** for ALL issue tracking. Do NOT use markdown TODOs, task lists, or other tracking methods.

### Why bd?

- Dependency-aware: Track blockers and relationships between issues
- Git-friendly: Auto-syncs to JSONL for version control
- Agent-optimized: JSON output, ready work detection, discovered-from links
- Prevents duplicate tracking systems and confusion

### Quick Start

**Check for ready work:**
```bash
bd ready --json
```

**Create new issues:**
```bash
bd create "Issue title" -t bug|feature|task -p 0-4 --json
bd create "Issue title" -p 1 --deps discovered-from:bd-123 --json
bd create "Subtask" --parent <epic-id> --json  # Hierarchical subtask (gets ID like epic-id.1)
```

**Claim and update:**
```bash
bd update bd-42 --status in_progress --json
bd update bd-42 --priority 1 --json
```

**Complete work:**
```bash
bd close bd-42 --reason "Completed" --json
```

### Issue Types

- `bug` - Something broken
- `feature` - New functionality
- `task` - Work item (tests, docs, refactoring)
- `epic` - Large feature with subtasks
- `chore` - Maintenance (dependencies, tooling)

### Priorities

- `0` - Critical (security, data loss, broken builds)
- `1` - High (major features, important bugs)
- `2` - Medium (default, nice-to-have)
- `3` - Low (polish, optimization)
- `4` - Backlog (future ideas)

### Workflow for AI Agents

1. **Check ready work**: `bd ready` shows unblocked issues
2. **Claim your task**: `bd update <id> --status in_progress`
3. **Work on it**: Implement, test, document
4. **Discover new work?** Create linked issue:
   - `bd create "Found bug" -p 1 --deps discovered-from:<parent-id>`
5. **Complete**: `bd close <id> --reason "Done"`
6. **Commit together**: Always commit the `.beads/issues.jsonl` file together with the code changes so issue state stays in sync with code state

### Auto-Sync

bd automatically syncs with git:
- Exports to `.beads/issues.jsonl` after changes (5s debounce)
- Imports from JSONL when newer (e.g., after `git pull`)
- No manual export/import needed!

### GitHub Copilot Integration

If using GitHub Copilot, also create `.github/copilot-instructions.md` for automatic instruction loading.
Run `bd onboard` to get the content, or see step 2 of the onboard instructions.

### MCP Server (Recommended)

If using Claude or MCP-compatible clients, install the beads MCP server:

```bash
pip install beads-mcp
```

Add to MCP config (e.g., `~/.config/claude/config.json`):
```json
{
  "beads": {
    "command": "beads-mcp",
    "args": []
  }
}
```

Then use `mcp__beads__*` functions instead of CLI commands.

### Managing AI-Generated Planning Documents

AI assistants often create planning and design documents during development:
- PLAN.md, IMPLEMENTATION.md, ARCHITECTURE.md
- DESIGN.md, CODEBASE_SUMMARY.md, INTEGRATION_PLAN.md
- TESTING_GUIDE.md, TECHNICAL_DESIGN.md, and similar files

**Best Practice: Use a dedicated directory for these ephemeral files**

**Recommended approach:**
- Create a `history/` directory in the project root
- Store ALL AI-generated planning/design docs in `history/`
- Keep the repository root clean and focused on permanent project files
- Only access `history/` when explicitly asked to review past planning

**Example .gitignore entry (optional):**
```
# AI planning documents (ephemeral)
history/
```

**Benefits:**
- ✅ Clean repository root
- ✅ Clear separation between ephemeral and permanent documentation
- ✅ Easy to exclude from version control if desired
- ✅ Preserves planning history for archeological research
- ✅ Reduces noise when browsing the project

### CLI Help

Run `bd <command> --help` to see all available flags for any command.
For example: `bd create --help` shows `--parent`, `--deps`, `--assignee`, etc.

### Important Rules

- ✅ Use bd for ALL task tracking
- ✅ Always use `--json` flag for programmatic use
- ✅ Link discovered work with `discovered-from` dependencies
- ✅ Check `bd ready` before asking "what should I work on?"
- ✅ Store AI planning docs in `history/` directory
- ✅ Run `bd <cmd> --help` to discover available flags
- ❌ Do NOT create markdown TODO lists
- ❌ Do NOT use external issue trackers
- ❌ Do NOT duplicate tracking systems
- ❌ Do NOT clutter repo root with planning documents

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
  `POLL_TIMEOUT_MINUTES` (default 360) caps one engine run.

### Idempotency guarantees

Repeat runs are safe by construction; keep these invariants when refactoring:

- **Engine**: the preset sets `maintain_download_archive` +
  `break_on_existing`, so a later poll never re-downloads existing episodes.
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
- **Timeouts**: `POLL_TIMEOUT_MINUTES` is a total deadline for the engine run
  (`shell.run_with_timeout`); streaming output cannot extend it, and EOF never
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
- **.claude/docs/beads-setup.md** - Detailed beads documentation
- **.claude/docs/mcp-agent-setup.md** - MCP agent coordination setup
