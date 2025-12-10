# YouTube Subscription Auto-Download Feature Plan

## Overview

Add automatic downloading of videos from YouTube subscriptions using yt-dlp cookies authentication, with configurable filtering (age, duration, keywords) and both automatic polling and manual refresh capabilities.

---

## Architecture Decision: yt-dlp Cookies Approach

Using yt-dlp's `--cookies-from-browser` flag to access YouTube subscriptions:

```bash
yt-dlp --cookies-from-browser firefox --flat-playlist "https://www.youtube.com/feed/subscriptions"
```

This approach:
- Requires no API key setup
- Uses existing browser authentication
- Integrates cleanly with our existing yt-dlp infrastructure
- Provides reliable access to subscription feed

---

## Implementation Phases

### Phase 1: Database Schema & Domain Types

**New Migration: `003_subscriptions.sql`**

```sql
-- Subscription feed configuration
CREATE TABLE IF NOT EXISTS subscription_config (
    id INTEGER PRIMARY KEY DEFAULT 1,
    enabled INTEGER NOT NULL DEFAULT 0,
    poll_interval_minutes INTEGER NOT NULL DEFAULT 60,
    browser TEXT NOT NULL DEFAULT 'firefox',
    cookies_path TEXT,  -- Optional override for cookies file
    max_age_days INTEGER NOT NULL DEFAULT 7,
    min_duration_seconds INTEGER NOT NULL DEFAULT 120,  -- 2 min to skip Shorts
    max_duration_seconds INTEGER DEFAULT NULL,
    keyword_filter TEXT,  -- JSON array of keywords, OR logic
    keyword_exclude TEXT, -- JSON array of excluded keywords
    last_poll_at INTEGER,
    created_at INTEGER NOT NULL,
    updated_at INTEGER NOT NULL,
    CHECK(id = 1)  -- Singleton config row
);

-- Track seen videos to avoid re-downloading
CREATE TABLE IF NOT EXISTS seen_videos (
    video_id TEXT PRIMARY KEY NOT NULL,
    channel_id TEXT,
    channel_name TEXT,
    title TEXT NOT NULL,
    published_at INTEGER,
    duration_seconds INTEGER,
    thumbnail_url TEXT,
    first_seen_at INTEGER NOT NULL,
    downloaded INTEGER NOT NULL DEFAULT 0,
    skipped INTEGER NOT NULL DEFAULT 0,
    skip_reason TEXT,
    job_id TEXT REFERENCES video_jobs(id)
);

CREATE INDEX IF NOT EXISTS idx_seen_videos_published ON seen_videos(published_at);
CREATE INDEX IF NOT EXISTS idx_seen_videos_downloaded ON seen_videos(downloaded);
CREATE INDEX IF NOT EXISTS idx_seen_videos_channel ON seen_videos(channel_id);

-- Per-channel overrides (optional advanced feature)
CREATE TABLE IF NOT EXISTS channel_settings (
    channel_id TEXT PRIMARY KEY NOT NULL,
    channel_name TEXT NOT NULL,
    enabled INTEGER NOT NULL DEFAULT 1,
    priority INTEGER NOT NULL DEFAULT 0,
    max_age_days INTEGER,  -- NULL = use global
    min_duration_seconds INTEGER,
    max_duration_seconds INTEGER,
    keyword_filter TEXT,
    keyword_exclude TEXT,
    created_at INTEGER NOT NULL,
    updated_at INTEGER NOT NULL
);
```

**New Domain Types** (`src/domain/subscription_types.gleam`):

```gleam
/// Subscription feature configuration
pub type SubscriptionConfig {
  SubscriptionConfig(
    enabled: Bool,
    poll_interval_minutes: Int,
    browser: Browser,
    cookies_path: Option(String),
    max_age_days: Int,
    min_duration_seconds: Int,        // Default 120s (2 min) to skip Shorts
    max_duration_seconds: Option(Int),
    keyword_filter: List(String),
    keyword_exclude: List(String),
    last_poll_at: Option(Int),
  )
}

/// Supported browsers for cookie extraction
pub type Browser {
  Firefox
  Chrome
  Chromium
  Edge
  Brave
  Safari
}

/// Video discovered from subscription feed
pub type DiscoveredVideo {
  DiscoveredVideo(
    video_id: String,
    channel_id: Option(String),
    channel_name: Option(String),
    title: String,
    url: String,
    published_at: Option(Int),
    duration_seconds: Option(Int),
    thumbnail_url: Option(String),
  )
}

/// Result of filtering a video
pub type FilterResult {
  PassedFilter
  SkippedTooOld
  SkippedTooShort
  SkippedTooLong
  SkippedNoKeywordMatch
  SkippedExcludedKeyword(String)
  SkippedAlreadySeen
  SkippedAlreadyDownloaded
}

/// Subscription poll result
pub type PollResult {
  PollResult(
    total_found: Int,
    new_videos: Int,
    queued_for_download: Int,
    skipped: Int,
    errors: List(String),
  )
}
```

---

### Phase 2: Subscription Feed Fetcher (`src/engine/subscription_feed.gleam`)

Core functionality to fetch subscription feed using yt-dlp:

```gleam
/// Fetch subscription feed from YouTube
pub fn fetch_feed(config: SubscriptionConfig) -> Result(List(DiscoveredVideo), String)

/// Build yt-dlp args for subscription feed
pub fn build_feed_args(config: SubscriptionConfig) -> List(String)

/// Parse yt-dlp JSON output into DiscoveredVideo list
pub fn parse_feed_output(json_output: String) -> Result(List(DiscoveredVideo), String)
```

**yt-dlp Command Structure:**
```bash
yt-dlp \
  --cookies-from-browser firefox \
  --flat-playlist \
  --dump-json \
  --playlist-items 1-50 \
  "https://www.youtube.com/feed/subscriptions"
```

Key implementation details:
- Use `--flat-playlist` for fast metadata-only fetch
- Limit items with `--playlist-items` to control scope
- Parse JSON output line-by-line (one JSON object per video)
- Extract: id, title, channel_id, channel, duration, timestamp, thumbnail

---

### Phase 3: Video Filter Engine (`src/engine/video_filter.gleam`)

Filtering logic to decide which videos to download:

```gleam
/// Check if a video passes all configured filters
pub fn should_download(
  video: DiscoveredVideo,
  config: SubscriptionConfig,
  channel_override: Option(ChannelSettings),
) -> FilterResult

/// Age filter - check if video was published within max_age_days
fn check_age_filter(published_at: Option(Int), max_age_days: Int) -> FilterResult

/// Duration filter - check min/max duration bounds
fn check_duration_filter(
  duration: Option(Int),
  min_seconds: Option(Int),
  max_seconds: Option(Int),
) -> FilterResult

/// Keyword filter - check if title matches any required keywords
fn check_keyword_filter(title: String, keywords: List(String)) -> FilterResult

/// Exclusion filter - check if title contains excluded keywords
fn check_exclusion_filter(title: String, excluded: List(String)) -> FilterResult
```

Filter precedence:
1. Already downloaded? -> Skip
2. Already seen and skipped? -> Skip (respect previous decision)
3. Age filter -> Skip if too old
4. Duration filter -> Skip if outside bounds
5. Keyword inclusion filter -> Skip if no match (when keywords configured)
6. Keyword exclusion filter -> Skip if matches exclusion
7. Pass -> Queue for download

---

### Phase 4: Subscription Manager Actor (`src/core/subscription_manager.gleam`)

OTP actor to manage subscription polling lifecycle:

```gleam
pub type SubscriptionMessage {
  Poll                           // Trigger immediate poll
  ScheduleNextPoll               // Schedule next automatic poll
  UpdateConfig(SubscriptionConfig)
  GetStatus(Subject(SubscriptionStatus))
  Shutdown
}

pub type SubscriptionStatus {
  SubscriptionStatus(
    enabled: Bool,
    last_poll_at: Option(Int),
    next_poll_at: Option(Int),
    last_result: Option(PollResult),
    is_polling: Bool,
  )
}

/// Start the subscription manager actor
pub fn start(db: Db, manager: Subject(ManagerMessage)) -> Result(Subject(SubscriptionMessage), StartError)

/// Handle poll message - fetch feed, filter, and queue downloads
fn handle_poll(state: State) -> State

/// Schedule next poll based on config interval
fn schedule_next_poll(state: State) -> State
```

Integration with existing manager:
- Subscription manager creates jobs via `repo.insert_job()`
- Jobs are picked up by existing `manager.gleam` polling
- Uses same download queue infrastructure

---

### Phase 5: Repository Layer (`src/infra/subscription_repo.gleam`)

Data access for subscription-related tables:

```gleam
/// Get subscription config (singleton)
pub fn get_config(db: Db) -> Result(SubscriptionConfig, DbError)

/// Update subscription config
pub fn update_config(db: Db, config: SubscriptionConfig) -> Result(Nil, DbError)

/// Mark video as seen
pub fn mark_seen(db: Db, video: DiscoveredVideo, downloaded: Bool, skip_reason: Option(String)) -> Result(Nil, DbError)

/// Check if video was already seen
pub fn is_seen(db: Db, video_id: String) -> Result(Bool, DbError)

/// Get seen video by ID
pub fn get_seen_video(db: Db, video_id: String) -> Result(Option(SeenVideo), DbError)

/// List recent seen videos for UI display
pub fn list_seen_videos(db: Db, limit: Int, offset: Int) -> Result(List(SeenVideo), DbError)

/// Get channel settings override
pub fn get_channel_settings(db: Db, channel_id: String) -> Result(Option(ChannelSettings), DbError)

/// Save/update channel settings
pub fn upsert_channel_settings(db: Db, settings: ChannelSettings) -> Result(Nil, DbError)
```

---

### Phase 6: Web UI Extensions

**New Route: `/subscriptions`** - Main subscriptions management page

**New Handler Functions** (`src/web/handlers.gleam`):
```gleam
/// Show subscriptions page
pub fn subscriptions(req: Request, ctx: Context) -> Response

/// Update subscription settings
pub fn update_subscription_config(req: Request, ctx: Context) -> Response

/// Trigger manual poll
pub fn poll_subscriptions(req: Request, ctx: Context) -> Response

/// Show subscription feed (recent discoveries)
pub fn subscription_feed(req: Request, ctx: Context) -> Response
```

**New Templates** (`src/web/templates.gleam`):
```gleam
/// Subscriptions settings page
pub fn subscriptions_page(
  config: SubscriptionConfig,
  status: SubscriptionStatus,
  recent_videos: List(SeenVideo),
) -> Element(a)

/// Subscription feed list component
pub fn subscription_feed_list(videos: List(SeenVideo)) -> Element(a)
```

**UI Features:**
1. Enable/disable subscriptions toggle
2. Poll interval selector (15min, 30min, 1hr, 2hr, 6hr, 12hr, 24hr)
3. Browser selector for cookies (Firefox, Chrome, etc.)
4. Age filter input (days)
5. Duration filter inputs (min/max minutes)
6. Keyword filter input (comma-separated)
7. Exclusion keywords input
8. Manual "Refresh Now" button
9. Last poll timestamp and result summary
10. Recent discoveries list (with status badges: downloaded, skipped, pending)

---

### Phase 7: Integration & Startup

**Updates to `src/video_puller.gleam`:**
```gleam
pub fn main() -> Nil {
  // ... existing initialization ...

  // Start subscription manager if enabled
  case subscription_repo.get_config(db) {
    Ok(config) if config.enabled -> {
      case subscription_manager.start(db, manager_subject) {
        Ok(sub_manager) -> {
          io.println("Subscription manager started")
          // Trigger initial poll
          process.send(sub_manager, subscription_types.Poll)
        }
        Error(_) -> io.println("Failed to start subscription manager")
      }
    }
    _ -> Nil
  }

  // ... rest of startup ...
}
```

**Updates to `src/web/middleware.gleam`:**
Add subscription_manager subject to Context for web handlers to trigger polls.

---

## File Structure Summary

```
src/
├── domain/
│   ├── types.gleam                    # Existing
│   └── subscription_types.gleam       # NEW
├── engine/
│   ├── downloader.gleam               # Existing
│   ├── ytdlp.gleam                    # Existing (minor additions)
│   ├── subscription_feed.gleam        # NEW - fetch subscription data
│   └── video_filter.gleam             # NEW - filtering logic
├── core/
│   ├── manager.gleam                  # Existing
│   └── subscription_manager.gleam     # NEW - polling orchestrator
├── infra/
│   ├── db.gleam                       # Existing
│   ├── repo.gleam                     # Existing
│   └── subscription_repo.gleam        # NEW - subscription data access
├── web/
│   ├── router.gleam                   # Updated - new routes
│   ├── handlers.gleam                 # Updated - new handlers
│   └── templates.gleam                # Updated - new templates
└── video_puller.gleam                 # Updated - startup integration

priv/migrations/
└── 003_subscriptions.sql              # NEW - schema migration
```

---

## Implementation Order

1. **Database & Types** (Foundation)
   - Create migration `003_subscriptions.sql`
   - Create `subscription_types.gleam`
   - Create `subscription_repo.gleam`

2. **Feed Fetcher** (Core Engine)
   - Create `subscription_feed.gleam`
   - Add yt-dlp subscription args to `ytdlp.gleam`
   - Test feed parsing

3. **Filter Engine**
   - Create `video_filter.gleam`
   - Unit tests for all filter types

4. **Subscription Manager Actor**
   - Create `subscription_manager.gleam`
   - Integrate with main startup
   - Test polling cycle

5. **Web UI**
   - Add routes to `router.gleam`
   - Add handlers to `handlers.gleam`
   - Add templates to `templates.gleam`
   - Test full UI flow

6. **Polish & Testing**
   - Integration tests
   - Error handling improvements
   - Documentation

---

## Configuration Example

Environment variables for subscription feature:

```bash
# Subscription defaults
SUBSCRIPTION_ENABLED=true
SUBSCRIPTION_POLL_INTERVAL=60        # minutes
SUBSCRIPTION_BROWSER=firefox
SUBSCRIPTION_MAX_AGE_DAYS=7
SUBSCRIPTION_MIN_DURATION=120        # seconds (2 min default to skip Shorts)
SUBSCRIPTION_MAX_DURATION=0          # seconds, 0 = no maximum
SUBSCRIPTION_KEYWORDS=               # comma-separated, empty = all
SUBSCRIPTION_EXCLUDE=                # comma-separated keywords to exclude
```

**Note:** The 2-minute minimum duration default effectively filters out YouTube Shorts, which are typically under 60 seconds.

---

## Risk Mitigation

1. **Browser Cookie Access**
   - yt-dlp may need browser to be closed for Firefox
   - Provide clear error messages for cookie extraction failures
   - Allow manual cookies file path as fallback

2. **YouTube Rate Limiting**
   - Default to 1-hour poll interval
   - Implement exponential backoff on errors
   - Limit playlist items fetched per poll

3. **Large Subscription Feeds**
   - Paginate feed fetching with `--playlist-items`
   - Process in batches to avoid memory issues
   - Store seen videos to avoid re-processing

4. **Stale Cookies**
   - Detect auth failures and alert user
   - Provide manual re-auth instructions

---

## Success Criteria

- [ ] Subscription feed fetches successfully with browser cookies
- [ ] Filters correctly apply age/duration/keyword rules
- [ ] New videos automatically queue for download
- [ ] Manual refresh works from UI
- [ ] Polling runs at configured intervals
- [ ] UI shows subscription status and recent discoveries
- [ ] Already-downloaded videos are not re-queued
- [ ] Error states are handled gracefully with user feedback
