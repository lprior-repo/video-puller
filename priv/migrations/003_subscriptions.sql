-- Subscription auto-download feature schema
-- Adds tables for subscription configuration and video tracking

-- Subscription feed configuration (singleton)
CREATE TABLE IF NOT EXISTS subscription_config (
    id INTEGER PRIMARY KEY DEFAULT 1,
    enabled INTEGER NOT NULL DEFAULT 0,
    poll_interval_minutes INTEGER NOT NULL DEFAULT 60,
    browser TEXT NOT NULL DEFAULT 'firefox',
    cookies_path TEXT,
    max_age_days INTEGER NOT NULL DEFAULT 7,
    min_duration_seconds INTEGER NOT NULL DEFAULT 120,
    max_duration_seconds INTEGER,
    keyword_filter TEXT,
    keyword_exclude TEXT,
    last_poll_at INTEGER,
    created_at INTEGER NOT NULL,
    updated_at INTEGER NOT NULL,
    CHECK(id = 1)
);

-- Insert default config row
INSERT OR IGNORE INTO subscription_config (id, enabled, poll_interval_minutes, browser, max_age_days, min_duration_seconds, created_at, updated_at)
VALUES (1, 0, 60, 'firefox', 7, 120, strftime('%s', 'now'), strftime('%s', 'now'));

-- Track seen videos to avoid re-downloading
CREATE TABLE IF NOT EXISTS seen_videos (
    video_id TEXT PRIMARY KEY NOT NULL,
    channel_id TEXT,
    channel_name TEXT,
    title TEXT NOT NULL,
    url TEXT NOT NULL,
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
CREATE INDEX IF NOT EXISTS idx_seen_videos_first_seen ON seen_videos(first_seen_at);

-- Per-channel settings overrides (optional advanced feature)
CREATE TABLE IF NOT EXISTS channel_settings (
    channel_id TEXT PRIMARY KEY NOT NULL,
    channel_name TEXT NOT NULL,
    enabled INTEGER NOT NULL DEFAULT 1,
    priority INTEGER NOT NULL DEFAULT 0,
    max_age_days INTEGER,
    min_duration_seconds INTEGER,
    max_duration_seconds INTEGER,
    keyword_filter TEXT,
    keyword_exclude TEXT,
    created_at INTEGER NOT NULL,
    updated_at INTEGER NOT NULL
);
