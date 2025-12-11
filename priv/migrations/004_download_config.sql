-- Download configuration settings (singleton)
-- Allows runtime configuration of download behavior through the UI

CREATE TABLE IF NOT EXISTS download_config (
    id INTEGER PRIMARY KEY DEFAULT 1,
    output_directory TEXT NOT NULL DEFAULT './downloads',
    format TEXT NOT NULL DEFAULT 'bestvideo[ext=mp4]+bestaudio[ext=m4a]/best[ext=mp4]/best',
    max_filesize TEXT NOT NULL DEFAULT '2G',
    audio_only INTEGER NOT NULL DEFAULT 0,
    audio_format TEXT NOT NULL DEFAULT 'best',
    allow_playlist INTEGER NOT NULL DEFAULT 0,
    download_timeout_minutes INTEGER NOT NULL DEFAULT 30,
    rate_limit_delay_ms INTEGER NOT NULL DEFAULT 500,
    bandwidth_limit TEXT NOT NULL DEFAULT '',
    use_channel_folders INTEGER NOT NULL DEFAULT 0,
    max_concurrency INTEGER NOT NULL DEFAULT 10,
    created_at INTEGER NOT NULL,
    updated_at INTEGER NOT NULL,
    CHECK(id = 1)
);

-- Insert default config row
INSERT OR IGNORE INTO download_config (
    id,
    output_directory,
    format,
    max_filesize,
    audio_only,
    audio_format,
    allow_playlist,
    download_timeout_minutes,
    rate_limit_delay_ms,
    bandwidth_limit,
    use_channel_folders,
    max_concurrency,
    created_at,
    updated_at
)
VALUES (
    1,
    './downloads',
    'bestvideo[ext=mp4]+bestaudio[ext=m4a]/best[ext=mp4]/best',
    '2G',
    0,
    'best',
    0,
    30,
    500,
    '',
    0,
    10,
    strftime('%s', 'now'),
    strftime('%s', 'now')
);
