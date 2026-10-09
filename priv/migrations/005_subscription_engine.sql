-- Subscription engine refresh
--
-- Subscription pulls now run through ytdl-sub against public channel URLs,
-- so browser cookie selection and per-video filters no longer exist.
ALTER TABLE subscription_config DROP COLUMN browser;
ALTER TABLE subscription_config DROP COLUMN cookies_path;
ALTER TABLE subscription_config DROP COLUMN max_age_days;
ALTER TABLE subscription_config DROP COLUMN min_duration_seconds;
ALTER TABLE subscription_config DROP COLUMN max_duration_seconds;
ALTER TABLE subscription_config DROP COLUMN keyword_filter;
ALTER TABLE subscription_config DROP COLUMN keyword_exclude;

-- Legacy default cadence was hourly; the engine defaults to six-hourly
UPDATE subscription_config
SET poll_interval_minutes = 360
WHERE poll_interval_minutes = 60;

DROP TABLE IF EXISTS channel_settings;
DROP INDEX IF EXISTS idx_seen_videos_published;
DROP INDEX IF EXISTS idx_seen_videos_channel;
