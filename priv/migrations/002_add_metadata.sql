-- Add video metadata columns
-- Stores title, thumbnail URL, duration, and format code from yt-dlp info

ALTER TABLE video_jobs ADD COLUMN title TEXT;
ALTER TABLE video_jobs ADD COLUMN thumbnail_url TEXT;
ALTER TABLE video_jobs ADD COLUMN duration_seconds INTEGER;
ALTER TABLE video_jobs ADD COLUMN format_code TEXT;
