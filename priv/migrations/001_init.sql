-- Initial schema for FractalVideoEater
-- Creates the video_jobs table with proper constraints and indexes

-- Enable WAL mode for better concurrency
PRAGMA journal_mode=WAL;
PRAGMA synchronous=NORMAL;

-- Create the main video_jobs table
CREATE TABLE IF NOT EXISTS video_jobs (
    id TEXT PRIMARY KEY NOT NULL,
    url TEXT NOT NULL,
    status TEXT NOT NULL CHECK(status IN ('pending', 'downloading', 'completed', 'failed')),
    progress INTEGER DEFAULT 0 CHECK(progress >= 0 AND progress <= 100),
    path TEXT,
    error_message TEXT,
    created_at INTEGER NOT NULL,
    updated_at INTEGER NOT NULL
);

-- Index on status for efficient polling of pending jobs
CREATE INDEX IF NOT EXISTS idx_video_jobs_status ON video_jobs(status);

-- Index on created_at for sorting and analytics
CREATE INDEX IF NOT EXISTS idx_video_jobs_created_at ON video_jobs(created_at);

-- Index on status + created_at for efficient queue operations
CREATE INDEX IF NOT EXISTS idx_video_jobs_status_created ON video_jobs(status, created_at);
