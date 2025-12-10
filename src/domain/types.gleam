/// Domain types for the FractalVideoEater system
///
/// This module defines the core types used throughout the application,
/// following the schemas defined in the system specification.
import gleam/dynamic.{type Dynamic}
import gleam/erlang/process.{type Subject}
import gleam/option.{type Option}

/// JobId - Unique identifier for a download job
pub type JobId {
  JobId(value: String)
}

/// Format option returned by yt-dlp -F command
pub type FormatOption {
  FormatOption(code: String, ext: String, resolution: String, note: String)
}

/// Video metadata extracted from yt-dlp --dump-json
pub type VideoMetadata {
  VideoMetadata(
    title: String,
    thumbnail: String,
    duration: Int,
    uploader: String,
    view_count: Option(Int),
  )
}

/// Video download status state machine
/// States: pending -> downloading -> completed | failed
pub type VideoStatus {
  Pending
  Downloading(progress: Int)
  Completed
  Failed(reason: String)
}

/// Complete video job record
/// Maps to the database video_jobs table
pub type VideoJob {
  VideoJob(
    id: JobId,
    url: String,
    status: VideoStatus,
    path: Option(String),
    title: Option(String),
    thumbnail_url: Option(String),
    duration_seconds: Option(Int),
    format_code: Option(String),
    created_at: Int,
    updated_at: Int,
  )
}

/// Command message for the downloader actor
pub type DownloadCommand {
  StartDownload(job_id: JobId, url: String)
  CancelDownload(job_id: JobId)
  GetProgress(job_id: JobId)
}

/// Result from download operations
pub type DownloadResult {
  DownloadStarted(job_id: JobId)
  DownloadProgress(job_id: JobId, progress: Int)
  DownloadComplete(job_id: JobId, path: String)
  DownloadFailed(job_id: JobId, reason: String)
}

/// Manager message for the orchestration layer
pub type ManagerMessage {
  PollJobs
  JobStatusUpdate(job_id: JobId, status: VideoStatus)
  UpdateProgress(job_id: JobId, progress: Int)
  Shutdown
  SetSelf(Subject(ManagerMessage))
  // Worker pool integration (opaque type to avoid circular dependency)
  SetWorkerPool(pool: Dynamic)
  // Get manager statistics
  GetStats(reply: Subject(ManagerStats))
}

/// Manager statistics for monitoring
pub type ManagerStats {
  ManagerStats(
    total_dispatched: Int,
    total_completed: Int,
    total_failed: Int,
    polls_executed: Int,
  )
}

/// Configuration for the video downloader
pub type DownloaderConfig {
  DownloaderConfig(
    max_concurrency: Int,
    poll_interval_ms: Int,
    output_directory: String,
  )
}

/// Helper to create a new JobId from a string
pub fn new_job_id(value: String) -> JobId {
  JobId(value)
}

/// Helper to extract the string value from a JobId
pub fn job_id_to_string(job_id: JobId) -> String {
  job_id.value
}

/// Helper to check if a status is terminal (completed or failed)
pub fn is_terminal_status(status: VideoStatus) -> Bool {
  case status {
    Completed | Failed(_) -> True
    Pending | Downloading(_) -> False
  }
}

/// Helper to convert status to string for database storage
pub fn status_to_string(status: VideoStatus) -> String {
  case status {
    Pending -> "pending"
    Downloading(_) -> "downloading"
    Completed -> "completed"
    Failed(_) -> "failed"
  }
}

/// Helper to parse status from string (from database)
pub fn string_to_status(s: String) -> VideoStatus {
  case s {
    "pending" -> Pending
    "downloading" -> Downloading(0)
    "completed" -> Completed
    "failed" -> Failed("Unknown error")
    _ -> Pending
  }
}
