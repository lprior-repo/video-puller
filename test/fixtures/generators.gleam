/// Test Fixtures - Type Generators
///
/// F-005: Provides factory functions and random generators for domain types.
/// Use these in tests to create consistent, reproducible test data.
import domain/core_types.{
  type DownloadCommand, type DownloadResult, type DownloaderConfig, type JobId,
  type VideoJob, type VideoStatus,
}
import domain/types.{type ManagerMessage}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}

// ============================================================================
// JobId Generators
// ============================================================================

/// Create a job ID with a specific string value
pub fn job_id(value: String) -> JobId {
  core_types.new_job_id(value)
}

/// Create a job ID with a numeric suffix
pub fn job_id_n(n: Int) -> JobId {
  core_types.new_job_id("test-job-" <> int.to_string(n))
}

/// Create a unique job ID based on counter
pub fn unique_job_id(prefix: String, counter: Int) -> JobId {
  core_types.new_job_id(prefix <> "-" <> int.to_string(counter))
}

// ============================================================================
// VideoStatus Generators
// ============================================================================

/// Create a Pending status
pub fn pending_status() -> VideoStatus {
  core_types.Pending
}

/// Create a Downloading status with given progress
pub fn downloading_status(progress: Int) -> VideoStatus {
  core_types.Downloading(progress)
}

/// Create a Completed status
pub fn completed_status() -> VideoStatus {
  core_types.Completed
}

/// Create a Failed status with given reason
pub fn failed_status(reason: String) -> VideoStatus {
  core_types.Failed(reason)
}

/// Create a Failed status with default error message
pub fn failed_status_default() -> VideoStatus {
  core_types.Failed("Download failed")
}

// ============================================================================
// VideoJob Generators
// ============================================================================

/// Create a minimal VideoJob with default values
pub fn video_job(id: String, url: String) -> VideoJob {
  core_types.VideoJob(
    id: core_types.new_job_id(id),
    url: url,
    status: core_types.Pending,
    path: None,
    title: None,
    thumbnail_url: None,
    duration_seconds: None,
    format_code: None,
    created_at: 1_000_000,
    updated_at: 1_000_000,
  )
}

/// Create a VideoJob with all fields specified
pub fn video_job_full(
  id: String,
  url: String,
  status: VideoStatus,
  path: Option(String),
  created_at: Int,
  updated_at: Int,
) -> VideoJob {
  core_types.VideoJob(
    id: core_types.new_job_id(id),
    url: url,
    status: status,
    path: path,
    title: None,
    thumbnail_url: None,
    duration_seconds: None,
    format_code: None,
    created_at: created_at,
    updated_at: updated_at,
  )
}

/// Create a pending VideoJob
pub fn pending_job(id: String, url: String) -> VideoJob {
  video_job(id, url)
}

/// Create a downloading VideoJob with progress
pub fn downloading_job(id: String, url: String, progress: Int) -> VideoJob {
  core_types.VideoJob(
    id: core_types.new_job_id(id),
    url: url,
    status: core_types.Downloading(progress),
    path: None,
    title: None,
    thumbnail_url: None,
    duration_seconds: None,
    format_code: None,
    created_at: 1_000_000,
    updated_at: 1_000_100,
  )
}

/// Create a completed VideoJob
pub fn completed_job(id: String, url: String, path: String) -> VideoJob {
  core_types.VideoJob(
    id: core_types.new_job_id(id),
    url: url,
    status: core_types.Completed,
    path: Some(path),
    title: None,
    thumbnail_url: None,
    duration_seconds: None,
    format_code: None,
    created_at: 1_000_000,
    updated_at: 1_000_200,
  )
}

/// Create a failed VideoJob
pub fn failed_job(id: String, url: String, reason: String) -> VideoJob {
  core_types.VideoJob(
    id: core_types.new_job_id(id),
    url: url,
    status: core_types.Failed(reason),
    path: None,
    title: None,
    thumbnail_url: None,
    duration_seconds: None,
    format_code: None,
    created_at: 1_000_000,
    updated_at: 1_000_200,
  )
}

// ============================================================================
// DownloadCommand Generators
// ============================================================================

/// Create a StartDownload command
pub fn start_download_cmd(job_id: JobId, url: String) -> DownloadCommand {
  core_types.StartDownload(job_id, url)
}

/// Create a CancelDownload command
pub fn cancel_download_cmd(job_id: JobId) -> DownloadCommand {
  core_types.CancelDownload(job_id)
}

/// Create a GetProgress command
pub fn get_progress_cmd(job_id: JobId) -> DownloadCommand {
  core_types.GetProgress(job_id)
}

// ============================================================================
// DownloadResult Generators
// ============================================================================

/// Create a DownloadStarted result
pub fn download_started(job_id: JobId) -> DownloadResult {
  core_types.DownloadStarted(job_id)
}

/// Create a DownloadProgress result
pub fn download_progress(job_id: JobId, progress: Int) -> DownloadResult {
  core_types.DownloadProgress(job_id, progress)
}

/// Create a DownloadComplete result
pub fn download_complete(job_id: JobId, path: String) -> DownloadResult {
  core_types.DownloadComplete(job_id, path)
}

/// Create a DownloadFailed result
pub fn download_failed(job_id: JobId, reason: String) -> DownloadResult {
  core_types.DownloadFailed(job_id, reason)
}

// ============================================================================
// ManagerMessage Generators
// ============================================================================

/// Create a PollJobs message
pub fn poll_jobs_msg() -> ManagerMessage {
  types.PollJobs
}

/// Create a JobStatusUpdate message
pub fn status_update_msg(job_id: JobId, status: VideoStatus) -> ManagerMessage {
  types.JobStatusUpdate(job_id, status)
}

/// Create a Shutdown message
pub fn shutdown_msg() -> ManagerMessage {
  types.Shutdown
}

// ============================================================================
// DownloaderConfig Generators
// ============================================================================

/// Create a default DownloaderConfig
pub fn default_config() -> DownloaderConfig {
  core_types.DownloaderConfig(
    max_concurrency: 3,
    poll_interval_ms: 1000,
    output_directory: "/tmp/downloads",
  )
}

/// Create a DownloaderConfig with custom values
pub fn downloader_config(
  max_concurrency: Int,
  poll_interval_ms: Int,
  output_directory: String,
) -> DownloaderConfig {
  core_types.DownloaderConfig(
    max_concurrency,
    poll_interval_ms,
    output_directory,
  )
}

// ============================================================================
// URL Generators
// ============================================================================

/// Generate a YouTube URL
pub fn youtube_url(video_id: String) -> String {
  "https://www.youtube.com/watch?v=" <> video_id
}

/// Generate a Vimeo URL
pub fn vimeo_url(video_id: String) -> String {
  "https://vimeo.com/" <> video_id
}

/// Generate a generic test URL
pub fn test_url(n: Int) -> String {
  "https://example.com/video/" <> int.to_string(n)
}

// ============================================================================
// Batch Generators
// ============================================================================

/// Generate multiple pending jobs
pub fn pending_jobs(count: Int) -> List(VideoJob) {
  list.range(1, count)
  |> list.map(fn(n) {
    pending_job("batch-job-" <> int.to_string(n), test_url(n))
  })
}

/// Generate multiple jobs with mixed statuses
pub fn mixed_status_jobs(count: Int) -> List(VideoJob) {
  list.range(1, count)
  |> list.map(fn(n) {
    let id = "mixed-job-" <> int.to_string(n)
    let url = test_url(n)
    case n % 4 {
      0 -> pending_job(id, url)
      1 -> downloading_job(id, url, n * 10)
      2 -> completed_job(id, url, "/downloads/" <> id <> ".mp4")
      _ -> failed_job(id, url, "Error " <> int.to_string(n))
    }
  })
}

// ============================================================================
// Timestamp Generators
// ============================================================================

/// Base timestamp for consistent testing
pub const base_timestamp = 1_700_000_000

/// Generate a timestamp offset from base
pub fn timestamp(offset: Int) -> Int {
  base_timestamp + offset
}

/// Generate sequential timestamps
pub fn timestamps(count: Int) -> List(Int) {
  list.range(0, count - 1)
  |> list.map(fn(n) { base_timestamp + n * 1000 })
}
