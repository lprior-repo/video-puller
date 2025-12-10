/// Domain types for the FractalVideoEater system
///
/// This module re-exports core types and defines orchestration-specific types.
/// Core types are defined in domain/core_types to break circular dependencies.
import core/pool_types.{type WorkerPoolSubject}
import domain/core_types
import gleam/erlang/process.{type Subject}

// Re-export core types for backwards compatibility
pub type JobId =
  core_types.JobId

pub type FormatOption =
  core_types.FormatOption

pub type VideoMetadata =
  core_types.VideoMetadata

pub type VideoStatus =
  core_types.VideoStatus

pub type VideoJob =
  core_types.VideoJob

pub type DownloadCommand =
  core_types.DownloadCommand

pub type DownloadResult =
  core_types.DownloadResult

pub type ManagerStats =
  core_types.ManagerStats

pub type DownloaderConfig =
  core_types.DownloaderConfig

/// Manager message for the orchestration layer
pub type ManagerMessage {
  PollJobs
  JobStatusUpdate(job_id: core_types.JobId, status: core_types.VideoStatus)
  UpdateProgress(job_id: core_types.JobId, progress: Int)
  Shutdown
  ForceShutdown
  SetSelf(Subject(ManagerMessage))
  // Worker pool integration - properly typed
  SetWorkerPool(pool: WorkerPoolSubject)
  // Get manager statistics
  GetStats(reply: Subject(core_types.ManagerStats))
}

/// Re-export core type constructors and helper functions
pub const new_job_id = core_types.new_job_id

pub const job_id_to_string = core_types.job_id_to_string

pub const is_terminal_status = core_types.is_terminal_status

pub const status_to_string = core_types.status_to_string

pub const string_to_status = core_types.string_to_status
