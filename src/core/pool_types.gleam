/// Worker Pool Types - Defined separately to break circular dependencies
///
/// This module defines the PoolMessage type and related types that need to be
/// shared between worker_pool.gleam and types.gleam without creating import cycles.
import domain/core_types.{type DownloadResult, type JobId}
import gleam/erlang/process.{type Subject}

/// Pool statistics for adaptive scaling
pub type PoolStats {
  PoolStats(
    total_completed: Int,
    total_failed: Int,
    avg_completion_time_ms: Int,
    queue_high_water_mark: Int,
  )
}

/// Pool status for monitoring
pub type PoolStatus {
  PoolStatus(
    available_workers: Int,
    busy_workers: Int,
    queue_depth: Int,
    total_workers: Int,
    stats: PoolStats,
  )
}

/// Messages the pool can receive
pub type PoolMessage {
  // Submit a download job to the pool
  SubmitJob(job_id: JobId, url: String)
  // Worker completed a job
  WorkerDone(worker_id: String, job_id: JobId, result: DownloadResult)
  // Worker failed/crashed
  WorkerFailed(worker_id: String, job_id: JobId, reason: String)
  // Scale workers up/down
  ScaleWorkers(target: Int)
  // Get pool status
  GetStatus(reply: Subject(PoolStatus))
  // Process work queue (internal)
  ProcessQueue
  // Periodic health check
  HealthCheck
  // Set self reference
  SetSelf(Subject(PoolMessage))
  // Shutdown gracefully
  Shutdown
}

/// Type alias for a worker pool subject
pub type WorkerPoolSubject =
  Subject(PoolMessage)
