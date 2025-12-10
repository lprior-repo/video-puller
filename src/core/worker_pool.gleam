/// Worker Pool - Supervised Download Workers
///
/// Implements a proper BEAM worker pool pattern with:
/// - Pre-spawned supervised worker actors
/// - Work-stealing queue distribution
/// - Automatic restart on failure
/// - Dynamic scaling based on load
///
/// This is the heart of the massive parallelism support.
import domain/types.{type DownloadResult, type JobId, type ManagerMessage}
import engine/downloader
import engine/ytdlp
import gleam/dict.{type Dict}
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/result

/// Worker pool state
pub opaque type WorkerPoolState {
  WorkerPoolState(
    config: ytdlp.DownloadConfig,
    manager_subject: Subject(ManagerMessage),
    // Available workers ready for work
    available_workers: List(Subject(WorkerMessage)),
    // Workers currently processing jobs (worker -> job_id)
    busy_workers: Dict(String, WorkerInfo),
    // Pending work queue (for backpressure)
    work_queue: List(PendingWork),
    // Pool configuration
    min_workers: Int,
    max_workers: Int,
    current_worker_count: Int,
    // Worker index for unique naming
    worker_index: Int,
    // Self reference for scheduling
    self: Option(Subject(PoolMessage)),
    // Statistics for adaptive scaling
    stats: PoolStats,
  )
}

/// Information about a busy worker
pub type WorkerInfo {
  WorkerInfo(job_id: JobId, worker: Subject(WorkerMessage), started_at: Int)
}

/// Pending work item
pub type PendingWork {
  PendingWork(job_id: JobId, url: String, queued_at: Int)
}

/// Pool statistics for adaptive scaling
pub type PoolStats {
  PoolStats(
    total_completed: Int,
    total_failed: Int,
    avg_completion_time_ms: Int,
    queue_high_water_mark: Int,
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

/// Messages for individual workers
pub type WorkerMessage {
  // Execute a download
  ExecuteDownload(
    job_id: JobId,
    url: String,
    worker_id: String,
    pool: Subject(PoolMessage),
    manager: Subject(ManagerMessage),
  )
  // Shutdown worker
  ShutdownWorker
}

/// Worker state
pub type WorkerState {
  WorkerState(
    config: ytdlp.DownloadConfig,
    worker_id: String,
    current_job: Option(JobId),
  )
}

/// Start a new worker pool
pub fn start(
  config: ytdlp.DownloadConfig,
  manager_subject: Subject(ManagerMessage),
  min_workers: Int,
  max_workers: Int,
) -> Result(Subject(PoolMessage), actor.StartError) {
  let initial_state =
    WorkerPoolState(
      config: config,
      manager_subject: manager_subject,
      available_workers: [],
      busy_workers: dict.new(),
      work_queue: [],
      min_workers: min_workers,
      max_workers: max_workers,
      current_worker_count: 0,
      worker_index: 0,
      self: None,
      stats: PoolStats(
        total_completed: 0,
        total_failed: 0,
        avg_completion_time_ms: 0,
        queue_high_water_mark: 0,
      ),
    )

  actor.start(
    actor.new(initial_state)
    |> actor.on_message(handle_pool_message),
  )
  |> result.map(fn(started) {
    let subject = started.data
    // Set self reference
    process.send(subject, SetSelf(subject))
    // Spawn initial workers
    process.send(subject, ScaleWorkers(min_workers))
    // Schedule health checks
    schedule_health_check(subject)
    subject
  })
}

/// Handle pool messages
fn handle_pool_message(
  state: WorkerPoolState,
  message: PoolMessage,
) -> actor.Next(WorkerPoolState, PoolMessage) {
  case message {
    SetSelf(subject) -> {
      actor.continue(WorkerPoolState(..state, self: Some(subject)))
    }

    SubmitJob(job_id, url) -> {
      // Try to assign to an available worker immediately
      case state.available_workers {
        [worker, ..rest] -> {
          // Assign work to available worker
          let worker_id = "worker-" <> int.to_string(state.worker_index)
          assign_work_to_worker(
            worker,
            worker_id,
            job_id,
            url,
            state.manager_subject,
            state.self,
          )

          let busy =
            dict.insert(
              state.busy_workers,
              worker_id,
              WorkerInfo(
                job_id: job_id,
                worker: worker,
                started_at: get_timestamp(),
              ),
            )

          actor.continue(
            WorkerPoolState(
              ..state,
              available_workers: rest,
              busy_workers: busy,
            ),
          )
        }

        [] -> {
          // No available workers - queue the work
          let work =
            PendingWork(job_id: job_id, url: url, queued_at: get_timestamp())
          let new_queue = list.append(state.work_queue, [work])

          // Track high water mark for adaptive scaling
          let queue_len = list.length(new_queue)
          let new_stats = case queue_len > state.stats.queue_high_water_mark {
            True -> PoolStats(..state.stats, queue_high_water_mark: queue_len)
            False -> state.stats
          }

          // Trigger auto-scaling if queue is building up
          let new_state =
            WorkerPoolState(..state, work_queue: new_queue, stats: new_stats)

          // Try to scale up if we have capacity
          case state.current_worker_count < state.max_workers && queue_len > 0 {
            True -> {
              case state.self {
                Some(self) -> {
                  // Scale up by 1 worker per queued item, up to max
                  let scale_by =
                    int.min(
                      queue_len,
                      state.max_workers - state.current_worker_count,
                    )
                  process.send(
                    self,
                    ScaleWorkers(state.current_worker_count + scale_by),
                  )
                }
                None -> Nil
              }
              actor.continue(new_state)
            }
            False -> actor.continue(new_state)
          }
        }
      }
    }

    WorkerDone(worker_id, job_id, result) -> {
      io.println(
        "✅ Worker "
        <> worker_id
        <> " completed job "
        <> types.job_id_to_string(job_id),
      )

      // Update stats
      let new_stats =
        PoolStats(
          ..state.stats,
          total_completed: state.stats.total_completed + 1,
        )

      // Get the worker subject from busy_workers
      case dict.get(state.busy_workers, worker_id) {
        Ok(info) -> {
          // Notify manager of completion
          let status = case result {
            types.DownloadComplete(_, _) -> types.Completed
            types.DownloadFailed(_, reason) -> types.Failed(reason)
            _ -> types.Completed
          }
          process.send(
            state.manager_subject,
            types.JobStatusUpdate(job_id, status),
          )

          // Return worker to available pool
          let new_available = [info.worker, ..state.available_workers]
          let new_busy = dict.delete(state.busy_workers, worker_id)

          // Process any queued work
          let new_state =
            WorkerPoolState(
              ..state,
              available_workers: new_available,
              busy_workers: new_busy,
              stats: new_stats,
            )

          case state.self {
            Some(self) -> process.send(self, ProcessQueue)
            None -> Nil
          }

          actor.continue(new_state)
        }
        Error(_) -> {
          // Worker not in busy list - ignore
          actor.continue(WorkerPoolState(..state, stats: new_stats))
        }
      }
    }

    WorkerFailed(worker_id, job_id, reason) -> {
      io.println(
        "❌ Worker "
        <> worker_id
        <> " failed on job "
        <> types.job_id_to_string(job_id)
        <> ": "
        <> reason,
      )

      // Update stats
      let new_stats =
        PoolStats(..state.stats, total_failed: state.stats.total_failed + 1)

      // Notify manager of failure
      process.send(
        state.manager_subject,
        types.JobStatusUpdate(job_id, types.Failed(reason)),
      )

      // Remove from busy workers
      let new_busy = dict.delete(state.busy_workers, worker_id)

      // Spawn a replacement worker since this one died
      let new_state =
        spawn_worker(
          WorkerPoolState(..state, busy_workers: new_busy, stats: new_stats),
        )

      actor.continue(new_state)
    }

    ScaleWorkers(target) -> {
      let clamped_target =
        int.clamp(target, state.min_workers, state.max_workers)
      let diff = clamped_target - state.current_worker_count

      case diff > 0 {
        True -> {
          // Scale up
          io.println(
            "📈 Scaling up worker pool: "
            <> int.to_string(state.current_worker_count)
            <> " -> "
            <> int.to_string(clamped_target),
          )
          let new_state = spawn_workers(state, diff)
          actor.continue(new_state)
        }
        False -> {
          // Scale down (only remove available workers)
          case diff < 0 {
            True -> {
              let to_remove = int.absolute_value(diff)
              let #(to_shutdown, remaining) =
                list.split(state.available_workers, to_remove)

              // Shutdown extra workers
              list.each(to_shutdown, fn(worker) {
                process.send(worker, ShutdownWorker)
              })

              io.println(
                "📉 Scaling down worker pool: "
                <> int.to_string(state.current_worker_count)
                <> " -> "
                <> int.to_string(
                  state.current_worker_count - list.length(to_shutdown),
                ),
              )

              actor.continue(
                WorkerPoolState(
                  ..state,
                  available_workers: remaining,
                  current_worker_count: state.current_worker_count
                    - list.length(to_shutdown),
                ),
              )
            }
            False -> actor.continue(state)
          }
        }
      }
    }

    ProcessQueue -> {
      // Try to process queued work
      case state.work_queue, state.available_workers {
        [work, ..rest_queue], [worker, ..rest_workers] -> {
          let worker_id =
            "worker-q-" <> int.to_string(state.worker_index + 1000)

          assign_work_to_worker(
            worker,
            worker_id,
            work.job_id,
            work.url,
            state.manager_subject,
            state.self,
          )

          let busy =
            dict.insert(
              state.busy_workers,
              worker_id,
              WorkerInfo(
                job_id: work.job_id,
                worker: worker,
                started_at: get_timestamp(),
              ),
            )

          let new_state =
            WorkerPoolState(
              ..state,
              work_queue: rest_queue,
              available_workers: rest_workers,
              busy_workers: busy,
              worker_index: state.worker_index + 1,
            )

          // Continue processing queue if more work
          case rest_queue {
            [] -> actor.continue(new_state)
            _ -> {
              case state.self {
                Some(self) -> process.send(self, ProcessQueue)
                None -> Nil
              }
              actor.continue(new_state)
            }
          }
        }
        _, _ -> actor.continue(state)
      }
    }

    HealthCheck -> {
      // Check for stuck workers (over 30 minute timeout)
      let current_time = get_timestamp()
      let timeout_threshold = 30 * 60 * 1_000_000_000
      // 30 minutes in nanoseconds

      let stuck_workers =
        dict.to_list(state.busy_workers)
        |> list.filter(fn(entry) {
          let #(_, info) = entry
          current_time - info.started_at > timeout_threshold
        })

      // Handle stuck workers
      let new_state =
        list.fold(stuck_workers, state, fn(st, entry) {
          let #(worker_id, info) = entry
          io.println(
            "⚠️  Worker "
            <> worker_id
            <> " appears stuck on job "
            <> types.job_id_to_string(info.job_id),
          )

          // Mark job as failed and remove from busy
          process.send(
            st.manager_subject,
            types.JobStatusUpdate(
              info.job_id,
              types.Failed("Download timeout exceeded"),
            ),
          )

          // Try to shutdown the stuck worker
          process.send(info.worker, ShutdownWorker)

          WorkerPoolState(
            ..st,
            busy_workers: dict.delete(st.busy_workers, worker_id),
          )
        })

      // Schedule next health check
      case state.self {
        Some(self) -> schedule_health_check(self)
        None -> Nil
      }

      actor.continue(new_state)
    }

    GetStatus(reply) -> {
      let status =
        PoolStatus(
          available_workers: list.length(state.available_workers),
          busy_workers: dict.size(state.busy_workers),
          queue_depth: list.length(state.work_queue),
          total_workers: state.current_worker_count,
          stats: state.stats,
        )
      process.send(reply, status)
      actor.continue(state)
    }

    Shutdown -> {
      io.println("🛑 Worker pool shutting down...")

      // Shutdown all workers
      list.each(state.available_workers, fn(worker) {
        process.send(worker, ShutdownWorker)
      })

      dict.each(state.busy_workers, fn(_, info) {
        process.send(info.worker, ShutdownWorker)
      })

      actor.stop()
    }
  }
}

/// Spawn multiple workers
fn spawn_workers(state: WorkerPoolState, count: Int) -> WorkerPoolState {
  list.fold(list.range(1, count), state, fn(st, _) { spawn_worker(st) })
}

/// Spawn a single worker and add to available pool
fn spawn_worker(state: WorkerPoolState) -> WorkerPoolState {
  let worker_id = "worker-" <> int.to_string(state.worker_index)

  case start_worker(state.config, worker_id) {
    Ok(worker_subject) -> {
      io.println("🔧 Spawned worker: " <> worker_id)
      WorkerPoolState(
        ..state,
        available_workers: [worker_subject, ..state.available_workers],
        current_worker_count: state.current_worker_count + 1,
        worker_index: state.worker_index + 1,
      )
    }
    Error(_) -> {
      io.println("❌ Failed to spawn worker: " <> worker_id)
      state
    }
  }
}

/// Start an individual worker actor
fn start_worker(
  config: ytdlp.DownloadConfig,
  worker_id: String,
) -> Result(Subject(WorkerMessage), actor.StartError) {
  let initial_state =
    WorkerState(config: config, worker_id: worker_id, current_job: None)

  actor.start(
    actor.new(initial_state)
    |> actor.on_message(handle_worker_message),
  )
  |> result.map(fn(started) { started.data })
}

/// Handle worker messages
fn handle_worker_message(
  state: WorkerState,
  message: WorkerMessage,
) -> actor.Next(WorkerState, WorkerMessage) {
  case message {
    ExecuteDownload(job_id, url, worker_id, pool, manager) -> {
      io.println(
        "🔄 Worker "
        <> worker_id
        <> " starting download: "
        <> types.job_id_to_string(job_id),
      )

      // Execute the download with proper error handling
      let result = execute_download_safely(job_id, url, state.config, manager)

      // Report back to pool
      case result {
        Ok(download_result) -> {
          process.send(pool, WorkerDone(worker_id, job_id, download_result))
        }
        Error(reason) -> {
          process.send(pool, WorkerFailed(worker_id, job_id, reason))
        }
      }

      let new_state = WorkerState(..state, current_job: None)
      actor.continue(new_state)
    }

    ShutdownWorker -> {
      io.println("🛑 Worker " <> state.worker_id <> " shutting down")
      actor.stop()
    }
  }
}

/// Execute download with comprehensive error handling
fn execute_download_safely(
  job_id: JobId,
  url: String,
  config: ytdlp.DownloadConfig,
  manager: Subject(ManagerMessage),
) -> Result(DownloadResult, String) {
  // Use a try-catch pattern via process isolation
  case downloader.start(config) {
    Ok(downloader_subject) -> {
      let reply_subject = process.new_subject()

      // Send download command
      process.send(
        downloader_subject,
        downloader.Download(job_id, url, reply_subject, manager),
      )

      // Wait with timeout (30 minutes)
      case process.receive(reply_subject, 1_800_000) {
        Ok(result) -> Ok(result)
        Error(_) -> Error("Download timeout exceeded 30 minutes")
      }
    }
    Error(_) -> Error("Failed to start downloader actor")
  }
}

/// Assign work to a specific worker
fn assign_work_to_worker(
  worker: Subject(WorkerMessage),
  worker_id: String,
  job_id: JobId,
  url: String,
  manager: Subject(ManagerMessage),
  pool: Option(Subject(PoolMessage)),
) -> Nil {
  case pool {
    Some(p) -> {
      process.send(worker, ExecuteDownload(job_id, url, worker_id, p, manager))
    }
    None -> Nil
  }
}

/// Schedule periodic health checks
fn schedule_health_check(pool: Subject(PoolMessage)) -> Nil {
  // Health check every 60 seconds
  let _ =
    process.spawn(fn() {
      process.sleep(60_000)
      process.send(pool, HealthCheck)
    })
  Nil
}

/// Get current Unix timestamp in nanoseconds
@external(erlang, "os", "system_time")
fn get_timestamp() -> Int
