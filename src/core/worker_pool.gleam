/// Worker Pool - Supervised Download Workers
///
/// Implements a proper BEAM worker pool pattern with:
/// - Pre-spawned supervised worker actors
/// - Work-stealing queue distribution
/// - Automatic restart on failure
/// - Dynamic scaling based on load
///
/// This is the heart of the massive parallelism support.
import core/pool_types.{
  type PoolMessage, type PoolStats, GetStatus, HealthCheck, PoolStats,
  PoolStatus, ProcessQueue, RateLimitedDispatch, ScaleWorkers, SetSelf, Shutdown,
  SubmitJob, WorkerDone, WorkerDown, WorkerFailed,
}
import domain/core_types.{
  type DownloadResult, type JobId, Completed, DownloadComplete, DownloadFailed,
  Failed, job_id_to_string,
}
import domain/types.{type ManagerMessage, JobStatusUpdate}
import engine/downloader
import engine/ytdlp
import gleam/dict.{type Dict}
import gleam/erlang/process.{type Down, type Monitor, type Pid, type Subject}
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/otp/factory_supervisor
import gleam/otp/supervision
import gleam/result
import simplifile

/// Worker pool state
pub opaque type WorkerPoolState {
  WorkerPoolState(
    config: ytdlp.DownloadConfig,
    manager_subject: Subject(ManagerMessage),
    // Available workers ready for work
    available_workers: List(Worker),
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
    // Factory supervisor is linked to this pool and owns all worker actors.
    factory: Option(
      #(factory_supervisor.Supervisor(WorkerArgs, Subject(WorkerMessage)), Pid),
    ),
    // Statistics for adaptive scaling
    stats: PoolStats,
    // Rate limiting: timestamp of last job dispatch
    last_dispatch_ts: Int,
    runner: fn(JobId, String, ytdlp.DownloadConfig, Subject(ManagerMessage)) ->
      Result(DownloadResult, String),
  )
}

/// Information about a busy worker
pub type WorkerInfo {
  WorkerInfo(
    job_id: JobId,
    worker: Worker,
    started_at_ms: Int,
    timeout_ms: Int,
    timeout_error: Option(String),
  )
}

/// A worker's message subject and owner process, kept together for safe termination.
pub type Worker {
  Worker(subject: Subject(WorkerMessage), pid: Pid, monitor: Monitor)
}

/// Pending work item
pub type PendingWork {
  PendingWork(
    job_id: JobId,
    url: String,
    config: ytdlp.DownloadConfig,
    queued_at: Int,
  )
}

// PoolStats, PoolMessage, and PoolStatus are now defined in core/pool_types.gleam
// to break the circular dependency with domain/types.gleam

/// Messages for individual workers
pub type WorkerMessage {
  // Execute a download using the submission's immutable settings snapshot.
  ExecuteDownload(
    job_id: JobId,
    url: String,
    config: ytdlp.DownloadConfig,
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
    runner: fn(JobId, String, ytdlp.DownloadConfig, Subject(ManagerMessage)) ->
      Result(DownloadResult, String),
  )
}

type WorkerArgs {
  WorkerArgs(
    config: ytdlp.DownloadConfig,
    worker_id: String,
    runner: fn(JobId, String, ytdlp.DownloadConfig, Subject(ManagerMessage)) ->
      Result(DownloadResult, String),
  )
}

/// Start a new worker pool
pub fn start(
  config: ytdlp.DownloadConfig,
  manager_subject: Subject(ManagerMessage),
  min_workers: Int,
  max_workers: Int,
) -> Result(Subject(PoolMessage), actor.StartError) {
  start_with_runner(
    config,
    manager_subject,
    min_workers,
    max_workers,
    fn(job_id, url, job_config, manager) {
      execute_download_safely(job_id, url, job_config, manager)
    },
  )
}

/// Start a worker pool with an injectable executor for deterministic recovery tests.
pub fn start_with_runner(
  config: ytdlp.DownloadConfig,
  manager_subject: Subject(ManagerMessage),
  min_workers: Int,
  max_workers: Int,
  runner: fn(JobId, String, ytdlp.DownloadConfig, Subject(ManagerMessage)) ->
    Result(DownloadResult, String),
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
      factory: None,
      stats: PoolStats(
        total_completed: 0,
        total_failed: 0,
        avg_completion_time_ms: 0,
        queue_high_water_mark: 0,
      ),
      // A first job is immediately eligible even if the monotonic epoch is
      // negative or otherwise unrelated to zero.
      last_dispatch_ts: get_monotonic_time_ms() - config.rate_limit_delay_ms,
      runner: runner,
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
      case
        factory_supervisor.worker_child(start_factory_worker)
        |> factory_supervisor.restart_strategy(supervision.Temporary)
        |> factory_supervisor.start
      {
        Ok(started) ->
          pool_continue(
            WorkerPoolState(
              ..state,
              self: Some(subject),
              factory: Some(#(started.data, started.pid)),
            ),
          )
        Error(_) ->
          actor.stop_abnormal("Failed to start worker factory supervisor")
      }
    }

    WorkerDown(down) -> handle_worker_down(state, down)

    SubmitJob(job_id, url, config) -> {
      // Check rate limiting - convert to milliseconds for comparison
      let current_time_ms = get_monotonic_time_ms()
      let rate_limit_ms = state.config.rate_limit_delay_ms
      let time_since_last = current_time_ms - state.last_dispatch_ts

      // Check if we can dispatch immediately (rate limit allows and worker available)
      case state.available_workers, time_since_last >= rate_limit_ms {
        [worker, ..rest], True -> {
          // Rate limit allows and worker available - dispatch immediately
          let worker_id = "worker-" <> int.to_string(state.worker_index)
          let worker_index = state.worker_index + 1
          assign_work_to_worker(
            worker.subject,
            worker_id,
            job_id,
            url,
            config,
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
                started_at_ms: get_monotonic_time_ms(),
                timeout_ms: config.download_timeout_ms,
                timeout_error: None,
              ),
            )

          pool_continue(
            WorkerPoolState(
              ..state,
              available_workers: rest,
              busy_workers: busy,
              worker_index: worker_index,
              last_dispatch_ts: current_time_ms,
            ),
          )
        }

        [_worker, ..], False -> {
          // Worker available but rate limit prevents dispatch - queue and schedule delayed dispatch
          let work =
            PendingWork(
              job_id: job_id,
              url: url,
              config: config,
              queued_at: get_monotonic_time_ms() / 1000,
            )
          let new_queue = list.append(state.work_queue, [work])
          let delay_needed = rate_limit_ms - time_since_last

          // Schedule rate-limited dispatch
          case state.self {
            Some(self) -> {
              schedule_rate_limited_dispatch(self, delay_needed)
            }
            None -> Nil
          }

          pool_continue(WorkerPoolState(..state, work_queue: new_queue))
        }

        [], _ -> {
          // No available workers - queue the work
          let work =
            PendingWork(
              job_id: job_id,
              url: url,
              config: config,
              queued_at: get_monotonic_time_ms() / 1000,
            )
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
              pool_continue(new_state)
            }
            False -> pool_continue(new_state)
          }
        }
      }
    }

    WorkerDone(worker_id, job_id, result) -> {
      io.println(
        "✅ Worker "
        <> worker_id
        <> " completed job "
        <> job_id_to_string(job_id),
      )

      // Get the worker subject from busy_workers
      case dict.get(state.busy_workers, worker_id) {
        Ok(info) if info.job_id == job_id && info.timeout_error == None -> {
          // Verify the download result and check file existence
          let #(status, path, stats_update) = case result {
            DownloadComplete(_, reported_path) -> {
              // Verify the exact path reported by yt-dlp. Do not reconstruct a
              // filename from the job id: templates may use title/channel/date.
              case verify_file_exists(reported_path) {
                Ok(actual_path) -> {
                  io.println("✓ File verified at: " <> actual_path)
                  #(
                    Completed,
                    Some(actual_path),
                    PoolStats(
                      ..state.stats,
                      total_completed: state.stats.total_completed + 1,
                    ),
                  )
                }
                Error(reason) -> {
                  io.println("⚠️  File verification failed: " <> reason)
                  #(
                    Failed(reason),
                    None,
                    PoolStats(
                      ..state.stats,
                      total_failed: state.stats.total_failed + 1,
                    ),
                  )
                }
              }
            }
            DownloadFailed(_, reason) -> {
              #(
                Failed(reason),
                None,
                PoolStats(
                  ..state.stats,
                  total_failed: state.stats.total_failed + 1,
                ),
              )
            }
            _ -> {
              // Unknown result type - mark as completed but don't update stats.
              #(Completed, None, state.stats)
            }
          }

          // Notify manager of final status and exact media path.
          process.send(
            state.manager_subject,
            JobStatusUpdate(job_id, status, path),
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
              stats: stats_update,
            )

          case state.self {
            Some(self) -> process.send(self, ProcessQueue)
            None -> Nil
          }

          pool_continue(new_state)
        }
        _ -> {
          // Worker not in busy list - ignore
          pool_continue(state)
        }
      }
    }

    WorkerFailed(worker_id, job_id, reason) -> {
      io.println(
        "❌ Worker "
        <> worker_id
        <> " failed on job "
        <> job_id_to_string(job_id)
        <> ": "
        <> reason,
      )

      case dict.get(state.busy_workers, worker_id) {
        Error(_) -> pool_continue(state)
        Ok(info) if info.job_id != job_id -> pool_continue(state)
        Ok(info) if info.timeout_error == None -> {
          // A failed download does not mean the worker actor failed. It has
          // finished its request and is healthy, so keep the same process.
          let new_stats =
            PoolStats(..state.stats, total_failed: state.stats.total_failed + 1)
          process.send(
            state.manager_subject,
            JobStatusUpdate(job_id, Failed(reason), None),
          )
          let new_state =
            WorkerPoolState(
              ..state,
              available_workers: [info.worker, ..state.available_workers],
              busy_workers: dict.delete(state.busy_workers, worker_id),
              stats: new_stats,
            )
          case state.self {
            Some(self) -> process.send(self, ProcessQueue)
            None -> Nil
          }
          pool_continue(new_state)
        }
        _ -> pool_continue(state)
      }
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
          pool_continue(new_state)
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
                // Retirement is intentional; cancel the monitor before the
                // worker exits so it cannot be mistaken for a crash.
                process.demonitor_process(worker.monitor)
                process.send(worker.subject, ShutdownWorker)
              })

              io.println(
                "📉 Scaling down worker pool: "
                <> int.to_string(state.current_worker_count)
                <> " -> "
                <> int.to_string(
                  state.current_worker_count - list.length(to_shutdown),
                ),
              )

              pool_continue(
                WorkerPoolState(
                  ..state,
                  available_workers: remaining,
                  current_worker_count: state.current_worker_count
                    - list.length(to_shutdown),
                ),
              )
            }
            False -> pool_continue(state)
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
            worker.subject,
            worker_id,
            work.job_id,
            work.url,
            work.config,
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
                started_at_ms: get_monotonic_time_ms(),
                timeout_ms: work.config.download_timeout_ms,
                timeout_error: None,
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
            [] -> pool_continue(new_state)
            _ -> {
              case state.self {
                Some(self) -> process.send(self, ProcessQueue)
                None -> Nil
              }
              pool_continue(new_state)
            }
          }
        }
        _, _ -> pool_continue(state)
      }
    }

    RateLimitedDispatch -> {
      // Handle rate-limited dispatch - try to dispatch queued work respecting rate limits
      let current_time_ms = get_monotonic_time_ms()
      let rate_limit_ms = state.config.rate_limit_delay_ms
      let time_since_last = current_time_ms - state.last_dispatch_ts

      case
        state.work_queue,
        state.available_workers,
        time_since_last >= rate_limit_ms
      {
        [work, ..rest_queue], [worker, ..rest_workers], True -> {
          // Rate limit allows and worker available - dispatch
          let worker_id =
            "worker-rl-" <> int.to_string(state.worker_index + 2000)

          assign_work_to_worker(
            worker.subject,
            worker_id,
            work.job_id,
            work.url,
            work.config,
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
                started_at_ms: get_monotonic_time_ms(),
                timeout_ms: work.config.download_timeout_ms,
                timeout_error: None,
              ),
            )

          let new_state =
            WorkerPoolState(
              ..state,
              work_queue: rest_queue,
              available_workers: rest_workers,
              busy_workers: busy,
              worker_index: state.worker_index + 1,
              last_dispatch_ts: current_time_ms,
            )

          // Schedule next rate-limited dispatch if more work
          case rest_queue {
            [] -> pool_continue(new_state)
            _ -> {
              case state.self {
                Some(self) ->
                  schedule_rate_limited_dispatch(self, rate_limit_ms)
                None -> Nil
              }
              pool_continue(new_state)
            }
          }
        }

        [_, ..], _, False -> {
          // Rate limit not yet satisfied - reschedule
          let delay_needed = rate_limit_ms - time_since_last
          case state.self {
            Some(self) -> schedule_rate_limited_dispatch(self, delay_needed)
            None -> Nil
          }
          pool_continue(state)
        }

        _, _, _ -> {
          // No work or no workers - nothing to do
          pool_continue(state)
        }
      }
    }

    HealthCheck -> {
      // Mark timeout exactly once. The monitor DOWN handler owns settlement
      // and replacement, so the worker slot remains occupied until it exits.
      let current_time_ms = get_monotonic_time_ms()
      let new_state =
        dict.to_list(state.busy_workers)
        |> list.fold(state, fn(st, entry) {
          let #(worker_id, info) = entry
          case
            info.timeout_error == None
            && current_time_ms - info.started_at_ms >= info.timeout_ms
          {
            True -> {
              let reason =
                "Download timeout exceeded "
                <> int.to_string(info.timeout_ms / 60_000)
                <> " minutes"
              io.println(
                "⚠️  Worker "
                <> worker_id
                <> " timed out on job "
                <> job_id_to_string(info.job_id),
              )
              // Confirm the old execution subtree and external command tree
              // are stopped before the DOWN handler releases its capacity.
              case st.factory {
                Some(#(_, factory_pid)) ->
                  stop_owned_worker(info.worker.pid, factory_pid)
                None -> process.kill(info.worker.pid)
              }
              WorkerPoolState(
                ..st,
                busy_workers: dict.insert(
                  st.busy_workers,
                  worker_id,
                  WorkerInfo(..info, timeout_error: Some(reason)),
                ),
              )
            }
            False -> st
          }
        })

      case new_state.self {
        Some(self) -> {
          schedule_health_check(self)
          case new_state.work_queue {
            [] -> Nil
            _ -> process.send(self, ProcessQueue)
          }
        }
        None -> Nil
      }

      pool_continue(new_state)
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
      pool_continue(state)
    }

    Shutdown -> {
      io.println("🛑 Worker pool shutting down...")

      // The factory owns the workers and shuts them down with itself. Killing
      // it here also covers a normal pool stop (normal link exits do not
      // propagate to linked processes).
      case state.factory {
        Some(#(_, factory_pid)) -> process.kill(factory_pid)
        None -> Nil
      }

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

  case state.factory {
    Some(#(factory, _)) ->
      case
        factory_supervisor.start_child(
          factory,
          WorkerArgs(state.config, worker_id, state.runner),
        )
      {
        Ok(started) -> {
          let worker =
            Worker(
              subject: started.data,
              pid: started.pid,
              monitor: process.monitor(started.pid),
            )
          io.println("🔧 Spawned worker: " <> worker_id)
          WorkerPoolState(
            ..state,
            available_workers: [worker, ..state.available_workers],
            current_worker_count: state.current_worker_count + 1,
            worker_index: state.worker_index + 1,
          )
        }
        Error(_) -> {
          io.println("❌ Failed to spawn worker: " <> worker_id)
          state
        }
      }
    None -> state
  }
}

fn start_factory_worker(
  args: WorkerArgs,
) -> actor.StartResult(Subject(WorkerMessage)) {
  let WorkerArgs(config, worker_id, runner) = args
  let initial_state =
    WorkerState(config:, worker_id:, current_job: None, runner:)

  actor.start(
    actor.new(initial_state)
    |> actor.on_message(handle_worker_message),
  )
}

/// Build the pool selector after each state transition. Actor continuations
/// replace selectors, so every live worker monitor must be selected again.
fn pool_continue(
  state: WorkerPoolState,
) -> actor.Next(WorkerPoolState, PoolMessage) {
  let next = actor.continue(state)
  case state.self {
    Some(self) ->
      actor.with_selector(
        next,
        process.new_selector()
          |> process.select(self)
          |> process.select_monitors(fn(down) { WorkerDown(down) }),
      )
    None -> next
  }
}

fn handle_worker_down(
  state: WorkerPoolState,
  down: Down,
) -> actor.Next(WorkerPoolState, PoolMessage) {
  case down {
    process.ProcessDown(_, pid, _) -> {
      let available =
        list.filter(state.available_workers, fn(worker) { worker.pid != pid })
      let removed_available =
        list.length(available) < list.length(state.available_workers)
      let busy_match =
        dict.to_list(state.busy_workers)
        |> list.fold(None, fn(found, entry) {
          case found {
            Some(_) -> found
            None -> {
              let #(worker_id, info) = entry
              case info.worker.pid == pid {
                True -> Some(#(worker_id, info))
                False -> None
              }
            }
          }
        })

      case removed_available, busy_match {
        False, None -> pool_continue(state)
        _, _ -> {
          let next_state = case busy_match {
            Some(#(worker_id, info)) -> {
              let reason = case info.timeout_error {
                Some(timeout_reason) -> timeout_reason
                None -> "Worker process exited unexpectedly"
              }
              process.send(
                state.manager_subject,
                JobStatusUpdate(info.job_id, Failed(reason), None),
              )
              WorkerPoolState(
                ..state,
                available_workers: available,
                busy_workers: dict.delete(state.busy_workers, worker_id),
                current_worker_count: int.max(state.current_worker_count - 1, 0),
                stats: PoolStats(
                  ..state.stats,
                  total_failed: state.stats.total_failed + 1,
                ),
              )
            }
            None ->
              WorkerPoolState(
                ..state,
                available_workers: available,
                current_worker_count: int.max(state.current_worker_count - 1, 0),
              )
          }
          let replaced = spawn_worker(next_state)
          case replaced.self {
            Some(self) -> process.send(self, ProcessQueue)
            None -> Nil
          }
          pool_continue(replaced)
        }
      }
    }
    process.PortDown(_, _, _) -> pool_continue(state)
  }
}

/// Handle worker messages
fn handle_worker_message(
  state: WorkerState,
  message: WorkerMessage,
) -> actor.Next(WorkerState, WorkerMessage) {
  case message {
    ExecuteDownload(job_id, url, config, worker_id, pool, manager) -> {
      io.println(
        "🔄 Worker "
        <> worker_id
        <> " starting download: "
        <> job_id_to_string(job_id),
      )

      // Execute the download with the submission's immutable config snapshot.
      let result = state.runner(job_id, url, config, manager)

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

      // Wait with configurable timeout
      case process.receive(reply_subject, config.download_timeout_ms) {
        Ok(result) -> {
          process.send(downloader_subject, downloader.Shutdown)
          Ok(result)
        }
        Error(_) -> {
          // The download actor owns the external command port. Kill that
          // owner on timeout so its port guardian terminates the command tree.
          stop_downloader(downloader_subject)
          Error(
            "Download timeout exceeded "
            <> int.to_string(config.download_timeout_ms / 60_000)
            <> " minutes",
          )
        }
      }
    }
    Error(_) -> Error("Failed to start downloader actor")
  }
}

fn stop_downloader(subject: Subject(downloader.DownloaderMessage)) -> Nil {
  case process.subject_owner(subject) {
    Ok(pid) -> {
      // The downloader was started linked to this worker. Unlink before the
      // untrappable kill so its `killed` exit cannot take the worker with it.
      process.unlink(pid)
      process.kill(pid)
    }
    Error(_) -> Nil
  }
}

@external(erlang, "subscription_manager_ffi", "stop_worker_owned")
fn stop_owned_worker(worker: Pid, owner: Pid) -> Nil

/// Assign work to a specific worker
fn assign_work_to_worker(
  worker: Subject(WorkerMessage),
  worker_id: String,
  job_id: JobId,
  url: String,
  config: ytdlp.DownloadConfig,
  manager: Subject(ManagerMessage),
  pool: Option(Subject(PoolMessage)),
) -> Nil {
  case pool {
    Some(p) -> {
      // Notify manager that job is now downloading
      process.send(
        manager,
        JobStatusUpdate(job_id, core_types.Downloading(0), None),
      )
      process.send(
        worker,
        ExecuteDownload(job_id, url, config, worker_id, p, manager),
      )
    }
    None -> Nil
  }
}

/// Schedule periodic health checks
fn schedule_health_check(pool: Subject(PoolMessage)) -> Nil {
  // A native timer does not allocate a sleeping helper process.
  process.send_after(pool, 60_000, HealthCheck)
  Nil
}

/// Verify that the exact path reported by yt-dlp points to a regular file.
fn verify_file_exists(path: String) -> Result(String, String) {
  case simplifile.is_file(path) {
    Ok(True) -> Ok(path)
    _ -> Error("Download reported success but output file is missing: " <> path)
  }
}

/// Monotonic time is used for elapsed-time calculations, avoiding wall-clock
/// jumps changing rate limiting or download deadlines.
type TimeUnit {
  Millisecond
}

@external(erlang, "erlang", "monotonic_time")
fn get_monotonic_time(unit: TimeUnit) -> Int

fn get_monotonic_time_ms() -> Int {
  get_monotonic_time(Millisecond)
}

/// Schedule rate-limited dispatch after a delay
fn schedule_rate_limited_dispatch(
  pool: Subject(PoolMessage),
  delay_ms: Int,
) -> Nil {
  // Use BEAM's native timer rather than a sleeping helper process.
  process.send_after(pool, delay_ms, RateLimitedDispatch)
  Nil
}
