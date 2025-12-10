/// Manager Actor - Job Queue Orchestrator (BEAM-Optimized)
///
/// Polls the database for pending jobs and dispatches them to the worker pool.
/// Implements the core orchestration logic with:
/// - Worker pool integration for supervised downloads
/// - BEAM-native scheduling (no infinite recursion)
/// - Backpressure awareness
/// - Graceful degradation under load
/// - Graceful shutdown with configurable timeout
import core/pool_types.{type WorkerPoolSubject}
import core/worker_pool
import domain/core_types
import domain/types.{
  type JobId, type ManagerMessage, ForceShutdown, SetSelf, SetWorkerPool,
}
import engine/ytdlp
import gleam/dict.{type Dict}
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/result
import infra/db.{type Db}
import infra/repo

/// Manager state (opaque - internal structure not exported)
pub opaque type ManagerState {
  ManagerState(
    db: Db,
    config: ytdlp.DownloadConfig,
    poll_interval_ms: Int,
    // Active downloads tracked by job_id
    active_downloads: Dict(String, ActiveDownload),
    max_concurrency: Int,
    // Worker pool for supervised download execution - properly typed
    worker_pool: Option(WorkerPoolSubject),
    // Self reference for scheduling
    self: Option(Subject(ManagerMessage)),
    // Statistics for monitoring
    stats: core_types.ManagerStats,
    // Graceful shutdown support
    is_shutting_down: Bool,
    shutdown_timeout_ms: Int,
  )
}

/// Active download tracking
pub type ActiveDownload {
  ActiveDownload(job_id: JobId, url: String, started_at: Int)
}

/// Start the manager actor
pub fn start(
  db: Db,
  config: ytdlp.DownloadConfig,
  poll_interval_ms: Int,
  max_concurrency: Int,
) -> Result(Subject(ManagerMessage), actor.StartError) {
  let state =
    ManagerState(
      db: db,
      config: config,
      poll_interval_ms: poll_interval_ms,
      active_downloads: dict.new(),
      max_concurrency: max_concurrency,
      worker_pool: None,
      self: None,
      stats: core_types.ManagerStats(
        total_dispatched: 0,
        total_completed: 0,
        total_failed: 0,
        polls_executed: 0,
      ),
      is_shutting_down: False,
      shutdown_timeout_ms: 30_000,
    )

  actor.start(
    actor.new(state)
    |> actor.on_message(handle_message),
  )
  |> result.map(fn(started) {
    let subject = started.data

    // Set self reference
    process.send(subject, SetSelf(subject))

    // Initialize worker pool with higher capacity for BEAM
    let min_workers = int.max(5, max_concurrency / 2)
    let max_workers = int.max(50, max_concurrency * 5)

    case worker_pool.start(config, subject, min_workers, max_workers) {
      Ok(pool) -> {
        io.println(
          "✅ Worker pool initialized (min="
          <> int.to_string(min_workers)
          <> ", max="
          <> int.to_string(max_workers)
          <> ")",
        )
        // Properly typed - no dynamic coercion needed
        process.send(subject, SetWorkerPool(pool))
      }
      Error(_) -> {
        io.println("⚠️  Failed to start worker pool, using fallback mode")
      }
    }

    // Schedule first poll using BEAM scheduler
    schedule_poll(subject, poll_interval_ms)

    subject
  })
}

/// Handle manager messages
fn handle_message(
  state: ManagerState,
  message: ManagerMessage,
) -> actor.Next(ManagerState, ManagerMessage) {
  case message {
    types.PollJobs -> {
      let new_state = poll_and_dispatch(state)

      // Schedule next poll using BEAM's native scheduler (no recursion!)
      case state.self {
        Some(self) -> schedule_poll(self, state.poll_interval_ms)
        None -> Nil
      }

      let updated_stats =
        core_types.ManagerStats(
          ..new_state.stats,
          polls_executed: new_state.stats.polls_executed + 1,
        )

      actor.continue(ManagerState(..new_state, stats: updated_stats))
    }

    types.JobStatusUpdate(job_id, status) -> {
      let timestamp = get_timestamp()
      let _ = repo.update_status(state.db, job_id, status, timestamp)

      let job_id_str = types.job_id_to_string(job_id)

      // Update statistics and remove from active if terminal
      let #(new_active, new_stats) = case types.is_terminal_status(status) {
        True -> {
          let updated_active = dict.delete(state.active_downloads, job_id_str)
          let updated_stats = case status {
            core_types.Completed ->
              core_types.ManagerStats(
                ..state.stats,
                total_completed: state.stats.total_completed + 1,
              )
            core_types.Failed(_) ->
              core_types.ManagerStats(
                ..state.stats,
                total_failed: state.stats.total_failed + 1,
              )
            _ -> state.stats
          }
          #(updated_active, updated_stats)
        }
        False -> #(state.active_downloads, state.stats)
      }

      let new_state =
        ManagerState(..state, active_downloads: new_active, stats: new_stats)

      // If shutting down and all downloads complete, shutdown now
      case new_state.is_shutting_down, dict.size(new_state.active_downloads) {
        True, 0 -> {
          io.println(
            "✅ All in-flight downloads complete, shutting down gracefully",
          )
          shutdown_immediately(new_state)
          actor.stop()
        }
        True, n -> {
          io.println(
            "⏳ Still waiting for "
            <> int.to_string(n)
            <> " download(s) to complete...",
          )
          actor.continue(new_state)
        }
        False, _ -> actor.continue(new_state)
      }
    }

    types.UpdateProgress(job_id, progress) -> {
      let timestamp = get_timestamp()
      let _ =
        repo.update_status(
          state.db,
          job_id,
          core_types.Downloading(progress),
          timestamp,
        )
      actor.continue(state)
    }

    types.Shutdown -> {
      io.println("🛑 Manager initiating graceful shutdown...")

      // Mark as shutting down to stop accepting new jobs
      let new_state = ManagerState(..state, is_shutting_down: True)

      // Check if we have in-flight downloads
      let active_count = dict.size(new_state.active_downloads)

      case active_count {
        0 -> {
          // No in-flight downloads, shutdown immediately
          io.println("✅ No in-flight downloads, shutting down immediately")
          shutdown_immediately(new_state)
          actor.stop()
        }
        n -> {
          // Wait for in-flight downloads to complete
          io.println(
            "⏳ Waiting for "
            <> int.to_string(n)
            <> " in-flight download(s) to complete...",
          )

          // Schedule a force shutdown after timeout
          case new_state.self {
            Some(self) -> {
              schedule_force_shutdown(self, new_state.shutdown_timeout_ms)
            }
            None -> Nil
          }

          // Continue running but don't accept new jobs
          actor.continue(new_state)
        }
      }
    }

    ForceShutdown -> {
      io.println("⚠️  Force shutdown triggered after timeout")
      let active_count = dict.size(state.active_downloads)

      case active_count > 0 {
        True -> {
          io.println(
            "⚠️  Forcing shutdown with "
            <> int.to_string(active_count)
            <> " download(s) still in progress",
          )
        }
        False -> Nil
      }

      shutdown_immediately(state)
      actor.stop()
    }

    SetSelf(subject) -> {
      actor.continue(ManagerState(..state, self: Some(subject)))
    }

    SetWorkerPool(pool) -> {
      // Pool is now properly typed - no coercion needed
      io.println("📦 Worker pool registered with manager")
      actor.continue(ManagerState(..state, worker_pool: Some(pool)))
    }

    types.GetStats(reply) -> {
      process.send(reply, state.stats)
      actor.continue(state)
    }
  }
}

/// Poll database and dispatch pending jobs to worker pool
fn poll_and_dispatch(state: ManagerState) -> ManagerState {
  // Don't accept new jobs if shutting down
  case state.is_shutting_down {
    True -> state
    False -> {
      // Check if we can accept more jobs
      let active_count = dict.size(state.active_downloads)
      let available_slots = state.max_concurrency - active_count

      case available_slots > 0, state.worker_pool {
        False, _ -> state
        _, None -> {
          // No worker pool - use fallback dispatch
          fallback_dispatch(state, available_slots)
        }
        True, Some(pool) -> {
          // Get pending jobs
          case repo.list_jobs(state.db, Some("pending")) {
            Ok(jobs) -> {
              let jobs_to_start =
                jobs
                |> list.take(available_slots)

              // Submit jobs to worker pool
              let #(new_active, dispatched_count) =
                list.fold(
                  jobs_to_start,
                  #(state.active_downloads, 0),
                  fn(acc, job) {
                    let #(active, count) = acc

                    // Submit to worker pool
                    process.send(pool, pool_types.SubmitJob(job.id, job.url))

                    io.println(
                      "📤 Submitted to pool: "
                      <> types.job_id_to_string(job.id)
                      <> " - "
                      <> job.url,
                    )

                    // Track as active
                    let job_id_str = types.job_id_to_string(job.id)
                    let download =
                      ActiveDownload(
                        job_id: job.id,
                        url: job.url,
                        started_at: get_timestamp(),
                      )
                    #(dict.insert(active, job_id_str, download), count + 1)
                  },
                )

              let new_stats =
                core_types.ManagerStats(
                  ..state.stats,
                  total_dispatched: state.stats.total_dispatched
                    + dispatched_count,
                )

              ManagerState(
                ..state,
                active_downloads: new_active,
                stats: new_stats,
              )
            }
            Error(_) -> state
          }
        }
      }
    }
  }
}

/// Fallback dispatch when worker pool isn't available
/// Uses process.spawn() but should rarely be needed
fn fallback_dispatch(state: ManagerState, available_slots: Int) -> ManagerState {
  case state.self {
    None -> state
    Some(self) -> {
      case repo.list_jobs(state.db, Some("pending")) {
        Ok(jobs) -> {
          let jobs_to_start =
            jobs
            |> list.take(available_slots)

          let #(new_active, dispatched_count) =
            list.fold(jobs_to_start, #(state.active_downloads, 0), fn(acc, job) {
              let #(active, count) = acc

              // Spawn process directly (fallback mode)
              let _ =
                process.spawn(fn() {
                  run_download_fallback(job.id, job.url, state.config, self)
                })

              io.println(
                "⚠️  Fallback spawn: "
                <> types.job_id_to_string(job.id)
                <> " - "
                <> job.url,
              )

              let job_id_str = types.job_id_to_string(job.id)
              let download =
                ActiveDownload(
                  job_id: job.id,
                  url: job.url,
                  started_at: get_timestamp(),
                )
              #(dict.insert(active, job_id_str, download), count + 1)
            })

          let new_stats =
            core_types.ManagerStats(
              ..state.stats,
              total_dispatched: state.stats.total_dispatched + dispatched_count,
            )

          ManagerState(..state, active_downloads: new_active, stats: new_stats)
        }
        Error(_) -> state
      }
    }
  }
}

/// Fallback download execution (when worker pool isn't available)
fn run_download_fallback(
  job_id: JobId,
  url: String,
  config: ytdlp.DownloadConfig,
  manager_subject: Subject(ManagerMessage),
) -> Nil {
  // Import downloader only for fallback
  case start_downloader(config) {
    Ok(downloader_subject) -> {
      let reply_subject = process.new_subject()

      process.send(
        downloader_subject,
        make_download_message(job_id, url, reply_subject, manager_subject),
      )

      case process.receive(reply_subject, 1_800_000) {
        Ok(result) -> {
          let status = result_to_status(result)
          process.send(manager_subject, types.JobStatusUpdate(job_id, status))
        }
        Error(_) -> {
          process.send(
            manager_subject,
            types.JobStatusUpdate(
              job_id,
              core_types.Failed("Download timeout exceeded"),
            ),
          )
        }
      }
    }
    Error(_) -> {
      process.send(
        manager_subject,
        types.JobStatusUpdate(
          job_id,
          core_types.Failed("Failed to start downloader"),
        ),
      )
    }
  }
}

/// Schedule next poll using BEAM's native timer
/// This is the KEY improvement over infinite recursion!
fn schedule_poll(manager: Subject(ManagerMessage), interval_ms: Int) -> Nil {
  process.send_after(manager, interval_ms, types.PollJobs)
  Nil
}

/// Schedule force shutdown after timeout
fn schedule_force_shutdown(
  manager: Subject(ManagerMessage),
  timeout_ms: Int,
) -> Nil {
  process.send_after(manager, timeout_ms, ForceShutdown)
  Nil
}

/// Shutdown immediately by stopping worker pool
fn shutdown_immediately(state: ManagerState) -> Nil {
  // Shutdown worker pool gracefully
  case state.worker_pool {
    Some(pool) -> process.send(pool, pool_types.Shutdown)
    None -> Nil
  }
  Nil
}

/// Convert download result to video status
fn result_to_status(result: types.DownloadResult) -> types.VideoStatus {
  case result {
    core_types.DownloadComplete(_, _) -> core_types.Completed
    core_types.DownloadFailed(_, reason) -> core_types.Failed(reason)
    _ -> core_types.Completed
  }
}

// External FFI declarations
@external(erlang, "os", "system_time")
fn get_timestamp() -> Int

// Downloader FFI (to avoid circular imports)
@external(erlang, "engine@downloader", "start")
fn start_downloader(
  config: ytdlp.DownloadConfig,
) -> Result(Subject(DownloaderMsg), actor.StartError)

type DownloaderMsg

@external(erlang, "engine@downloader", "Download")
fn make_download_message(
  job_id: JobId,
  url: String,
  reply: Subject(types.DownloadResult),
  progress: Subject(ManagerMessage),
) -> DownloaderMsg
