/// Manager Actor - Job Queue Orchestrator
///
/// Polls the database for pending jobs and dispatches them to downloader actors.
/// Implements the core orchestration logic with configurable polling intervals.
import domain/types.{type JobId, type ManagerMessage, SetSelf}
import engine/downloader
import engine/ytdlp
import gleam/erlang/process.{type Subject}
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
    active_downloads: List(JobId),
    max_concurrency: Int,
    self: Option(Subject(ManagerMessage)),
  )
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
      active_downloads: [],
      max_concurrency: max_concurrency,
      self: None,
    )

  actor.start(
    actor.new(state)
    |> actor.on_message(handle_message),
  )
  |> result.map(fn(started) {
    // Send the subject back to itself so it can store it
    let subject = started.data
    process.send(subject, SetSelf(subject))
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
      // Check for pending jobs and dispatch
      let new_state = poll_and_dispatch(state)

      // Note: In production, we'd use process.send_after with a subject
      // to schedule the next poll. For now, external callers will trigger polls.

      actor.continue(new_state)
    }

    types.JobStatusUpdate(job_id, status) -> {
      // Update job status in database
      let timestamp = get_timestamp()
      let _ = repo.update_status(state.db, job_id, status, timestamp)

      // Remove from active downloads if terminal
      let new_active = case types.is_terminal_status(status) {
        True ->
          list.filter(state.active_downloads, fn(id) {
            types.job_id_to_string(id) != types.job_id_to_string(job_id)
          })
        False -> state.active_downloads
      }

      let new_state = ManagerState(..state, active_downloads: new_active)

      actor.continue(new_state)
    }

    types.UpdateProgress(job_id, progress) -> {
      // Update job status with progress in database
      let timestamp = get_timestamp()
      let _ =
        repo.update_status(
          state.db,
          job_id,
          types.Downloading(progress),
          timestamp,
        )

      actor.continue(state)
    }

    types.Shutdown -> {
      io.println("Manager shutting down...")
      actor.stop()
    }

    SetSelf(subject) -> {
      let new_state = ManagerState(..state, self: Some(subject))
      actor.continue(new_state)
    }
  }
}

/// Poll database and dispatch pending jobs
fn poll_and_dispatch(state: ManagerState) -> ManagerState {
  // Check if we can accept more jobs
  let active_count = list.length(state.active_downloads)
  let available_slots = state.max_concurrency - active_count

  case available_slots > 0, state.self {
    False, _ -> state
    _, None -> state
    True, Some(self) -> {
      // Get pending jobs
      case repo.list_jobs(state.db, Some("pending")) {
        Ok(jobs) -> {
          // Take only what we can handle
          let jobs_to_start =
            jobs
            |> list.take(available_slots)

          // Start each job
          let new_active =
            list.fold(jobs_to_start, state.active_downloads, fn(active, job) {
              case start_download(job.id, job.url, state.config, self) {
                Ok(_) -> {
                  io.println(
                    "Started download: "
                    <> types.job_id_to_string(job.id)
                    <> " - "
                    <> job.url,
                  )
                  [job.id, ..active]
                }
                Error(_) -> active
              }
            })

          ManagerState(..state, active_downloads: new_active)
        }
        Error(_) -> state
      }
    }
  }
}

/// Start a download by spawning a downloader actor
fn start_download(
  job_id: JobId,
  url: String,
  config: ytdlp.DownloadConfig,
  manager_subject: Subject(ManagerMessage),
) -> Result(Nil, String) {
  case downloader.start(config) {
    Ok(subject) -> {
      // Create a reply subject for receiving the download result
      let reply_subject = process.new_subject()

      // Send Download message to the downloader with manager subject for progress
      process.send(
        subject,
        downloader.Download(job_id, url, reply_subject, manager_subject),
      )

      Ok(Nil)
    }
    Error(_) -> Error("Failed to start downloader")
  }
}

/// Get current Unix timestamp
@external(erlang, "os", "system_time")
fn get_timestamp() -> Int
