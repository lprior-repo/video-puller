/// Subscription Manager Actor
///
/// OTP actor that owns the subscription download schedule. Each poll runs the
/// ytdl-sub engine over the configured public channel list using bounded
/// workers, and records what arrived in the library.
import core/channel_reconciler
import domain/subscription_types.{
  type PollResult, type SubscriptionConfig, type SubscriptionStatus, PollResult,
  SubscriptionConfig, SubscriptionStatus,
}
import engine/channel_import
import engine/takeout
import engine/ytdl_sub
import envoy
import gleam/erlang/process.{
  type Down, type Monitor, type Name, type Pid, type Subject, type Timer,
}
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/result
import gleam/string
import infra/db.{type Db}
import infra/plex
import infra/subscription_repo

/// Messages the subscription manager can receive
pub type SubscriptionMessage {
  /// Trigger an immediate poll
  Poll
  /// Apply a new desired channel list, stopping outdated workers first.
  ReloadChannels
  /// Internal: converge changes made to the persisted desired file.
  CheckDesiredChannels
  /// Internal: the scheduled poll timer fired
  ScheduledPoll(generation: Int)
  /// Internal: the poll worker did not report back within its budget
  PollWatchdog(generation: Int)
  /// Update configuration
  UpdateConfig(SubscriptionConfig)
  /// Get current status
  GetStatus(Subject(SubscriptionStatus))
  /// Shutdown the actor
  Shutdown
  /// Internal: store self reference
  SetSelf(Subject(SubscriptionMessage))
  /// Internal: a background pull finished
  PollFinished(generation: Int, outcome: Result(ytdl_sub.PullSummary, String))
  /// Internal: the poll worker crashed without reporting a result
  PollWorkerDown(generation: Int, down: Down)
}

/// Internal state of the subscription manager
pub opaque type SubscriptionState {
  SubscriptionState(
    db: Db,
    config: SubscriptionConfig,
    last_result: Option(PollResult),
    is_polling: Bool,
    next_poll_at: Option(Int),
    /// Cadence is measured from the poll start, not its completion.
    poll_started_at: Option(Int),
    self: Option(Subject(SubscriptionMessage)),
    /// Exactly one timer chain exists: arming cancels the previous timer and
    /// bumps the generation, so a message already in flight is ignored
    timer: Option(Timer),
    /// Fires when a poll worker never reports back
    watchdog: Option(Timer),
    schedule_generation: Int,
    /// Poll identity is independent of settings changes and schedule timers
    poll_generation: Int,
    worker: Option(Pid),
    worker_monitor: Option(Monitor),
    poll_guard_name: Option(Name(SubscriptionMessage)),
    run_poll: fn() -> Result(ytdl_sub.PullSummary, String),
    watchdog_ms: Int,
    reconcile_pending: Bool,
  )
}

/// Start the subscription manager actor
pub fn start(db: Db) -> Result(Subject(SubscriptionMessage), actor.StartError) {
  let interval = case subscription_repo.get_config(db) {
    Ok(config) -> config.poll_interval_minutes
    Error(_) -> subscription_types.default_config().poll_interval_minutes
  }
  start_with_runner_internal(
    db,
    fn() { run_pull_worker(db) },
    effective_pull_timeout_ms(interval) + 120_000,
    None,
  )
}

/// Start a named manager whose subject remains valid across supervisor restarts.
/// Before reusing its name, wait for any previous manager's poll worker and
/// downloader process tree to finish cleanup.
pub fn start_named(
  db: Db,
  name: Name(SubscriptionMessage),
) -> Result(Subject(SubscriptionMessage), actor.StartError) {
  let interval = case subscription_repo.get_config(db) {
    Ok(config) -> config.poll_interval_minutes
    Error(_) -> subscription_types.default_config().poll_interval_minutes
  }
  start_named_with_runner(
    db,
    name,
    fn() { run_pull_worker(db) },
    effective_pull_timeout_ms(interval) + 120_000,
  )
}

/// Named variant with an injectable poll runner for deterministic lifecycle tests.
pub fn start_named_with_runner(
  db: Db,
  name: Name(SubscriptionMessage),
  run_poll: fn() -> Result(ytdl_sub.PullSummary, String),
  watchdog_ms: Int,
) -> Result(Subject(SubscriptionMessage), actor.StartError) {
  case await_previous_poll_cleanup(name) {
    False ->
      Error(actor.InitFailed("previous subscription poll cleanup timed out"))
    True -> {
      start_with_runner_internal(db, run_poll, watchdog_ms, Some(name))
    }
  }
}

/// Start with an injectable poll runner and watchdog deadline.
///
/// The runner executes in a separate process and must return only after all
/// its downloads have stopped. This also lets lifecycle tests run without a
/// downloader or network access.
pub fn start_with_runner(
  db: Db,
  run_poll: fn() -> Result(ytdl_sub.PullSummary, String),
  watchdog_ms: Int,
) -> Result(Subject(SubscriptionMessage), actor.StartError) {
  start_with_runner_internal(db, run_poll, watchdog_ms, None)
}

fn start_with_runner_internal(
  db: Db,
  run_poll: fn() -> Result(ytdl_sub.PullSummary, String),
  watchdog_ms: Int,
  poll_guard_name: Option(Name(SubscriptionMessage)),
) -> Result(Subject(SubscriptionMessage), actor.StartError) {
  let config = case subscription_repo.get_config(db) {
    Ok(c) -> c
    Error(_) -> subscription_types.default_config()
  }

  let state =
    SubscriptionState(
      db: db,
      config: config,
      last_result: None,
      is_polling: False,
      next_poll_at: None,
      poll_started_at: None,
      self: None,
      timer: None,
      watchdog: None,
      schedule_generation: 0,
      poll_generation: 0,
      worker: None,
      worker_monitor: None,
      poll_guard_name: poll_guard_name,
      run_poll: run_poll,
      watchdog_ms: int.max(watchdog_ms, 1),
      reconcile_pending: False,
    )

  let builder =
    actor.new(state)
    |> actor.on_message(handle_message)
  let builder = case poll_guard_name {
    Some(name) -> actor.named(builder, name)
    None -> builder
  }
  actor.start(builder)
  |> result.map(fn(started) {
    let subject = started.data
    process.send(subject, SetSelf(subject))
    subject
  })
}

/// Handle subscription manager messages
fn handle_message(
  state: SubscriptionState,
  message: SubscriptionMessage,
) -> actor.Next(SubscriptionState, SubscriptionMessage) {
  case message {
    CheckDesiredChannels -> {
      case state.self {
        Some(subject) -> {
          let _ = process.send_after(subject, 30_000, CheckDesiredChannels)
          case
            channel_import.has_pending_changes(
              ytdl_sub.layout_from_env().channels_file,
            )
          {
            Ok(True) -> process.send(subject, ReloadChannels)
            Ok(False) ->
              case state.reconcile_pending && !state.is_polling {
                True -> process.send(subject, ReloadChannels)
                False -> Nil
              }
            Error(message) ->
              io.println("Desired channels invalid: " <> message)
          }
        }
        None -> Nil
      }
      actor.continue(state)
    }
    ReloadChannels -> {
      let stopped = disarm_watchdog(disarm_timer(state))
      stop_active_worker(stopped)
      let stopped =
        SubscriptionState(
          ..stopped,
          is_polling: False,
          worker: None,
          worker_monitor: None,
          reconcile_pending: True,
        )
      case stopped.config.enabled {
        True -> start_poll(stopped)
        False -> {
          let reconciled = case
            channel_reconciler.reconcile(stopped.db, ytdl_sub.layout_from_env())
          {
            Ok(_) -> True
            Error(message) -> {
              io.println("Channel reconciliation failed: " <> message)
              False
            }
          }
          actor.continue(
            SubscriptionState(..stopped, reconcile_pending: !reconciled),
          )
        }
      }
    }
    Poll -> {
      case state.config.enabled, state.is_polling {
        True, False -> {
          // A manual poll must not leave the scheduled chain running: drop the
          // pending timer, and arm exactly one when the poll finishes
          start_poll(disarm_timer(state))
        }
        False, _ -> {
          io.println("Subscription polling disabled")
          actor.continue(state)
        }
        _, True -> {
          io.println("Poll already in progress, skipping")
          actor.continue(state)
        }
      }
    }

    PollFinished(generation, outcome) -> {
      case state.is_polling && generation == state.poll_generation {
        True -> finish_poll(state, outcome)
        // A late completion from a timed-out worker cannot finish a newer poll
        False -> actor.continue(state)
      }
    }

    PollWorkerDown(generation, down) -> {
      case state.is_polling && generation == state.poll_generation {
        True -> {
          io.println("⚠ Poll worker exited unexpectedly")
          stop_active_worker(state)
          finish_poll(
            state,
            Error(
              "subscription poll worker exited: " <> string.inspect(down.reason),
            ),
          )
        }
        False -> actor.continue(state)
      }
    }

    ScheduledPoll(generation) -> {
      case generation == state.schedule_generation {
        True ->
          case state.config.enabled, state.is_polling {
            True, False -> start_poll(SubscriptionState(..state, timer: None))
            _, _ -> actor.continue(SubscriptionState(..state, timer: None))
          }
        // Stale timer from a chain that was replaced: never start a poll from
        // it, or a saved setting ends up scheduling twice
        False -> actor.continue(state)
      }
    }

    PollWatchdog(generation) -> {
      case generation == state.poll_generation, state.is_polling {
        // The worker died without reporting back; recover instead of staying
        // "polling" until the next restart
        True, True -> {
          io.println("⚠ Poll worker exceeded its deadline; stopping poll")
          stop_active_worker(state)
          finish_poll(state, Error("subscription poll worker did not finish"))
        }
        _, _ -> actor.continue(state)
      }
    }

    UpdateConfig(new_config) -> {
      let timestamp = get_timestamp()
      let _ = subscription_repo.update_config(state.db, new_config, timestamp)

      io.println(
        "Subscription config updated: enabled="
        <> bool_to_string(new_config.enabled),
      )

      // One chain only: replacing the timer here is what stops a settings save
      // from adding a second recurring poll
      actor.continue(arm_next_poll(
        SubscriptionState(..state, config: new_config),
      ))
    }

    GetStatus(reply) -> {
      let status =
        SubscriptionStatus(
          enabled: state.config.enabled,
          last_poll_at: state.config.last_poll_at,
          next_poll_at: state.next_poll_at,
          last_result: state.last_result,
          is_polling: state.is_polling,
        )
      process.send(reply, status)
      actor.continue(state)
    }

    Shutdown -> {
      io.println("Subscription manager shutting down...")
      let state = disarm_watchdog(disarm_timer(state))
      stop_active_worker(state)
      actor.stop()
    }

    SetSelf(subject) -> {
      case state.self {
        None -> {
          let _ = process.send_after(subject, 30_000, CheckDesiredChannels)
          Nil
        }
        Some(_) -> Nil
      }
      // Receiving the self reference is what starts the schedule
      actor.continue(arm_next_poll(
        SubscriptionState(..state, self: Some(subject)),
      ))
    }
  }
}

/// Finish only the current poll, reconciling committed files on every outcome.
fn finish_poll(
  state: SubscriptionState,
  outcome: Result(ytdl_sub.PullSummary, String),
) -> actor.Next(SubscriptionState, SubscriptionMessage) {
  let timestamp = get_timestamp()
  let _ = subscription_repo.update_last_poll(state.db, timestamp)
  let library_dir = ytdl_sub.layout_from_env().library_dir

  let poll_result = case outcome {
    Ok(summary) -> {
      let unrecorded =
        record_downloads(state.db, library_dir, summary.added_files, timestamp)
      // Runs after the engine's report so only files the report missed
      // count as reconciled
      let #(reconciled, unrecorded_reconcile) =
        reconcile_library(state.db, library_dir, timestamp)
      let total = summary.downloaded + reconciled
      PollResult(
        total_found: total,
        new_videos: total,
        queued_for_download: total,
        skipped: 0,
        errors: append_record_errors(
          summary.errors,
          unrecorded + unrecorded_reconcile,
        ),
      )
    }
    Error(err) -> {
      io.println("Poll error: " <> err)
      // The engine may have committed files to the library before it
      // failed or hit the timeout. The download archive skips them on
      // later polls, so they need recording here.
      let #(reconciled, unrecorded) =
        reconcile_library(state.db, library_dir, timestamp)
      PollResult(
        total_found: reconciled,
        new_videos: reconciled,
        queued_for_download: reconciled,
        skipped: 0,
        errors: append_record_errors([err], unrecorded),
      )
    }
  }

  log_poll_end(
    poll_result.total_found,
    poll_result.new_videos,
    poll_result.queued_for_download,
    list.length(poll_result.errors),
  )

  clear_worker_monitor(state.worker_monitor)
  let finished_state =
    SubscriptionState(
      ..state,
      is_polling: False,
      worker: None,
      worker_monitor: None,
      last_result: Some(poll_result),
      config: SubscriptionConfig(..state.config, last_poll_at: Some(timestamp)),
      reconcile_pending: case outcome {
        Ok(_) -> False
        Error(_) -> state.reconcile_pending
      },
    )

  let finished_state = disarm_watchdog(finished_state)
  case outcome {
    Ok(_) -> actor.continue(arm_next_poll(finished_state))
    Error(_) ->
      actor.continue(arm_next_poll_after(finished_state, retry_delay_ms()))
  }
}

/// Run one engine pull within a deadline that includes channel preparation.
fn run_pull_worker(db: Db) -> Result(ytdl_sub.PullSummary, String) {
  let interval = case subscription_repo.get_config(db) {
    Ok(config) -> config.poll_interval_minutes
    Error(_) -> subscription_types.default_config().poll_interval_minutes
  }
  let deadline = monotonic_milliseconds() + effective_pull_timeout_ms(interval)
  let layout = ytdl_sub.layout_from_env()

  use _ <- result.try(
    ytdl_sub.ensure_layout(layout)
    |> result.map_error(fn(err) { "layout: " <> err }),
  )
  takeout.import_and_log(layout)
  use _ <- result.try(channel_reconciler.reconcile(db, layout))
  use channels <- result.try(
    ytdl_sub.read_channels(layout.channels_file)
    |> result.map_error(fn(err) { "channels: " <> err }),
  )
  case channels {
    [] -> Error("no channels configured in " <> layout.channels_file)
    [_, ..] -> {
      use remaining <- result.try(remaining_poll_time(deadline))
      use _ <- result.try(
        ytdl_sub.write_subscriptions_with_budget(layout, channels, remaining)
        |> result.map_error(fn(err) { "subscriptions: " <> err }),
      )
      run_poll_phases(layout, deadline, db)
    }
  }
}

/// Run one poll in two passes over the same channel list
///
/// The recent pass checks fresh uploads first within a slice of the poll
/// budget, then backfill spends the remaining time on history. Both passes
/// may time out before reaching every channel; the persisted channel rotation
/// gives later channels the first chance on subsequent polls. Files from a
/// surviving pass are retained even when the other pass fails.
fn run_poll_phases(
  layout: ytdl_sub.Layout,
  deadline: Int,
  db: Db,
) -> Result(ytdl_sub.PullSummary, String) {
  use remaining <- result.try(remaining_poll_time(deadline))
  let #(recent_ms, _) = phase_timeouts(remaining, recent_phase_ms())

  io.println(
    "Poll pass 1/2 - recent uploads: up to "
    <> int.to_string(recent_ms / 60_000)
    <> " min",
  )
  let recent =
    label_pass(
      "recent pass",
      ytdl_sub.run_pull_with_progress(
        layout,
        layout.recent_subscriptions_file,
        recent_ms,
        fn(_) { update_library_progress(db, layout.library_dir) },
      ),
    )
  log_pass("recent", recent)

  let backfill = case remaining_poll_time(deadline) {
    Error(err) -> Error("backfill pass: " <> err)
    Ok(backfill_ms) -> {
      io.println(
        "Poll pass 2/2 - backfill: up to "
        <> int.to_string(backfill_ms / 60_000)
        <> " min",
      )
      label_pass(
        "backfill pass",
        ytdl_sub.run_pull_with_progress(
          layout,
          layout.subscriptions_file,
          backfill_ms,
          fn(_) { update_library_progress(db, layout.library_dir) },
        ),
      )
    }
  }
  log_pass("backfill", backfill)

  ytdl_sub.merge_summaries(recent, backfill)
}

fn update_library_progress(db: Db, library_dir: String) -> Nil {
  let _ = reconcile_library(db, library_dir, get_timestamp())
  plex.enrich_titles(library_dir)
  plex.refresh_library()
}

/// Name the pass in its errors, so a poll row says which one failed
fn label_pass(
  name: String,
  outcome: Result(ytdl_sub.PullSummary, String),
) -> Result(ytdl_sub.PullSummary, String) {
  case outcome {
    Ok(summary) ->
      Ok(
        ytdl_sub.PullSummary(
          ..summary,
          errors: list.map(summary.errors, fn(err) { name <> ": " <> err }),
        ),
      )
    Error(err) -> Error(name <> ": " <> err)
  }
}

/// Report what a pass produced, including a failed one
fn log_pass(
  name: String,
  outcome: Result(ytdl_sub.PullSummary, String),
) -> Nil {
  case outcome {
    Ok(summary) ->
      io.println(
        "  "
        <> name
        <> ": "
        <> int.to_string(summary.downloaded)
        <> " file(s), "
        <> int.to_string(list.length(summary.errors))
        <> " error(s)",
      )
    Error(err) -> io.println("  " <> name <> " failed: " <> err)
  }
}

/// Split the poll budget between the recent-upload pass and the backfill
///
/// The recent pass never takes more than its own setting or a quarter of the
/// poll budget. Backfill keeps at least a minute whenever the remaining
/// budget permits, and sub-second remainders never become negative.
pub fn phase_timeouts(total_ms: Int, recent_ms: Int) -> #(Int, Int) {
  let total_ms = int.max(total_ms, 0)
  let minimum_recent = int.min(1000, total_ms / 4)
  let cap = int.max(int.min(total_ms / 4, recent_ms), minimum_recent)
  let recent =
    int.min(cap, int.max(total_ms - minimum_backfill_ms, minimum_recent))
  #(recent, total_ms - recent)
}

/// Budget for the recent-upload pass (`RECENT_PHASE_MINUTES`)
fn recent_phase_ms() -> Int {
  let minutes = case envoy.get("RECENT_PHASE_MINUTES") {
    Ok(raw) ->
      case int.parse(raw) {
        Ok(minutes) if minutes > 0 -> minutes
        _ -> default_recent_phase_minutes
      }
    Error(_) -> default_recent_phase_minutes
  }
  minutes * 60_000
}

const default_recent_phase_minutes = 30

/// Reserve this much backfill time whenever the remaining budget permits
const minimum_backfill_ms = 60_000

/// Record each newly added file in the seen-video table
///
/// Returns how many rows could not be written, so a storage failure is
/// reported instead of silently losing history.
fn record_downloads(
  db: Db,
  library_dir: String,
  paths: List(String),
  timestamp: Int,
) -> Int {
  list.fold(paths, 0, fn(failed, path) {
    let video = ytdl_sub.to_discovered_video(library_dir, path)
    case subscription_repo.mark_seen(db, video, True, None, None, timestamp) {
      Ok(_) -> {
        io.println("Downloaded: " <> path)
        failed
      }
      Error(_) -> {
        io.println("⚠ Cannot record " <> path)
        failed + 1
      }
    }
  })
}

/// Record library files and refresh metadata for existing records.
///
/// A poll that exits non-zero or times out leaves downloaded files in the
/// library without the engine's "Files created" report, and the per-channel
/// download archive skips them on later polls — so this pass is what keeps the
/// feed and the counters accurate. Returns how many files were recorded and
/// how many rows could not be written.
pub fn reconcile_library(
  db: Db,
  library_dir: String,
  timestamp: Int,
) -> #(Int, Int) {
  ytdl_sub.list_library_media(library_dir)
  |> list.fold(#(0, 0), fn(acc, path) {
    let #(recorded, failed) = acc
    let video = ytdl_sub.to_discovered_video(library_dir, path)
    case subscription_repo.get_seen_video(db, video.video_id) {
      Ok(Some(existing)) -> {
        // Preserve arrival order and job history when fuller sidecar metadata
        // becomes available after an interrupted or earlier download.
        case
          existing.title == video.title
          && existing.url == video.url
          && existing.duration_seconds == video.duration_seconds
          && existing.thumbnail_url == video.thumbnail_url
          && existing.published_at == video.published_at
        {
          True -> acc
          False ->
            case
              subscription_repo.mark_seen(
                db,
                video,
                True,
                existing.skip_reason,
                existing.job_id,
                existing.first_seen_at,
              )
            {
              Ok(_) -> acc
              Error(_) -> #(recorded, failed + 1)
            }
        }
      }
      _ ->
        case
          subscription_repo.mark_seen(db, video, True, None, None, timestamp)
        {
          Ok(_) -> {
            io.println("Reconciled: " <> path)
            #(recorded + 1, failed)
          }
          Error(_) -> {
            io.println("⚠ Cannot record " <> path)
            #(recorded, failed + 1)
          }
        }
    }
  })
}

/// Add a poll error when records could not be written
fn append_record_errors(errors: List(String), failed: Int) -> List(String) {
  case failed {
    0 -> errors
    n ->
      list.append(errors, [
        "failed to record " <> int.to_string(n) <> " downloaded file(s)",
      ])
  }
}

/// Poll timeout in milliseconds
fn pull_timeout_ms() -> Int {
  let minutes = case envoy.get("POLL_TIMEOUT_MINUTES") {
    Ok(raw) ->
      case int.parse(raw) {
        Ok(n) if n > 0 -> n
        _ -> 360
      }
    Error(_) -> 360
  }
  minutes * 60_000
}

fn effective_pull_timeout_ms(interval_minutes: Int) -> Int {
  int.min(
    pull_timeout_ms(),
    int.max(safe_minutes(interval_minutes) * 60_000 - 120_000, 1000),
  )
}

/// Start one engine poll in its own process, with a watchdog
fn start_poll(
  state: SubscriptionState,
) -> actor.Next(SubscriptionState, SubscriptionMessage) {
  log_poll_start()
  case state.self {
    Some(self) -> {
      // Unlinked on purpose: a crashed worker must not take the manager down,
      // and the watchdog recovers the polling flag either way
      let generation = state.poll_generation + 1
      let worker =
        process.spawn_unlinked(fn() {
          let outcome = state.run_poll()
          process.send(self, PollFinished(generation, outcome))
        })
      let worker_monitor = process.monitor(worker)
      case state.poll_guard_name {
        Some(name) -> guard_poll_worker(name, process.self(), worker)
        None -> Nil
      }
      // Two engine passes share the poll budget, so the watchdog leaves room
      // for both deadlines plus the startup and teardown of each process
      let watchdog_delay =
        int.min(
          state.watchdog_ms,
          safe_minutes(state.config.poll_interval_minutes) * 60_000,
        )
      let watchdog =
        process.send_after(self, watchdog_delay, PollWatchdog(generation))
      actor.continue(
        SubscriptionState(
          ..state,
          is_polling: True,
          poll_started_at: Some(get_timestamp()),
          watchdog: Some(watchdog),
          poll_generation: generation,
          worker: Some(worker),
          worker_monitor: Some(worker_monitor),
        ),
      )
      |> actor.with_selector(
        process.new_selector()
        |> process.select(self)
        |> process.select_specific_monitor(worker_monitor, fn(down) {
          PollWorkerDown(generation, down)
        }),
      )
    }
    None -> actor.continue(state)
  }
}

/// Cancel the pending poll timer and invalidate its generation
fn disarm_timer(state: SubscriptionState) -> SubscriptionState {
  let _ = case state.timer {
    Some(timer) -> process.cancel_timer(timer)
    None -> process.TimerNotFound
  }
  SubscriptionState(
    ..state,
    timer: None,
    next_poll_at: None,
    schedule_generation: state.schedule_generation + 1,
  )
}

/// Cancel the watchdog of a poll that has finished
fn disarm_watchdog(state: SubscriptionState) -> SubscriptionState {
  let _ = case state.watchdog {
    Some(timer) -> process.cancel_timer(timer)
    None -> process.TimerNotFound
  }
  SubscriptionState(..state, watchdog: None)
}

/// Arm the single poll timer chain
///
/// Arming replaces whatever timer was pending, so a settings change, a manual
/// poll or a finished poll can never leave more than one chain running.
fn arm_next_poll(state: SubscriptionState) -> SubscriptionState {
  let state = disarm_timer(state)
  let minutes = safe_minutes(state.config.poll_interval_minutes)
  let now = get_timestamp()
  let started_at = option.unwrap(state.poll_started_at, now)
  let delay = next_poll_delay(started_at, now, minutes)
  arm_next_poll_after_disarmed(state, delay * 1000)
}

fn arm_next_poll_after(
  state: SubscriptionState,
  delay_ms: Int,
) -> SubscriptionState {
  let state = disarm_timer(state)
  arm_next_poll_after_disarmed(state, int.max(delay_ms, 1))
}

fn arm_next_poll_after_disarmed(
  state: SubscriptionState,
  delay_ms: Int,
) -> SubscriptionState {
  let now = get_timestamp()
  case state.config.enabled, state.is_polling, state.self {
    True, False, Some(self) -> {
      let timer =
        process.send_after(
          self,
          delay_ms,
          ScheduledPoll(state.schedule_generation),
        )
      SubscriptionState(
        ..state,
        timer: Some(timer),
        next_poll_at: Some(now + delay_ms / 1000),
      )
    }
    _, _, _ -> state
  }
}

/// Return seconds until the next poll, anchored to the previous poll start.
/// A backwards wall-clock adjustment is treated as zero elapsed time.
pub fn next_poll_delay(
  started_at: Int,
  now: Int,
  interval_minutes: Int,
) -> Int {
  let interval_seconds = safe_minutes(interval_minutes) * 60
  let elapsed = int.max(now - started_at, 0)
  int.max(interval_seconds - elapsed, 1)
}

fn retry_delay_ms() -> Int {
  5 * 60 * 1000
}

/// Stop downloads before making the poll slot available to another worker.
fn stop_active_worker(state: SubscriptionState) -> Nil {
  case state.worker {
    Some(worker) -> stop_worker(worker)
    None -> Nil
  }
  clear_worker_monitor(state.worker_monitor)
}

fn clear_worker_monitor(monitor: Option(Monitor)) -> Nil {
  case monitor {
    Some(value) -> process.demonitor_process(value)
    None -> Nil
  }
}

@external(erlang, "subscription_manager_ffi", "stop_worker")
fn stop_worker(worker: Pid) -> Nil

@external(erlang, "subscription_manager_ffi", "await_previous_poll_cleanup")
fn await_previous_poll_cleanup(name: Name(SubscriptionMessage)) -> Bool

@external(erlang, "subscription_manager_ffi", "guard_poll_worker")
fn guard_poll_worker(
  name: Name(SubscriptionMessage),
  manager: Pid,
  worker: Pid,
) -> Nil

fn remaining_poll_time(deadline: Int) -> Result(Int, String) {
  let remaining = deadline - monotonic_milliseconds()
  case remaining > 0 {
    True -> Ok(remaining)
    False -> Error("subscription poll deadline exceeded")
  }
}

@external(erlang, "erlang", "monotonic_time")
fn monotonic_time(unit: MonotonicUnit) -> Int

fn monotonic_milliseconds() -> Int {
  monotonic_time(Millisecond)
}

type MonotonicUnit {
  Millisecond
}

fn safe_minutes(minutes: Int) -> Int {
  case minutes < 1 {
    True -> 1
    False -> minutes
  }
}

fn bool_to_string(b: Bool) -> String {
  case b {
    True -> "true"
    False -> "false"
  }
}

/// Get current Unix timestamp in seconds
fn get_timestamp() -> Int {
  get_system_time_seconds(Second)
}

type TimeUnit {
  Second
}

@external(erlang, "erlang", "system_time")
fn get_system_time_seconds(unit: TimeUnit) -> Int

fn log_poll_start() -> Nil {
  let timestamp = format_timestamp(get_timestamp())
  io.println("[" <> timestamp <> "] [SUBSCRIPTION] POLL_START")
}

fn log_poll_end(found: Int, new: Int, queued: Int, errors: Int) -> Nil {
  let timestamp = format_timestamp(get_timestamp())
  io.println(
    "["
    <> timestamp
    <> "] [SUBSCRIPTION] POLL_END found="
    <> int.to_string(found)
    <> " new="
    <> int.to_string(new)
    <> " queued="
    <> int.to_string(queued)
    <> " errors="
    <> int.to_string(errors),
  )
}

fn format_timestamp(ts: Int) -> String {
  format_iso8601_raw(ts, [#(Unit, Second)])
  |> charlist_to_string
}

@external(erlang, "calendar", "system_time_to_rfc3339")
fn format_iso8601_raw(ts: Int, opts: List(#(FormatOpt, TimeUnit))) -> Charlist

@external(erlang, "erlang", "list_to_binary")
fn charlist_to_string(charlist: Charlist) -> String

type Charlist

type FormatOpt {
  Unit
}
