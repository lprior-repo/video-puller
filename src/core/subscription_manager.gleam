/// Subscription Manager Actor
///
/// OTP actor that owns the subscription download schedule. Each poll runs the
/// ytdl-sub engine over the configured public channel list, one download at a
/// time, and records what arrived in the library.
import domain/subscription_types.{
  type PollResult, type SubscriptionConfig, type SubscriptionStatus, PollResult,
  SubscriptionConfig, SubscriptionStatus,
}
import engine/takeout
import engine/ytdl_sub
import envoy
import gleam/erlang/process.{type Subject, type Timer}
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/result
import infra/db.{type Db}
import infra/subscription_repo

/// Messages the subscription manager can receive
pub type SubscriptionMessage {
  /// Trigger an immediate poll
  Poll
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
  PollFinished(Result(ytdl_sub.PullSummary, String))
}

/// Internal state of the subscription manager
pub opaque type SubscriptionState {
  SubscriptionState(
    db: Db,
    config: SubscriptionConfig,
    last_result: Option(PollResult),
    is_polling: Bool,
    next_poll_at: Option(Int),
    self: Option(Subject(SubscriptionMessage)),
    /// Exactly one timer chain exists: arming cancels the previous timer and
    /// bumps the generation, so a message already in flight is ignored
    timer: Option(Timer),
    /// Fires when a poll worker never reports back
    watchdog: Option(Timer),
    poll_generation: Int,
  )
}

/// Start the subscription manager actor
pub fn start(db: Db) -> Result(Subject(SubscriptionMessage), actor.StartError) {
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
      self: None,
      timer: None,
      watchdog: None,
      poll_generation: 0,
    )

  actor.start(
    actor.new(state)
    |> actor.on_message(handle_message),
  )
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

    PollFinished(outcome) -> {
      let timestamp = get_timestamp()
      let _ = subscription_repo.update_last_poll(state.db, timestamp)
      let library_dir = ytdl_sub.layout_from_env().library_dir

      let poll_result = case outcome {
        Ok(summary) -> {
          let unrecorded =
            record_downloads(
              state.db,
              library_dir,
              summary.added_files,
              timestamp,
            )
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

      let finished_state =
        SubscriptionState(
          ..state,
          is_polling: False,
          last_result: Some(poll_result),
          config: SubscriptionConfig(
            ..state.config,
            last_poll_at: Some(timestamp),
          ),
        )

      // One chain only: this is the sole place the next poll timer is armed
      actor.continue(arm_next_poll(disarm_watchdog(finished_state)))
    }

    ScheduledPoll(generation) -> {
      case generation == state.poll_generation {
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
          io.println("⚠ Poll worker never reported back; recovering poll state")
          let stalled =
            SubscriptionState(
              ..state,
              is_polling: False,
              watchdog: None,
              last_result: Some(
                PollResult(
                  total_found: 0,
                  new_videos: 0,
                  queued_for_download: 0,
                  skipped: 0,
                  errors: ["subscription poll worker did not finish"],
                ),
              ),
            )
          actor.continue(arm_next_poll(stalled))
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
      actor.stop()
    }

    SetSelf(subject) -> {
      // Receiving the self reference is what starts the schedule
      actor.continue(arm_next_poll(
        SubscriptionState(..state, self: Some(subject)),
      ))
    }
  }
}

/// Run one engine pull and report the outcome back to the manager
fn run_pull_worker(self: Subject(SubscriptionMessage)) -> Nil {
  let layout = ytdl_sub.layout_from_env()

  let outcome = case ytdl_sub.ensure_layout(layout) {
    Error(err) -> Error("layout: " <> err)
    Ok(_) -> {
      takeout.import_and_log(layout)
      case ytdl_sub.read_channels(layout.channels_file) {
        Error(err) -> Error("channels: " <> err)
        Ok([]) -> Error("no channels configured in " <> layout.channels_file)
        Ok(channels) ->
          case ytdl_sub.write_subscriptions(layout, channels) {
            Error(err) -> Error("subscriptions: " <> err)
            Ok(_) -> ytdl_sub.run_pull(layout, pull_timeout_ms())
          }
      }
    }
  }

  process.send(self, PollFinished(outcome))
}

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

/// Record library files that have no seen-video row yet
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
    case subscription_repo.is_seen(db, video.video_id) {
      Ok(True) -> acc
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

/// Start one engine poll in its own process, with a watchdog
fn start_poll(
  state: SubscriptionState,
) -> actor.Next(SubscriptionState, SubscriptionMessage) {
  log_poll_start()
  case state.self {
    Some(self) -> {
      // Unlinked on purpose: a crashed worker must not take the manager down,
      // and the watchdog recovers the polling flag either way
      let _ = process.spawn_unlinked(fn() { run_pull_worker(self) })
      let watchdog =
        process.send_after(
          self,
          pull_timeout_ms() + 60_000,
          PollWatchdog(state.poll_generation),
        )
      actor.continue(
        SubscriptionState(..state, is_polling: True, watchdog: Some(watchdog)),
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
    poll_generation: state.poll_generation + 1,
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
  let minutes = safe_minutes(state.config.poll_interval_minutes)
  case state.config.enabled, state.self {
    True, Some(self) -> {
      let state = disarm_timer(state)
      let timer =
        process.send_after(
          self,
          minutes * 60_000,
          ScheduledPoll(state.poll_generation),
        )
      SubscriptionState(
        ..state,
        timer: Some(timer),
        next_poll_at: Some(get_timestamp() + minutes * 60),
      )
    }
    _, _ -> state
  }
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
