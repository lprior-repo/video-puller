/// Subscription Manager Actor
///
/// OTP actor that owns the subscription download schedule. Each poll runs the
/// ytdl-sub engine over the configured public channel list, one download at a
/// time, and records what arrived in the library.
import domain/subscription_types.{
  type PollResult, type SubscriptionConfig, type SubscriptionStatus, PollResult,
  SubscriptionConfig, SubscriptionStatus,
}
import engine/ytdl_sub
import envoy
import gleam/erlang/process.{type Subject}
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
  /// Schedule the next automatic poll
  ScheduleNextPoll
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
          log_poll_start()
          case state.self {
            Some(self) -> {
              process.spawn(fn() { run_pull_worker(self) })
              actor.continue(SubscriptionState(..state, is_polling: True))
            }
            None -> actor.continue(state)
          }
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

      let poll_result = case outcome {
        Ok(summary) -> {
          record_downloads(state.db, summary.added_files, timestamp)
          PollResult(
            total_found: summary.downloaded,
            new_videos: summary.downloaded,
            queued_for_download: summary.downloaded,
            skipped: 0,
            errors: summary.errors,
          )
        }
        Error(err) -> {
          io.println("Poll error: " <> err)
          PollResult(
            total_found: 0,
            new_videos: 0,
            queued_for_download: 0,
            skipped: 0,
            errors: [err],
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

      case finished_state.config.enabled, finished_state.self {
        True, Some(self) -> {
          let next_ts =
            timestamp + finished_state.config.poll_interval_minutes * 60
          schedule_poll(self, finished_state.config.poll_interval_minutes)
          actor.continue(
            SubscriptionState(..finished_state, next_poll_at: Some(next_ts)),
          )
        }
        _, _ -> actor.continue(finished_state)
      }
    }

    ScheduleNextPoll -> {
      case state.config.enabled, state.self {
        True, Some(self) -> {
          let timestamp = get_timestamp()
          let next_ts = timestamp + state.config.poll_interval_minutes * 60
          schedule_poll(self, state.config.poll_interval_minutes)
          actor.continue(
            SubscriptionState(..state, next_poll_at: Some(next_ts)),
          )
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

      let new_state = SubscriptionState(..state, config: new_config)

      case new_config.enabled, state.self {
        True, Some(self) -> {
          let next_ts = timestamp + new_config.poll_interval_minutes * 60
          schedule_poll(self, new_config.poll_interval_minutes)
          actor.continue(
            SubscriptionState(..new_state, next_poll_at: Some(next_ts)),
          )
        }
        _, _ -> actor.continue(new_state)
      }
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
      actor.continue(SubscriptionState(..state, self: Some(subject)))
    }
  }
}

/// Run one engine pull and report the outcome back to the manager
fn run_pull_worker(self: Subject(SubscriptionMessage)) -> Nil {
  let layout = ytdl_sub.layout_from_env()

  let outcome = case ytdl_sub.ensure_layout(layout) {
    Error(err) -> Error("layout: " <> err)
    Ok(_) ->
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

  process.send(self, PollFinished(outcome))
}

/// Record each newly added file in the seen-video table
fn record_downloads(db: Db, paths: List(String), timestamp: Int) -> Nil {
  let library_dir = ytdl_sub.layout_from_env().library_dir

  list.each(paths, fn(path) {
    let video = ytdl_sub.to_discovered_video(library_dir, path)
    let _ = subscription_repo.mark_seen(db, video, True, None, None, timestamp)
    io.println("Downloaded: " <> path)
  })
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

/// Schedule next poll after given minutes
fn schedule_poll(self: Subject(SubscriptionMessage), minutes: Int) -> Nil {
  let ms = minutes * 60 * 1000
  let _ =
    process.spawn(fn() {
      process.sleep(ms)
      process.send(self, Poll)
    })
  Nil
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

fn log_poll_end(found: Int, new: Int, queued: Int, skipped: Int) -> Nil {
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
    <> " skipped="
    <> int.to_string(skipped),
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
