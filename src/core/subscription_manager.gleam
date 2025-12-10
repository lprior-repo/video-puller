/// Subscription Manager Actor
///
/// OTP actor that manages subscription feed polling lifecycle.
/// Periodically fetches subscription feed, filters videos, and queues downloads.
import domain/subscription_types.{
  type PollResult, type SubscriptionConfig, type SubscriptionStatus, PollResult,
  SubscriptionConfig, SubscriptionStatus,
}
import domain/types
import engine/subscription_feed
import engine/video_filter
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/result
import infra/db.{type Db}
import infra/repo
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
  // Load initial config from database
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
    // Send self reference for scheduling
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
          // Start polling
          io.println("Starting subscription feed poll...")
          let new_state = SubscriptionState(..state, is_polling: True)
          let poll_result = execute_poll(new_state)

          // Update state with result
          let timestamp = get_timestamp()
          let _ = subscription_repo.update_last_poll(state.db, timestamp)

          let finished_state =
            SubscriptionState(
              ..new_state,
              is_polling: False,
              last_result: Some(poll_result),
              config: SubscriptionConfig(
                ..state.config,
                last_poll_at: Some(timestamp),
              ),
            )

          // Log result
          io.println(
            "Poll complete: found="
            <> int.to_string(poll_result.total_found)
            <> " new="
            <> int.to_string(poll_result.new_videos)
            <> " queued="
            <> int.to_string(poll_result.queued_for_download)
            <> " skipped="
            <> int.to_string(poll_result.skipped),
          )

          // Schedule next poll
          case finished_state.self {
            Some(self) -> {
              let next_ts =
                timestamp + finished_state.config.poll_interval_minutes * 60
              schedule_poll(self, finished_state.config.poll_interval_minutes)
              actor.continue(
                SubscriptionState(..finished_state, next_poll_at: Some(next_ts)),
              )
            }
            None -> actor.continue(finished_state)
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
      // Save to database
      let timestamp = get_timestamp()
      let _ = subscription_repo.update_config(state.db, new_config, timestamp)

      io.println(
        "Subscription config updated: enabled="
        <> bool_to_string(new_config.enabled),
      )

      let new_state = SubscriptionState(..state, config: new_config)

      // If enabled and not already scheduled, schedule next poll
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

/// Execute a poll operation
fn execute_poll(state: SubscriptionState) -> PollResult {
  case subscription_feed.fetch_feed(state.config) {
    Ok(videos) -> {
      let total_found = list.length(videos)
      let timestamp = get_timestamp()

      // Process each video
      let results =
        list.map(videos, fn(video) {
          // Check if already seen
          case subscription_repo.is_seen(state.db, video.video_id) {
            Ok(True) -> #(video, subscription_types.SkippedAlreadySeen)
            _ -> {
              // Apply filters
              let filter_result =
                video_filter.should_download(
                  video,
                  state.config,
                  None,
                  timestamp,
                )

              // Mark as seen and queue if passed
              case filter_result {
                subscription_types.PassedFilter -> {
                  // Create a download job
                  let job_id = generate_job_id()
                  let job_id_str = types.job_id_to_string(job_id)

                  case repo.insert_job(state.db, job_id, video.url, timestamp) {
                    Ok(_) -> {
                      // Mark as seen with job reference
                      let _ =
                        subscription_repo.mark_seen(
                          state.db,
                          video,
                          True,
                          None,
                          Some(job_id_str),
                          timestamp,
                        )
                      io.println(
                        "Queued: "
                        <> video.title
                        <> " ["
                        <> video.video_id
                        <> "]",
                      )
                      #(video, subscription_types.PassedFilter)
                    }
                    Error(_) -> {
                      io.println("Failed to create job for: " <> video.title)
                      #(
                        video,
                        subscription_types.SkippedExcludedKeyword(
                          "job creation failed",
                        ),
                      )
                    }
                  }
                }
                _ -> {
                  // Mark as seen with skip reason
                  let skip_reason =
                    subscription_types.filter_result_to_string(filter_result)
                  let _ =
                    subscription_repo.mark_seen(
                      state.db,
                      video,
                      False,
                      Some(skip_reason),
                      None,
                      timestamp,
                    )
                  io.println(
                    "Skipped: " <> video.title <> " (" <> skip_reason <> ")",
                  )
                  #(video, filter_result)
                }
              }
            }
          }
        })

      // Compute statistics
      let new_videos =
        list.count(results, fn(r) {
          case r.1 {
            subscription_types.SkippedAlreadySeen -> False
            _ -> True
          }
        })

      let queued =
        list.count(results, fn(r) {
          case r.1 {
            subscription_types.PassedFilter -> True
            _ -> False
          }
        })

      let skipped =
        list.count(results, fn(r) { subscription_types.is_skipped(r.1) })

      PollResult(
        total_found: total_found,
        new_videos: new_videos,
        queued_for_download: queued,
        skipped: skipped,
        errors: [],
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

/// Generate a unique job ID
fn generate_job_id() -> types.JobId {
  let id = generate_uuid()
  types.new_job_id(id)
}

/// Generate UUID
fn generate_uuid() -> String {
  let bytes = crypto_strong_rand_bytes(16)
  bytes_to_hex(bytes)
}

@external(erlang, "crypto", "strong_rand_bytes")
fn crypto_strong_rand_bytes(n: Int) -> BitArray

fn bytes_to_hex(bytes: BitArray) -> String {
  bytes
  |> binary_to_hex_list
  |> list.map(int_to_hex_char)
  |> list.fold("", fn(acc, s) { acc <> s })
}

@external(erlang, "binary", "bin_to_list")
fn binary_to_hex_list(bytes: BitArray) -> List(Int)

fn int_to_hex_char(n: Int) -> String {
  let high = n / 16
  let low = n % 16
  hex_digit(high) <> hex_digit(low)
}

fn hex_digit(n: Int) -> String {
  case n {
    0 -> "0"
    1 -> "1"
    2 -> "2"
    3 -> "3"
    4 -> "4"
    5 -> "5"
    6 -> "6"
    7 -> "7"
    8 -> "8"
    9 -> "9"
    10 -> "a"
    11 -> "b"
    12 -> "c"
    13 -> "d"
    14 -> "e"
    _ -> "f"
  }
}

fn bool_to_string(b: Bool) -> String {
  case b {
    True -> "true"
    False -> "false"
  }
}

/// Get current Unix timestamp
@external(erlang, "os", "system_time")
fn get_timestamp() -> Int
