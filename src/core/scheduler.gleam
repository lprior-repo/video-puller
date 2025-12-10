/// Scheduler - BEAM-native Task Scheduling
///
/// Replaces infinite recursion polling with proper BEAM scheduling.
/// Uses process.send_after() for efficient, non-blocking scheduled messages.
///
/// This module provides a scheduler actor that:
/// - Schedules periodic tasks without stack growth
/// - Supports multiple named schedules
/// - Allows dynamic interval adjustment
/// - Provides jitter to prevent thundering herd
import gleam/dict.{type Dict}
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/result

/// Scheduler state
pub opaque type SchedulerState {
  SchedulerState(
    schedules: Dict(String, Schedule),
    self: Option(Subject(SchedulerMessage)),
  )
}

/// A scheduled task
pub type Schedule {
  Schedule(
    name: String,
    interval_ms: Int,
    target: Subject(ScheduledTick),
    enabled: Bool,
    jitter_percent: Int,
    last_tick: Int,
  )
}

/// Message sent to targets when their schedule ticks
pub type ScheduledTick {
  ScheduledTick(schedule_name: String, tick_number: Int)
}

/// Scheduler messages
pub type SchedulerMessage {
  // Register a new schedule
  RegisterSchedule(
    name: String,
    interval_ms: Int,
    target: Subject(ScheduledTick),
    jitter_percent: Int,
  )
  // Unregister a schedule
  UnregisterSchedule(name: String)
  // Enable/disable a schedule
  SetScheduleEnabled(name: String, enabled: Bool)
  // Update schedule interval
  UpdateInterval(name: String, interval_ms: Int)
  // Internal: tick a specific schedule
  Tick(name: String)
  // Set self reference
  SetSelf(Subject(SchedulerMessage))
  // Get all schedule statuses
  GetStatus(reply: Subject(List(ScheduleStatus)))
  // Shutdown
  Shutdown
}

/// Schedule status for monitoring
pub type ScheduleStatus {
  ScheduleStatus(name: String, interval_ms: Int, enabled: Bool, last_tick: Int)
}

/// Start the scheduler
pub fn start() -> Result(Subject(SchedulerMessage), actor.StartError) {
  let initial_state = SchedulerState(schedules: dict.new(), self: None)

  actor.start(
    actor.new(initial_state)
    |> actor.on_message(handle_message),
  )
  |> result.map(fn(started) {
    let subject = started.data
    process.send(subject, SetSelf(subject))
    subject
  })
}

/// Handle scheduler messages
fn handle_message(
  state: SchedulerState,
  message: SchedulerMessage,
) -> actor.Next(SchedulerState, SchedulerMessage) {
  case message {
    SetSelf(subject) -> {
      actor.continue(SchedulerState(..state, self: Some(subject)))
    }

    RegisterSchedule(name, interval_ms, target, jitter_percent) -> {
      let schedule =
        Schedule(
          name: name,
          interval_ms: interval_ms,
          target: target,
          enabled: True,
          jitter_percent: jitter_percent,
          last_tick: 0,
        )

      let new_schedules = dict.insert(state.schedules, name, schedule)

      io.println(
        "📅 Registered schedule: "
        <> name
        <> " (interval: "
        <> int.to_string(interval_ms)
        <> "ms)",
      )

      // Schedule first tick
      case state.self {
        Some(self) ->
          schedule_next_tick(self, name, interval_ms, jitter_percent)
        None -> Nil
      }

      actor.continue(SchedulerState(..state, schedules: new_schedules))
    }

    UnregisterSchedule(name) -> {
      io.println("📅 Unregistered schedule: " <> name)
      let new_schedules = dict.delete(state.schedules, name)
      actor.continue(SchedulerState(..state, schedules: new_schedules))
    }

    SetScheduleEnabled(name, enabled) -> {
      case dict.get(state.schedules, name) {
        Ok(schedule) -> {
          let updated = Schedule(..schedule, enabled: enabled)
          let new_schedules = dict.insert(state.schedules, name, updated)

          // If re-enabling, schedule next tick
          case enabled, state.self {
            True, Some(self) ->
              schedule_next_tick(
                self,
                name,
                schedule.interval_ms,
                schedule.jitter_percent,
              )
            _, _ -> Nil
          }

          actor.continue(SchedulerState(..state, schedules: new_schedules))
        }
        Error(_) -> actor.continue(state)
      }
    }

    UpdateInterval(name, interval_ms) -> {
      case dict.get(state.schedules, name) {
        Ok(schedule) -> {
          let updated = Schedule(..schedule, interval_ms: interval_ms)
          let new_schedules = dict.insert(state.schedules, name, updated)
          io.println(
            "📅 Updated schedule "
            <> name
            <> " interval to "
            <> int.to_string(interval_ms)
            <> "ms",
          )
          actor.continue(SchedulerState(..state, schedules: new_schedules))
        }
        Error(_) -> actor.continue(state)
      }
    }

    Tick(name) -> {
      case dict.get(state.schedules, name) {
        Ok(schedule) -> {
          case schedule.enabled {
            True -> {
              // Send tick to target
              let tick_number = schedule.last_tick + 1
              process.send(
                schedule.target,
                ScheduledTick(schedule_name: name, tick_number: tick_number),
              )

              // Update last tick
              let updated = Schedule(..schedule, last_tick: tick_number)
              let new_schedules = dict.insert(state.schedules, name, updated)

              // Schedule next tick using BEAM's native scheduler
              case state.self {
                Some(self) ->
                  schedule_next_tick(
                    self,
                    name,
                    schedule.interval_ms,
                    schedule.jitter_percent,
                  )
                None -> Nil
              }

              actor.continue(SchedulerState(..state, schedules: new_schedules))
            }
            False -> actor.continue(state)
          }
        }
        Error(_) -> actor.continue(state)
      }
    }

    GetStatus(reply) -> {
      let statuses =
        dict.to_list(state.schedules)
        |> list.map(fn(entry) {
          let #(_, schedule) = entry
          ScheduleStatus(
            name: schedule.name,
            interval_ms: schedule.interval_ms,
            enabled: schedule.enabled,
            last_tick: schedule.last_tick,
          )
        })
      process.send(reply, statuses)
      actor.continue(state)
    }

    Shutdown -> {
      io.println("📅 Scheduler shutting down")
      actor.stop()
    }
  }
}

/// Schedule the next tick using BEAM's native send_after
/// This is THE KEY FUNCTION that properly uses the BEAM scheduler
fn schedule_next_tick(
  scheduler: Subject(SchedulerMessage),
  name: String,
  interval_ms: Int,
  jitter_percent: Int,
) -> Nil {
  // Add jitter to prevent thundering herd
  let jitter_amount = case jitter_percent > 0 {
    True -> {
      let max_jitter = { interval_ms * jitter_percent } / 100
      random_int(0, max_jitter)
    }
    False -> 0
  }

  let actual_interval = interval_ms + jitter_amount

  // Use BEAM's native scheduler via send_after
  // This does NOT block or consume stack space!
  let _ = send_after(scheduler, actual_interval, Tick(name))
  Nil
}

/// Send a message after a delay using Erlang's timer:send_after
/// This is the BEAM-native way to schedule messages
@external(erlang, "timer", "send_after")
fn send_after(
  target: Subject(SchedulerMessage),
  delay_ms: Int,
  message: SchedulerMessage,
) -> Result(Nil, Nil)

/// Generate a random integer in range [min, max]
fn random_int(min: Int, max: Int) -> Int {
  case max > min {
    True -> min + erlang_random_uniform(max - min + 1) - 1
    False -> min
  }
}

/// Erlang's random:uniform/1 - returns 1..N
@external(erlang, "rand", "uniform")
fn erlang_random_uniform(n: Int) -> Int
