/// Deterministic actor lifecycle tests without network or downloader binaries.
import core/subscription_manager.{type SubscriptionMessage}
import domain/subscription_types.{
  type SubscriptionConfig, type SubscriptionStatus, SubscriptionConfig,
}
import engine/recovery_store
import engine/shell
import engine/ytdl_sub
import envoy
import gleam/erlang/process.{type Name, type Pid, type Subject}
import gleam/int
import gleam/option.{None, Some}
import gleam/string
import gleeunit/should
import infra/db
import infra/migrator
import simplifile

type RunningPoll {
  RunningPoll(pid: Pid, finish: Subject(Nil))
}

fn setup(name: String) -> #(db.Db, String) {
  let path = "/tmp/test_subscription_lifecycle_" <> name <> ".db"
  let _ = simplifile.delete(path)
  let conn = db.init_db(path) |> should.be_ok()
  migrator.run_migrations(conn) |> should.be_ok()
  #(conn, path)
}

fn config(enabled: Bool) -> SubscriptionConfig {
  SubscriptionConfig(
    enabled: enabled,
    poll_interval_minutes: 360,
    last_poll_at: None,
  )
}

fn status(subject: Subject(SubscriptionMessage)) {
  let reply = process.new_subject()
  process.send(subject, subscription_manager.GetStatus(reply))
  process.receive(reply, 2000) |> should.be_ok()
}

fn start_manager(conn: db.Db, started: Subject(RunningPoll)) {
  let runner = fn() {
    let finish = process.new_subject()
    process.send(started, RunningPoll(process.self(), finish))
    let _ = process.receive(finish, 30_000)
    Ok(ytdl_sub.PullSummary(0, [], []))
  }
  let subject =
    subscription_manager.start_with_runner(conn, runner, 30_000)
    |> should.be_ok()
  process.send(subject, subscription_manager.UpdateConfig(config(True)))
  subject
}

fn cleanup(subject: Subject(SubscriptionMessage), conn: db.Db, path: String) {
  process.send(subject, subscription_manager.Shutdown)
  // The actor may have a poll completion queued, so close its database only
  // after it has processed the shutdown message.
  process.sleep(50)
  let _ = db.close(conn)
  let _ = simplifile.delete(path)
  Nil
}

pub fn settings_change_preserves_active_watchdog_test() {
  let #(conn, path) = setup("settings_watchdog")
  let started = process.new_subject()
  let subject = start_manager(conn, started)
  process.send(subject, subscription_manager.Poll)
  let worker = process.receive(started, 2000) |> should.be_ok()

  process.send(subject, subscription_manager.UpdateConfig(config(True)))
  let polling = status(subject)
  polling.is_polling |> should.be_true()
  polling.next_poll_at |> should.equal(None)

  // Settings may replace the schedule generation, never this poll's identity.
  process.send(subject, subscription_manager.PollWatchdog(1))
  let finished = status(subject)
  finished.is_polling |> should.be_false()
  process.is_alive(worker.pid) |> should.be_false()
  let assert Some(result) = finished.last_result
  result.errors
  |> should.equal(["subscription poll worker did not finish"])

  cleanup(subject, conn, path)
}

pub fn late_completion_and_watchdog_cannot_finish_new_poll_test() {
  let #(conn, path) = setup("stale_completion")
  let started = process.new_subject()
  let subject = start_manager(conn, started)
  process.send(subject, subscription_manager.Poll)
  let first = process.receive(started, 2000) |> should.be_ok()
  process.send(subject, subscription_manager.PollWatchdog(1))
  status(subject).is_polling |> should.be_false()
  process.is_alive(first.pid) |> should.be_false()

  process.send(subject, subscription_manager.Poll)
  let second = process.receive(started, 2000) |> should.be_ok()
  process.send(subject, subscription_manager.PollFinished(1, Error("stale")))
  process.send(subject, subscription_manager.PollWatchdog(1))
  let polling = status(subject)
  polling.is_polling |> should.be_true()
  process.is_alive(second.pid) |> should.be_true()
  let assert Some(result) = polling.last_result
  result.errors
  |> should.equal(["subscription poll worker did not finish"])

  cleanup(subject, conn, path)
  process.is_alive(second.pid) |> should.be_false()
}

pub fn disabled_schedule_clears_next_poll_and_keeps_active_watchdog_test() {
  let #(conn, path) = setup("disable")
  let started = process.new_subject()
  let subject = start_manager(conn, started)
  let assert Some(_) = status(subject).next_poll_at
  process.send(subject, subscription_manager.UpdateConfig(config(False)))
  status(subject).next_poll_at |> should.equal(None)

  process.send(subject, subscription_manager.UpdateConfig(config(True)))
  process.send(subject, subscription_manager.Poll)
  let worker = process.receive(started, 2000) |> should.be_ok()
  process.send(subject, subscription_manager.UpdateConfig(config(False)))
  process.send(subject, subscription_manager.PollWatchdog(1))
  let finished = status(subject)
  finished.is_polling |> should.be_false()
  finished.next_poll_at |> should.equal(None)
  process.is_alive(worker.pid) |> should.be_false()

  cleanup(subject, conn, path)
}

pub fn completed_poll_ignores_duplicate_completion_test() {
  let #(conn, path) = setup("duplicate_completion")
  let started = process.new_subject()
  let subject = start_manager(conn, started)
  process.send(subject, subscription_manager.Poll)
  let worker = process.receive(started, 2000) |> should.be_ok()
  process.send(worker.finish, Nil)
  wait_for_completion(subject, 100)
  let finished = status(subject)

  process.send(
    subject,
    subscription_manager.PollFinished(1, Error("duplicate")),
  )
  let unchanged = status(subject)
  unchanged.last_result |> should.equal(finished.last_result)
  unchanged.next_poll_at |> should.equal(finished.next_poll_at)

  cleanup(subject, conn, path)
}

fn wait_for_completion(subject: Subject(SubscriptionMessage), attempts: Int) {
  case status(subject).is_polling, attempts {
    False, _ -> Nil
    True, 0 -> should.fail()
    True, _ -> {
      process.sleep(10)
      wait_for_completion(subject, attempts - 1)
    }
  }
}

fn wait_for_worker_failure(
  subject: Subject(SubscriptionMessage),
  attempts: Int,
) -> SubscriptionStatus {
  let current = status(subject)
  case current.is_polling, current.last_result, attempts {
    False, Some(_), _ -> current
    _, _, 0 -> panic as "worker crash was not handled before the watchdog"
    _, _, _ -> {
      process.sleep(10)
      wait_for_worker_failure(subject, attempts - 1)
    }
  }
}

pub fn phase_timeouts_never_exceed_remaining_deadline_test() {
  subscription_manager.phase_timeouts(500, 1_800_000)
  |> should.equal(#(125, 375))
  subscription_manager.phase_timeouts(0, 1_800_000)
  |> should.equal(#(0, 0))
  subscription_manager.phase_timeouts(-1, 1_800_000)
  |> should.equal(#(0, 0))
}

pub fn next_poll_delay_uses_poll_start_and_clamps_elapsed_test() {
  subscription_manager.next_poll_delay(1000, 1100, 360)
  |> should.equal(21_500)
  subscription_manager.next_poll_delay(1000, 50_000, 360)
  |> should.equal(1)
  subscription_manager.next_poll_delay(1000, 900, 360)
  |> should.equal(21_600)
  subscription_manager.next_poll_delay(1000, 1000, 0)
  |> should.equal(60)
}

pub fn failed_poll_arms_a_short_retry_test() {
  let #(conn, path) = setup("failed_retry")
  let runner = fn() { Error("network unavailable") }
  let subject =
    subscription_manager.start_with_runner(conn, runner, 30_000)
    |> should.be_ok()
  process.send(subject, subscription_manager.UpdateConfig(config(True)))
  process.send(subject, subscription_manager.Poll)
  wait_for_completion(subject, 100)
  let finished = status(subject)
  let assert Some(last_poll) = finished.last_poll_at
  let assert Some(next_poll) = finished.next_poll_at
  next_poll - last_poll |> should.equal(300)
  let assert Some(result) = finished.last_result
  result.errors |> should.equal(["network unavailable"])
  cleanup(subject, conn, path)
}

pub fn crashed_poll_worker_retries_without_waiting_for_watchdog_test() {
  let #(conn, path) = setup("worker_crash")
  let started = process.new_subject()
  let runner = fn() {
    process.send(started, Nil)
    panic as "simulated poll worker crash"
  }
  let subject =
    subscription_manager.start_with_runner(conn, runner, 30_000)
    |> should.be_ok()
  process.send(subject, subscription_manager.UpdateConfig(config(True)))
  process.send(subject, subscription_manager.Poll)
  process.receive(started, 2000) |> should.be_ok()

  let failed = wait_for_worker_failure(subject, 100)
  failed.is_polling |> should.be_false()
  let assert Some(last_poll) = failed.last_poll_at
  let assert Some(next_poll) = failed.next_poll_at
  next_poll - last_poll |> should.equal(300)
  let assert Some(result) = failed.last_result
  case result.errors {
    [error] ->
      string.starts_with(error, "subscription poll worker exited:")
      |> should.be_true()
    _ -> should.fail()
  }
  cleanup(subject, conn, path)
}

pub fn successful_partial_poll_keeps_regular_cadence_test() {
  let #(conn, path) = setup("partial_errors")
  let runner = fn() {
    Ok(ytdl_sub.PullSummary(0, [], ["one private video was skipped"]))
  }
  let subject =
    subscription_manager.start_with_runner(conn, runner, 30_000)
    |> should.be_ok()
  process.send(subject, subscription_manager.UpdateConfig(config(True)))
  process.send(subject, subscription_manager.Poll)
  wait_for_completion(subject, 100)
  let finished = status(subject)
  let assert Some(last_poll) = finished.last_poll_at
  let assert Some(next_poll) = finished.next_poll_at
  let delay = next_poll - last_poll
  should.be_true(delay >= 21_000 && delay <= 21_600)
  let assert Some(result) = finished.last_result
  result.errors |> should.equal(["one private video was skipped"])
  cleanup(subject, conn, path)
}

pub fn partial_success_with_recoverable_errors_automatically_retries_test() {
  let #(conn, path) = setup("automatic_partial_retry")
  let called = process.new_subject()
  let runner = fn() {
    let outcomes = process.new_subject()
    process.send(called, outcomes)
    let assert Ok(outcome) = process.receive(outcomes, 1000)
    outcome
  }
  let subject =
    subscription_manager.start_with_retry_runner(conn, runner, 30_000, 1000)
    |> should.be_ok()
  process.send(subject, subscription_manager.UpdateConfig(config(True)))
  process.send(subject, subscription_manager.Poll)
  let first = process.receive(called, 1000) |> should.be_ok()
  process.send(
    first,
    Ok(
      ytdl_sub.PullSummary(0, [], [
        "Sign in to confirm you're not a bot",
      ]),
    ),
  )
  // No manual trigger: the error-bearing Ok result must schedule this retry.
  let second = process.receive(called, 3000) |> should.be_ok()
  process.send(second, Ok(ytdl_sub.PullSummary(0, [], [])))
  wait_for_completion(subject, 100)
  let finished = status(subject)
  let assert Some(result) = finished.last_result
  result.errors |> should.equal([])
  let assert Some(last) = finished.last_poll_at
  let assert Some(next) = finished.next_poll_at
  { next - last > 20_000 } |> should.be_true()
  cleanup(subject, conn, path)
}

pub fn crashed_worker_automatically_retries_and_recovers_test() {
  let #(conn, path) = setup("automatic_crash_retry")
  let called = process.new_subject()
  let runner = fn() {
    let outcomes = process.new_subject()
    process.send(called, outcomes)
    let assert Ok(crash) = process.receive(outcomes, 1000)
    case crash {
      True -> panic as "injected first-attempt crash"
      False -> Ok(ytdl_sub.PullSummary(0, [], []))
    }
  }
  let subject =
    subscription_manager.start_with_retry_runner(conn, runner, 30_000, 1000)
    |> should.be_ok()
  process.send(subject, subscription_manager.UpdateConfig(config(True)))
  process.send(subject, subscription_manager.Poll)
  let first = process.receive(called, 1000) |> should.be_ok()
  process.send(first, True)
  let second = process.receive(called, 3000) |> should.be_ok()
  process.send(second, False)
  wait_for_completion(subject, 100)
  let assert Some(result) = status(subject).last_result
  result.errors |> should.equal([])
  cleanup(subject, conn, path)
}

pub fn restart_preserves_backoff_and_resumes_without_manual_poll_test() {
  let #(conn, path) = setup("restart_retry")
  let root = new_pid_file() <> "-retry-state"
  let old_data_dir = envoy.get("DATA_DIR")
  let _ = set_env("DATA_DIR", root)
  let failed =
    subscription_manager.start_with_retry_runner(
      conn,
      fn() { Error("network unavailable") },
      30_000,
      3000,
    )
    |> should.be_ok()
  process.send(failed, subscription_manager.UpdateConfig(config(True)))
  process.send(failed, subscription_manager.Poll)
  wait_for_completion(failed, 100)
  let persisted = recovery_store.load()
  persisted.failures |> should.equal(1)
  process.send(failed, subscription_manager.Shutdown)
  process.sleep(50)

  let called = process.new_subject()
  let resumed =
    subscription_manager.start_with_retry_runner(
      conn,
      fn() {
        process.send(called, Nil)
        Ok(ytdl_sub.PullSummary(0, [], []))
      },
      30_000,
      3000,
    )
    |> should.be_ok()
  process.send(resumed, subscription_manager.Resume)
  process.receive(called, 100) |> should.be_error()
  status(resumed).next_poll_at |> should.equal(Some(persisted.retry_at))
  process.receive(called, 4000) |> should.be_ok()
  wait_for_completion(resumed, 100)
  recovery_store.load().status |> should.equal("healthy")
  recovery_store.load().failures |> should.equal(0)
  cleanup(resumed, conn, path)
  case old_data_dir {
    Ok(value) -> {
      let _ = set_env("DATA_DIR", value)
      Nil
    }
    Error(_) -> {
      let _ = unset_env("DATA_DIR")
      Nil
    }
  }
  let _ = delete_tree(root)
}

type PollStarted {
  PollStarted(root_pid: String, child_pid: String)
}

pub fn named_manager_restart_waits_for_old_downloader_cleanup_test() {
  let #(conn, db_path) = setup("named_restart_cleanup")
  let root = new_pid_file() <> "-named-manager"
  let executable = root <> "/yt-dlp"
  let data_dir = root <> "/data"
  let old_data_dir = envoy.get("DATA_DIR")
  let _ = simplifile.create_directory_all(root)
  let _ =
    simplifile.write(
      executable,
      "#!/bin/sh\nsleep 60 &\nchild=$!\nprintf '%s %s\\n' \"$$\" \"$child\"\nwait\n",
    )
  let _ = shell.run("chmod", ["+x", executable])
  let _ = set_env("DATA_DIR", data_dir)

  let name: Name(SubscriptionMessage) =
    process.new_name(prefix: "test_named_subscription_manager")
  let started = process.new_subject()
  let runner = fn() {
    case shell.open_stream(executable, []) {
      Error(_) -> Error("could not open fake downloader")
      Ok(stream) -> {
        let pid_line = case shell.read_stream_line(stream) {
          shell.OutputLine(line) -> line
          _ -> ""
        }
        case string.split(pid_line, " ") {
          [root_pid, child_pid] -> {
            process.send(started, PollStarted(root_pid, child_pid))
            case shell.read_stream_line(stream) {
              shell.StreamError(message) -> Error(message)
              shell.EndOfStream -> Error("fake downloader ended")
              shell.ProcessExit(code) ->
                Error("fake downloader exit " <> int.to_string(code))
              shell.OutputLine(_) -> Error("unexpected fake downloader output")
            }
          }
          _ -> Error("fake downloader did not report its pids")
        }
      }
    }
  }
  let manager =
    subscription_manager.start_named_with_runner(conn, name, runner, 30_000)
    |> should.be_ok()
  process.send(manager, subscription_manager.UpdateConfig(config(True)))
  process.send(manager, subscription_manager.Poll)
  let assert Ok(PollStarted(root_pid, child_pid)) =
    process.receive(started, 3000)
  process_running(root_pid) |> should.be_true()
  process_running(child_pid) |> should.be_true()

  // Lose the guardian first: its restored ownership journal must still stop
  // the old downloader when the manager then crashes and restarts.
  test_kill_poll_guard()
  // Simulate a manager crash. The stable-name restart must block until its
  // guardian has stopped the worker and verified downloader process tree.
  let assert Ok(old_manager_pid) = process.named(name)
  process.unlink(old_manager_pid)
  let gone = process.monitor(old_manager_pid)
  process.kill(old_manager_pid)
  process.new_selector()
  |> process.select_specific_monitor(gone, fn(_) { Nil })
  |> process.selector_receive(2000)
  |> should.be_ok()
  let restarted =
    subscription_manager.start_named(conn, name)
    |> should.be_ok()
  process_running(root_pid) |> should.be_false()
  process_running(child_pid) |> should.be_false()
  status(restarted).is_polling |> should.be_false()

  cleanup(restarted, conn, db_path)
  case old_data_dir {
    Ok(value) -> {
      let _ = set_env("DATA_DIR", value)
      Nil
    }
    Error(_) -> {
      let _ = unset_env("DATA_DIR")
      Nil
    }
  }
  let _ = delete_tree(root)
}

@external(erlang, "service_supervisor_test_ffi", "kill_poll_guard")
fn test_kill_poll_guard() -> Nil

@external(erlang, "shell_test_ffi", "set_env")
fn set_env(name: String, value: String) -> Bool

@external(erlang, "shell_test_ffi", "unset_env")
fn unset_env(name: String) -> Bool

@external(erlang, "shell_test_ffi", "process_running")
fn process_running(pid: String) -> Bool

@external(erlang, "shell_test_ffi", "new_pid_file")
fn new_pid_file() -> String

@external(erlang, "shell_test_ffi", "delete_tree")
fn delete_tree(path: String) -> Bool
