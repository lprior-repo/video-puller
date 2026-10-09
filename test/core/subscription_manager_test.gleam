/// Deterministic actor lifecycle tests without network or downloader binaries.
import core/subscription_manager.{type SubscriptionMessage}
import domain/subscription_types.{type SubscriptionConfig, SubscriptionConfig}
import engine/ytdl_sub
import gleam/erlang/process.{type Pid, type Subject}
import gleam/option.{None, Some}
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

pub fn phase_timeouts_never_exceed_remaining_deadline_test() {
  subscription_manager.phase_timeouts(500, 1_800_000)
  |> should.equal(#(125, 375))
  subscription_manager.phase_timeouts(0, 1_800_000)
  |> should.equal(#(0, 0))
  subscription_manager.phase_timeouts(-1, 1_800_000)
  |> should.equal(#(0, 0))
}
