/// Tests for the bounded subscription executor.
import engine/shell
import engine/subscription_pool
import gleam/erlang/process.{type Subject}
import gleam/list
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

/// Keep two jobs blocked while confirming the third has not started. Once a
/// slot opens, the queued job starts and all three finish.
pub fn run_never_starts_more_jobs_than_worker_limit_test() {
  let started: Subject(Int) = process.new_subject()
  let finished: Subject(Result(Int, String)) = process.new_subject()
  let complete: Subject(List(Result(Int, String))) = process.new_subject()

  let executor = fn(job, _timeout_ms) {
    process.send(started, job)
    process.sleep(case job {
      1 -> 250
      2 -> 500
      _ -> 10
    })
    Ok(job)
  }
  let callback = fn(result) { process.send(finished, result) }
  let _runner =
    process.spawn_unlinked(fn() {
      let results =
        subscription_pool.run([1, 2, 3], 2, 5000, executor, callback)
      process.send(complete, results)
    })

  let first = process.receive(started, 1000) |> should.be_ok()
  let second = process.receive(started, 1000) |> should.be_ok()
  { first != second && first + second == 3 } |> should.be_true()

  // No third job can begin while both worker slots remain occupied.
  process.receive(started, 100) |> should.be_error()
  let completed = process.receive(finished, 1000) |> should.be_ok()
  case completed {
    Ok(1) -> Nil
    Ok(2) -> Nil
    _ -> should.fail()
  }
  process.receive(started, 1000) |> should.equal(Ok(3))

  let results = process.receive(complete, 2000) |> should.be_ok()
  list.length(results) |> should.equal(3)
  list.contains(results, Ok(1)) |> should.be_true()
  list.contains(results, Ok(2)) |> should.be_true()
  list.contains(results, Ok(3)) |> should.be_true()
}

/// The callback receives each result as soon as its job finishes, while the
/// pool remains active on another job.
pub fn run_reports_each_completion_before_batch_finishes_test() {
  let callbacks: Subject(Result(Int, String)) = process.new_subject()
  let complete: Subject(List(Result(Int, String))) = process.new_subject()
  let executor = fn(job, _timeout_ms) {
    let delay = case job {
      1 -> 50
      _ -> 700
    }
    process.sleep(delay)
    Ok(job)
  }
  let callback = fn(result) { process.send(callbacks, result) }
  let _runner =
    process.spawn_unlinked(fn() {
      let results = subscription_pool.run([1, 2], 2, 3000, executor, callback)
      process.send(complete, results)
    })

  process.receive(callbacks, 1000) |> should.equal(Ok(Ok(1)))
  process.receive(complete, 100) |> should.be_error()
  process.receive(callbacks, 1500) |> should.equal(Ok(Ok(2)))
  process.receive(complete, 1000) |> should.equal(Ok([Ok(1), Ok(2)]))
}

pub fn run_ordered_finishes_serial_jobs_before_parallel_jobs_test() {
  let started: Subject(Int) = process.new_subject()
  let complete: Subject(List(Result(Int, String))) = process.new_subject()
  let executor = fn(job, _timeout_ms) {
    process.send(started, job)
    process.sleep(10)
    Ok(job)
  }
  let callback = fn(_) { Nil }
  let _runner =
    process.spawn_unlinked(fn() {
      let results =
        subscription_pool.run_ordered(
          [1, 2],
          [3, 4],
          2,
          3000,
          executor,
          callback,
        )
      process.send(complete, results)
    })

  process.receive(started, 1000) |> should.equal(Ok(1))
  process.receive(started, 1000) |> should.equal(Ok(2))
  process.receive(started, 1000) |> should.equal(Ok(3))
  process.receive(started, 1000) |> should.equal(Ok(4))
  process.receive(complete, 2000)
  |> should.equal(Ok([Ok(1), Ok(2), Ok(3), Ok(4)]))
}

/// Results completed before the shared deadline remain available, and the
/// shell process tree owned by a timed-out executor is stopped.
pub fn run_deadline_preserves_partial_results_and_stops_shell_test() {
  let marker = new_pid_file()
  let callbacks: Subject(Result(String, String)) = process.new_subject()
  let executor = fn(job, _timeout_ms) {
    case job {
      "complete" -> Ok("complete")
      _ ->
        case
          shell.run_with_timeout(
            "sh",
            ["-c", "echo $$ > " <> marker <> "; exec sleep 30"],
            30_000,
          )
        {
          Ok(_) -> Ok("slow")
          Error(_) -> Error("shell timed out")
        }
    }
  }
  let callback = fn(result) { process.send(callbacks, result) }

  let results =
    subscription_pool.run(["complete", "slow"], 2, 700, executor, callback)

  results
  |> should.equal([
    Ok("complete"),
    Error("parallel subscription deadline exceeded"),
  ])
  process.receive(callbacks, 1000) |> should.equal(Ok(Ok("complete")))
  descendant_stopped(marker) |> should.be_true()
}

pub fn shared_failure_stops_serial_and_pending_parallel_jobs_test() {
  let started = process.new_subject()
  let results =
    subscription_pool.run_ordered_with_stop(
      [1, 2],
      [3, 4],
      2,
      3000,
      fn(job, _) {
        process.send(started, job)
        Error("blocked session")
      },
      fn(_) { Nil },
      fn(result) { result == Error("blocked session") },
    )
  results |> should.equal([Error("blocked session")])
  process.receive(started, 1000) |> should.equal(Ok(1))
  process.receive(started, 50) |> should.be_error()
}

pub fn shared_failure_drains_running_workers_before_returning_test() {
  let slow = process.new_subject()
  let blocked = process.new_subject()
  let complete = process.new_subject()
  let queued = process.new_subject()
  let _ =
    process.spawn_unlinked(fn() {
      let results =
        subscription_pool.run_with_stop(
          [1, 2, 3],
          2,
          5000,
          fn(job, _) {
            case job {
              1 -> {
                let release = process.new_subject()
                process.send(blocked, release)
                let _ = process.receive(release, 3000)
                Error("blocked session")
              }
              2 -> {
                process.send(slow, process.self())
                process.sleep(30_000)
                Ok(job)
              }
              _ -> {
                process.send(queued, Nil)
                Ok(job)
              }
            }
          },
          fn(_) { Nil },
          fn(result) { result == Error("blocked session") },
        )
      process.send(complete, results)
    })
  let pid = process.receive(slow, 2000) |> should.be_ok()
  let release = process.receive(blocked, 2000) |> should.be_ok()
  process.send(release, Nil)
  process.receive(complete, 2000)
  |> should.equal(Ok([Error("blocked session")]))
  process.is_alive(pid) |> should.be_false()
  process.receive(queued, 50) |> should.be_error()
}

@external(erlang, "shell_test_ffi", "new_pid_file")
fn new_pid_file() -> String

@external(erlang, "shell_test_ffi", "descendant_stopped")
fn descendant_stopped(path: String) -> Bool
