/// A bounded BEAM worker pool sharing one deadline across all channels.
import gleam/erlang/process.{type Pid, type Subject}
import gleam/int
import gleam/list

pub fn run(
  jobs: List(job),
  workers: Int,
  timeout_ms: Int,
  execute: fn(job, Int) -> Result(value, String),
  finished: fn(Result(value, String)) -> Nil,
) -> List(Result(value, String)) {
  let subject = process.new_subject()
  let pending = list.index_map(jobs, fn(job, index) { #(index, job) })
  let deadline = now() + timeout_ms
  dispatch(
    pending,
    [],
    [],
    subject,
    int.max(workers, 1),
    deadline,
    execute,
    finished,
  )
}

/// Run overlap-prone sources one at a time, then run independent channel
/// endpoints concurrently. Both stages consume one shared deadline.
pub fn run_ordered(
  serial_jobs: List(job),
  parallel_jobs: List(job),
  workers: Int,
  timeout_ms: Int,
  execute: fn(job, Int) -> Result(value, String),
  finished: fn(Result(value, String)) -> Nil,
) -> List(Result(value, String)) {
  let started = now()
  let serial = run(serial_jobs, 1, timeout_ms, execute, finished)
  let elapsed = now() - started
  let remaining = int.max(timeout_ms - elapsed, 0)
  let parallel = run(parallel_jobs, workers, remaining, execute, finished)
  list.append(serial, parallel)
}

fn dispatch(
  pending: List(#(Int, job)),
  running: List(#(Int, Pid)),
  outcomes: List(Result(value, String)),
  subject: Subject(#(Int, Result(value, String))),
  workers: Int,
  deadline: Int,
  execute: fn(job, Int) -> Result(value, String),
  finished: fn(Result(value, String)) -> Nil,
) -> List(Result(value, String)) {
  let remaining = deadline - now()
  case remaining > 0, pending, list.length(running) < workers {
    True, [#(index, job), ..rest], True -> {
      let pid =
        process.spawn(fn() {
          process.send(subject, #(
            index,
            execute(job, int.max(deadline - now(), 1)),
          ))
        })
      dispatch(
        rest,
        [#(index, pid), ..running],
        outcomes,
        subject,
        workers,
        deadline,
        execute,
        finished,
      )
    }
    _, [], _ if running == [] -> list.reverse(outcomes)
    False, _, _ -> {
      list.each(running, fn(worker) {
        process.unlink(worker.1)
        stop_worker(worker.1)
      })
      list.reverse([
        Error("parallel subscription deadline exceeded"),
        ..outcomes
      ])
    }
    _, _, _ -> {
      case process.receive(subject, remaining) {
        Ok(#(index, outcome)) -> {
          finished(outcome)
          dispatch(
            pending,
            list.filter(running, fn(worker) { worker.0 != index }),
            [outcome, ..outcomes],
            subject,
            workers,
            deadline,
            execute,
            finished,
          )
        }
        Error(_) ->
          dispatch(
            pending,
            running,
            outcomes,
            subject,
            workers,
            deadline,
            execute,
            finished,
          )
      }
    }
  }
}

@external(erlang, "subscription_manager_ffi", "stop_worker")
fn stop_worker(pid: Pid) -> Nil

type TimeUnit {
  Millisecond
}

@external(erlang, "erlang", "monotonic_time")
fn monotonic_time(unit: TimeUnit) -> Int

fn now() -> Int {
  monotonic_time(Millisecond)
}
