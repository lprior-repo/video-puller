/// Tests for the shell execution primitive
import engine/shell
import gleam/string
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

@external(erlang, "erlang", "monotonic_time")
fn monotonic_time(unit: MonotonicUnit) -> Int

type MonotonicUnit {
  Millisecond
}

fn monotonic_milliseconds() -> Int {
  monotonic_time(Millisecond)
}

/// The deadline spans the whole command: a command that keeps emitting output
/// cannot push the deadline forward, so it is killed once the budget is spent
pub fn run_with_timeout_enforces_total_deadline_test() {
  let started = monotonic_milliseconds()

  let result =
    shell.run_with_timeout(
      "sh",
      [
        "-c",
        "i=0; while [ $i -lt 20 ]; do echo tick; sleep 0.3; i=$((i+1)); done",
      ],
      1000,
    )

  let elapsed = monotonic_milliseconds() - started

  case result {
    Error(shell.ExecutionError(message)) -> {
      // The tail of the output is carried so the caller can see why it stalled
      string.starts_with(message, "Command timeout exceeded")
      |> should.be_true()
      string.contains(message, "tick") |> should.be_true()
    }
    _ -> should.fail()
  }

  { elapsed < 5000 } |> should.be_true()
}

pub fn run_with_timeout_returns_output_for_fast_command_test() {
  case shell.run_with_timeout("echo", ["hello"], 5000) {
    Ok(shell.ShellResult(exit_code, stdout, _stderr)) -> {
      exit_code |> should.equal(0)
      stdout |> should.equal("hello")
    }
    Error(_) -> should.fail()
  }
}

/// EOF says the output ended, not that the command succeeded: the exit status
/// must still decide the result
pub fn run_with_timeout_uses_exit_status_after_eof_test() {
  case
    shell.run_with_timeout(
      "sh",
      ["-c", "exec 1>&- 2>&-; sleep 0.3; exit 7"],
      5000,
    )
  {
    Ok(shell.ShellResult(exit_code, _stdout, _stderr)) ->
      exit_code |> should.equal(7)
    Error(_) -> should.fail()
  }
}

/// A command that closes its output and keeps running still hits the deadline
pub fn run_with_timeout_kills_command_that_closes_output_test() {
  let started = monotonic_milliseconds()
  let result =
    shell.run_with_timeout("sh", ["-c", "exec 1>&- 2>&-; sleep 30"], 800)

  case result {
    Error(shell.ExecutionError(message)) ->
      string.starts_with(message, "Command timeout exceeded")
      |> should.be_true()
    _ -> should.fail()
  }

  { monotonic_milliseconds() - started < 5000 } |> should.be_true()
}

/// A timed-out command that ignores EOF is killed, not left running in the
/// background (the bracket keeps pgrep from matching its own command line)
pub fn run_with_timeout_kills_child_process_tree_test() {
  let marker = "vp-shell-kill-marker"
  let _ =
    shell.run_with_timeout(
      "sh",
      ["-c", "exec 1>&- 2>&-; sleep 30 # " <> marker],
      800,
    )

  case
    shell.run_with_timeout(
      "sh",
      ["-c", "pgrep -f vp-shell-kill-marke[r]"],
      5000,
    )
  {
    // pgrep exits 1 when nothing matches: the shell is gone
    Ok(shell.ShellResult(exit_code, _, _)) -> exit_code |> should.equal(1)
    Error(_) -> should.fail()
  }
}
