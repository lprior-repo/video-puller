/// Tests for the shell execution primitive
import engine/shell
import envoy
import gleam/list
import gleam/result
import gleam/string
import gleeunit
import gleeunit/should
import simplifile

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

/// The timeout error carries the newest output, not the oldest: where a
/// stalled engine started says nothing about where it got stuck
pub fn run_with_timeout_reports_newest_output_test() {
  case
    shell.run_with_timeout(
      "sh",
      ["-c", "echo opened; echo working; echo stalled; sleep 30"],
      1500,
    )
  {
    Error(shell.ExecutionError(message)) -> {
      string.contains(message, "stalled") |> should.be_true()
      string.contains(message, "opened") |> should.be_false()
    }
    _ -> should.fail()
  }
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

pub fn run_streaming_uses_exit_status_after_eof_test() {
  shell.run_streaming("sh", ["-c", "exec 1>&- 2>&-; sleep 0.1; exit 7"], fn(_) {
    Nil
  })
  |> should.equal(Ok(7))
}

pub fn run_streaming_returns_success_test() {
  shell.run_streaming("echo", ["hello"], fn(_) { Nil })
  |> should.equal(Ok(0))
}

@external(erlang, "shell_test_ffi", "new_pid_file")
fn new_pid_file() -> String

@external(erlang, "shell_test_ffi", "descendant_stopped")
fn descendant_stopped(path: String) -> Bool

@external(erlang, "shell_test_ffi", "guardian_kills_tree")
fn guardian_kills_tree() -> Bool

/// A child shell starts a grandchild which ignores closed stdout. Checking its
/// PID catches the old direct-children-only cleanup without global pgrep names.
pub fn run_with_timeout_kills_grandchildren_test() {
  let path = new_pid_file()
  let result =
    shell.run_with_timeout(
      "sh",
      [
        "-c",
        "sh -c 'sh -c '\"'\"'echo $$ > "
          <> path
          <> " ; exec sleep 30'\"'\"' & wait' & wait",
      ],
      500,
    )
  case result {
    Error(shell.ExecutionError(_)) -> Nil
    _ -> should.fail()
  }
  descendant_stopped(path) |> should.be_true()
}

pub fn killed_stream_owner_does_not_orphan_children_test() {
  guardian_kills_tree() |> should.be_true()
}

pub fn orphan_recovery_verifies_owner_and_process_fingerprints_test() {
  let root = new_pid_file() <> "-registry"
  let data_dir = root <> "/data"
  let registry_dir = data_dir <> "/run/processes"
  let executable = root <> "/yt-dlp"
  let malformed_record = registry_dir <> "/malformed.record"
  let old_data_dir = envoy.get("DATA_DIR")
  let _ = simplifile.create_directory_all(root)
  let _ =
    simplifile.write(
      executable,
      "#!/bin/sh\nsleep 60 &\nchild=$!\nprintf '%s %s\\n' \"$$\" \"$child\"\nwait\n",
    )
  let _ = shell.run("chmod", ["+x", executable])
  let _ = set_env("DATA_DIR", data_dir)

  case shell.open_stream(executable, []) {
    Error(_) -> should.fail()
    Ok(stream) -> {
      let pids = case shell.read_stream_line(stream) {
        shell.OutputLine(value) -> value
        _ -> ""
      }
      case string.split(pids, " ") {
        [root_pid, child_pid] -> {
          process_running(root_pid) |> should.be_true()
          process_running(child_pid) |> should.be_true()

          // A live owner keeps its downloader running during a startup scan.
          shell.recover_orphan_downloads() |> should.equal(0)
          process_running(root_pid) |> should.be_true()

          let _ = simplifile.write(malformed_record, "not a registry record\n")
          shell.recover_orphan_downloads() |> should.equal(0)
          simplifile.is_file(malformed_record) |> should.equal(Ok(True))

          let record =
            simplifile.read_directory(registry_dir)
            |> result.unwrap([])
            |> list.find(fn(name) {
              name != "malformed.record" && string.ends_with(name, ".record")
            })
            |> result.unwrap("")
          let record_path = registry_dir <> "/" <> record
          let contents = simplifile.read(record_path) |> result.unwrap("")
          let stale_owner =
            contents
            |> string.split("\n")
            |> list.map(fn(line) {
              case string.starts_with(line, "vm_pid=") {
                True -> "vm_pid=2147483647"
                False ->
                  case string.starts_with(line, "vm_fp=") {
                    True ->
                      "vm_fp=0000000000000000000000000000000000000000000000000000000000000000"
                    False -> line
                  }
              }
            })
            |> string.join("\n")
          let _ = simplifile.write(record_path, stale_owner)

          // Only the registered root with matching birth/command hashes is killed.
          shell.recover_orphan_downloads() |> should.equal(1)
          process_running(root_pid) |> should.be_false()
          process_running(child_pid) |> should.be_false()
          simplifile.is_file(record_path) |> should.equal(Ok(False))
          simplifile.is_file(malformed_record) |> should.equal(Ok(True))
          shell.close_stream(stream)
        }
        _ -> should.fail()
      }
    }
  }

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

@external(erlang, "shell_test_ffi", "set_env")
fn set_env(name: String, value: String) -> Bool

@external(erlang, "shell_test_ffi", "unset_env")
fn unset_env(name: String) -> Bool

@external(erlang, "shell_test_ffi", "process_running")
fn process_running(pid: String) -> Bool

@external(erlang, "shell_test_ffi", "delete_tree")
fn delete_tree(path: String) -> Bool
