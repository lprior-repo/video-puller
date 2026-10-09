/// Shell command execution primitive
///
/// Provides safe shell command execution using shellout and streaming via Erlang ports.
/// CRITICAL: All inputs must be sanitized to prevent shell injection (INV-001).
import gleam/erlang/port.{type Port}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import shellout

/// Result of a shell command execution
pub type ShellResult {
  ShellResult(exit_code: Int, stdout: String, stderr: String)
}

/// Error type for shell operations
pub type ShellError {
  ExecutionError(String)
  InvalidCommand(String)
}

/// Execute a shell command safely
///
/// This function uses os.run to execute commands. The command and arguments
/// are passed separately to avoid shell injection issues.
///
/// ## Examples
///
/// ```gleam
/// run("echo", ["hello"])
/// // -> Ok(ShellResult(0, "hello\n", ""))
/// ```
pub fn run(
  command: String,
  args: List(String),
) -> Result(ShellResult, ShellError) {
  // Validate command doesn't contain shell metacharacters
  case validate_command(command) {
    Error(e) -> Error(e)
    Ok(_) -> {
      case shellout.command(run: command, with: args, in: ".", opt: []) {
        Ok(output) -> Ok(ShellResult(exit_code: 0, stdout: output, stderr: ""))
        Error(#(code, stderr)) ->
          Ok(ShellResult(exit_code: code, stdout: "", stderr: stderr))
      }
    }
  }
}

/// Execute a shell command with a timeout (in milliseconds)
///
/// If the command doesn't complete within the timeout, it returns an error.
///
/// ## Examples
///
/// ```gleam
/// run_with_timeout("yt-dlp", ["--help"], 30_000)
/// // -> Ok(ShellResult(...)) or Error(ExecutionError("Timeout"))
/// ```
pub fn run_with_timeout(
  command: String,
  args: List(String),
  timeout_ms: Int,
) -> Result(ShellResult, ShellError) {
  // Validate command doesn't contain shell metacharacters
  case validate_command(command) {
    Error(e) -> Error(e)
    Ok(_) -> {
      // Use streaming execution with a total deadline: a command that keeps
      // emitting output must still be killed once the budget is used up
      use stream <- result.try(open_stream(command, args))
      let deadline = monotonic_milliseconds() + timeout_ms

      case read_with_deadline(stream, deadline, None, False, [], []) {
        Ok(#(exit_code, stdout_lines, stderr_lines)) -> {
          close_stream(stream)
          Ok(ShellResult(
            exit_code: exit_code,
            stdout: string.join(stdout_lines, "\n"),
            stderr: string.join(stderr_lines, "\n"),
          ))
        }
        Error(e) -> {
          // A timed-out command may ignore the port close and keep running;
          // kill it so it cannot keep downloading in the background
          kill_port_tree(stream)
          close_stream(stream)
          Error(e)
        }
      }
    }
  }
}

/// Read from a stream until it ends or the total deadline passes
///
/// The deadline spans the whole command rather than each line: a command that
/// keeps emitting output cannot extend its own budget by doing so. Erlang
/// gives no ordering guarantee between EOF and the exit status, so the status
/// is tracked separately: EOF never fabricates a zero exit.
fn read_with_deadline(
  stream: StreamingPort,
  deadline: Int,
  exit_code: Option(Int),
  saw_eof: Bool,
  stdout_acc: List(String),
  stderr_acc: List(String),
) -> Result(#(Int, List(String), List(String)), ShellError) {
  case remaining_time(deadline) {
    Error(_) ->
      case exit_code {
        Some(code) -> Ok(finish_result(code, stdout_acc, stderr_acc))
        None -> Error(timeout_error(stdout_acc))
      }
    Ok(remaining) ->
      case read_stream_line_with_timeout(stream, remaining) {
        Ok(OutputLine(line)) -> {
          // All output goes to stdout (stderr is harder to separate in ports)
          read_with_deadline(
            stream,
            deadline,
            exit_code,
            saw_eof,
            [line, ..stdout_acc],
            stderr_acc,
          )
        }
        Ok(ProcessExit(code)) ->
          case saw_eof {
            // Output already ended: nothing left to drain
            True -> Ok(finish_result(code, stdout_acc, stderr_acc))
            False ->
              // Continue reading to drain remaining output
              read_with_deadline(
                stream,
                deadline,
                Some(code),
                saw_eof,
                stdout_acc,
                stderr_acc,
              )
              |> result.map(fn(res) {
                let #(_, out, err) = res
                #(code, out, err)
              })
              |> result.or(Ok(finish_result(code, stdout_acc, stderr_acc)))
          }
        Ok(EndOfStream) ->
          case exit_code {
            Some(code) -> Ok(finish_result(code, stdout_acc, stderr_acc))
            // EOF only says the output ended; the exit status may still be
            // in flight, and the deadline still applies while waiting
            None ->
              read_with_deadline(
                stream,
                deadline,
                None,
                True,
                stdout_acc,
                stderr_acc,
              )
          }
        Ok(StreamError(_)) ->
          case exit_code {
            Some(code) -> Ok(finish_result(code, stdout_acc, stderr_acc))
            None -> Ok(finish_result(1, stdout_acc, stderr_acc))
          }
        Error(Timeout) ->
          case exit_code {
            // The status was already seen; it outranks the deadline
            Some(code) -> Ok(finish_result(code, stdout_acc, stderr_acc))
            None -> Error(timeout_error(stdout_acc))
          }
      }
  }
}

fn finish_result(
  code: Int,
  stdout_acc: List(String),
  stderr_acc: List(String),
) -> #(Int, List(String), List(String)) {
  #(code, list.reverse(stdout_acc), list.reverse(stderr_acc))
}

/// Milliseconds left until the deadline, or an error once it has passed
fn remaining_time(deadline: Int) -> Result(Int, Nil) {
  let remaining = deadline - monotonic_milliseconds()
  case remaining <= 0 {
    True -> Error(Nil)
    False -> Ok(remaining)
  }
}

const timeout_message = "Command timeout exceeded"

const timeout_tail_limit = 200

/// Timeout error carrying the tail of the output
///
/// A silent timeout is the hardest failure to diagnose: the caller otherwise
/// only learns that the command exceeded its deadline.
fn timeout_error(stdout_acc: List(String)) -> ShellError {
  ExecutionError(case output_tail(stdout_acc) {
    "" -> timeout_message
    tail -> timeout_message <> " (last output: " <> tail <> ")"
  })
}

/// The most recent non-empty output lines, oldest first, truncated
fn output_tail(lines: List(String)) -> String {
  let tail =
    lines
    |> list.reverse
    |> list.filter(fn(line) { !string.is_empty(string.trim(line)) })
    |> list.take(2)
    |> list.reverse
    |> string.join(" / ")
  case string.length(tail) > timeout_tail_limit {
    True -> string.slice(tail, 0, timeout_tail_limit) <> "…"
    False -> tail
  }
}

@external(erlang, "erlang", "monotonic_time")
fn monotonic_time(unit: MonotonicUnit) -> Int

fn monotonic_milliseconds() -> Int {
  monotonic_time(Millisecond)
}

type MonotonicUnit {
  Millisecond
}

/// Timeout error for stream reading
type TimeoutError {
  Timeout
}

/// Read a line from stream with timeout
fn read_stream_line_with_timeout(
  stream: StreamingPort,
  timeout_ms: Int,
) -> Result(StreamLine, TimeoutError) {
  // Use erlang receive with timeout
  do_read_line_timeout(stream.port, timeout_ms)
}

@external(erlang, "shell_ffi", "read_line_timeout")
fn do_read_line_timeout(
  port: Port,
  timeout_ms: Int,
) -> Result(StreamLine, TimeoutError)

/// Execute a command and return only stdout on success
pub fn run_simple(
  command: String,
  args: List(String),
) -> Result(String, ShellError) {
  use result <- result.try(run(command, args))
  case result.exit_code {
    0 -> Ok(string.trim(result.stdout))
    _ ->
      Error(ExecutionError(
        "Command exited with code: " <> int_to_string(result.exit_code),
      ))
  }
}

/// Validate that a command string doesn't contain dangerous characters
fn validate_command(command: String) -> Result(Nil, ShellError) {
  let dangerous_chars = [";", "|", "&", "$", "`", "(", ")", "<", ">", "\n"]

  let has_dangerous =
    list.any(dangerous_chars, fn(char) { string.contains(command, char) })

  case has_dangerous {
    True ->
      Error(InvalidCommand("Command contains shell metacharacters: " <> command))
    False -> Ok(Nil)
  }
}

/// Helper to convert int to string (avoiding import)
fn int_to_string(n: Int) -> String {
  case n {
    0 -> "0"
    1 -> "1"
    2 -> "2"
    _ -> "unknown"
  }
}

// ============================================================================
// Streaming Shell Execution using Erlang Ports
// ============================================================================

/// Line read from a streaming command
pub type StreamLine {
  /// A line of output from the command
  OutputLine(String)
  /// The command has finished
  EndOfStream
  /// The command exited with a status code
  ProcessExit(Int)
  /// An error occurred while reading
  StreamError(String)
}

/// Result of opening a streaming port
pub type StreamingPort {
  StreamingPort(port: Port)
}

/// Open a streaming port for a command
/// Returns a port that can be used to read lines as they arrive
@external(erlang, "shell_ffi", "open_streaming_port")
fn do_open_port(command: String, args: List(String)) -> Result(Port, String)

/// Read the next line from a streaming port
@external(erlang, "shell_ffi", "read_line")
fn do_read_line(port: Port) -> StreamLine

/// Close a streaming port
@external(erlang, "shell_ffi", "close_port")
fn do_close_port(port: Port) -> Nil

/// Kill the process behind a port and its children
@external(erlang, "shell_ffi", "kill_port_tree")
fn do_kill_port_tree(port: Port) -> Nil

/// Open a streaming shell command
///
/// This creates a port that streams output line-by-line as the command runs.
/// Use `read_stream_line` to read lines and `close_stream` to clean up.
///
/// ## Examples
///
/// ```gleam
/// case open_stream("nu", ["-c", "for i in 1..3 { print $i; sleep 1sec }"]) {
///   Ok(stream) -> {
///     // Read lines...
///     close_stream(stream)
///   }
///   Error(e) -> io.println("Failed: " <> e)
/// }
/// ```
pub fn open_stream(
  command: String,
  args: List(String),
) -> Result(StreamingPort, ShellError) {
  case validate_command(command) {
    Error(e) -> Error(e)
    Ok(_) ->
      case do_open_port(command, args) {
        Ok(port) -> Ok(StreamingPort(port))
        Error(msg) -> Error(ExecutionError(msg))
      }
  }
}

/// Read the next line from a streaming command
///
/// Returns one of:
/// - `OutputLine(text)` - A line of output
/// - `EndOfStream` - No more output (command may still be running)
/// - `ProcessExit(code)` - Command exited with status code
/// - `StreamError(msg)` - An error occurred
pub fn read_stream_line(stream: StreamingPort) -> StreamLine {
  do_read_line(stream.port)
}

/// Close a streaming port and clean up resources
pub fn close_stream(stream: StreamingPort) -> Nil {
  do_close_port(stream.port)
}

/// Kill the process behind a stream and its children
///
/// Used when a command ignores its deadline (ytdl-sub keeps downloading after
/// the port is closed, and holds its working-directory lock while it does).
fn kill_port_tree(stream: StreamingPort) -> Nil {
  do_kill_port_tree(stream.port)
}

/// Execute a command with streaming output, calling a callback for each line
///
/// The callback receives each line as it arrives. The function returns when
/// the command completes.
///
/// ## Examples
///
/// ```gleam
/// run_streaming("nu", ["-c", "for i in 1..5 { print $i }"], fn(line) {
///   io.println("Got: " <> line)
/// })
/// ```
pub fn run_streaming(
  command: String,
  args: List(String),
  callback: fn(String) -> Nil,
) -> Result(Int, ShellError) {
  use stream <- result.try(open_stream(command, args))

  let exit_code = stream_loop(stream, callback, 0)

  close_stream(stream)

  Ok(exit_code)
}

/// Internal loop for processing stream lines
fn stream_loop(
  stream: StreamingPort,
  callback: fn(String) -> Nil,
  last_exit_code: Int,
) -> Int {
  case read_stream_line(stream) {
    OutputLine(line) -> {
      callback(line)
      stream_loop(stream, callback, last_exit_code)
    }
    ProcessExit(code) -> {
      // Continue reading to drain any remaining output
      stream_loop(stream, callback, code)
    }
    EndOfStream -> last_exit_code
    StreamError(_) -> last_exit_code
  }
}
