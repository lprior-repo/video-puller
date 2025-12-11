/// Shell command execution primitive
///
/// Provides safe shell command execution using shellout and streaming via Erlang ports.
/// CRITICAL: All inputs must be sanitized to prevent shell injection (INV-001).
import gleam/erlang/port.{type Port}
import gleam/list
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
      // Use streaming execution with timeout
      use stream <- result.try(open_stream(command, args))

      case read_with_timeout(stream, timeout_ms, [], []) {
        Ok(#(exit_code, stdout_lines, stderr_lines)) -> {
          close_stream(stream)
          Ok(ShellResult(
            exit_code: exit_code,
            stdout: string.join(stdout_lines, "\n"),
            stderr: string.join(stderr_lines, "\n"),
          ))
        }
        Error(e) -> {
          close_stream(stream)
          Error(e)
        }
      }
    }
  }
}

/// Read from stream with timeout
fn read_with_timeout(
  stream: StreamingPort,
  timeout_ms: Int,
  stdout_acc: List(String),
  stderr_acc: List(String),
) -> Result(#(Int, List(String), List(String)), ShellError) {
  case read_stream_line_with_timeout(stream, timeout_ms) {
    Ok(OutputLine(line)) -> {
      // All output goes to stdout (stderr is harder to separate in ports)
      read_with_timeout(stream, timeout_ms, [line, ..stdout_acc], stderr_acc)
    }
    Ok(ProcessExit(code)) -> {
      // Continue reading to drain remaining output
      read_with_timeout(stream, timeout_ms, stdout_acc, stderr_acc)
      |> result.map(fn(res) {
        let #(_, out, err) = res
        #(code, out, err)
      })
      |> result.or(
        Ok(#(code, list.reverse(stdout_acc), list.reverse(stderr_acc))),
      )
    }
    Ok(EndOfStream) -> {
      Ok(#(0, list.reverse(stdout_acc), list.reverse(stderr_acc)))
    }
    Ok(StreamError(_)) -> {
      Ok(#(1, list.reverse(stdout_acc), list.reverse(stderr_acc)))
    }
    Error(Timeout) -> {
      Error(ExecutionError("Command timeout exceeded"))
    }
  }
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

/// Check if yt-dlp is available
pub fn check_yt_dlp() -> Result(String, ShellError) {
  run_simple("yt-dlp", ["--version"])
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

/// Check if a port is still alive
@external(erlang, "shell_ffi", "is_port_alive")
fn do_is_port_alive(port: Port) -> Bool

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

/// Check if a streaming port is still alive
pub fn is_stream_alive(stream: StreamingPort) -> Bool {
  do_is_port_alive(stream.port)
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
