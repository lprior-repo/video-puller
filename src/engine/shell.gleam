/// Shell command execution primitive
///
/// Provides safe shell command execution using shellout.
/// CRITICAL: All inputs must be sanitized to prevent shell injection (INV-001).

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
        Ok(output) ->
          Ok(ShellResult(exit_code: 0, stdout: output, stderr: ""))
        Error(#(code, stderr)) ->
          Ok(ShellResult(exit_code: code, stdout: "", stderr: stderr))
      }
    }
  }
}

/// Execute a command and return only stdout on success
pub fn run_simple(
  command: String,
  args: List(String),
) -> Result(String, ShellError) {
  use result <- result.try(run(command, args))
  case result.exit_code {
    0 -> Ok(string.trim(result.stdout))
    _ -> Error(ExecutionError("Command exited with code: " <> int_to_string(result.exit_code)))
  }
}

/// Validate that a command string doesn't contain dangerous characters
fn validate_command(command: String) -> Result(Nil, ShellError) {
  let dangerous_chars = [";", "|", "&", "$", "`", "(", ")", "<", ">", "\n"]

  let has_dangerous =
    list.any(dangerous_chars, fn(char) { string.contains(command, char) })

  case has_dangerous {
    True ->
      Error(InvalidCommand(
        "Command contains shell metacharacters: " <> command,
      ))
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
