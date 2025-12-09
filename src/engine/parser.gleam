/// Yt-dlp output parser
///
/// Parses yt-dlp progress output to extract download percentage and status.
/// Uses regex to match progress patterns like "[download] 45.3% of 100.00MiB"

import gleam/option.{Some}
import gleam/regexp
import gleam/string

/// Parsed progress information from yt-dlp output
pub type ProgressInfo {
  ProgressInfo(percentage: Int, message: String)
}

/// Parse download progress from yt-dlp output line
///
/// Example input: "[download]  45.3% of  100.00MiB at  1.23MiB/s ETA 00:42"
/// Returns: Ok(ProgressInfo(45, "..."))
pub fn parse_progress(line: String) -> Result(ProgressInfo, String) {
  // Create regex pattern for matching progress percentage
  // Pattern: [download] followed by percentage
  let pattern = "\\[download\\]\\s+(\\d+(?:\\.\\d+)?)%"

  case regexp.from_string(pattern) {
    Ok(re) -> {
      case regexp.scan(re, line) {
        [match, ..] -> {
          // Extract the percentage from the first capture group
          case match.submatches {
            [Some(pct_str), ..] -> {
              // Parse the percentage string to float, then round to int
              case parse_float_string(pct_str) {
                Ok(pct) -> {
                  let pct_int = round_float(pct)
                  Ok(ProgressInfo(percentage: pct_int, message: line))
                }
                Error(_) -> Error("Failed to parse percentage: " <> pct_str)
              }
            }
            _ -> Error("No percentage captured from: " <> line)
          }
        }
        [] -> Error("No progress pattern found in: " <> line)
      }
    }
    Error(_) -> Error("Invalid regex pattern")
  }
}

/// Check if a line indicates download completion
pub fn is_complete(line: String) -> Bool {
  string.contains(line, "[download] 100%")
    || string.contains(line, "has already been downloaded")
}

/// Check if a line indicates an error
pub fn is_error(line: String) -> Bool {
  string.contains(string.lowercase(line), "error:")
    || string.contains(line, "ERROR:")
    || string.contains(line, "Failed to")
}

/// Extract error message from yt-dlp error output
pub fn extract_error(line: String) -> String {
  case string.split(line, "ERROR:") {
    [_, error] -> string.trim(error)
    _ ->
      case string.split(line, "error:") {
        [_, error] -> string.trim(error)
        _ -> line
      }
  }
}

/// Simple float parsing using Erlang FFI
@external(erlang, "parser_ffi", "parse_float")
fn parse_float_string(str: String) -> Result(Float, Nil)

/// Round a float to nearest integer
@external(erlang, "erlang", "round")
fn round_float(f: Float) -> Int
