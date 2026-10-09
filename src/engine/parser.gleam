/// Yt-dlp output parser
///
/// Parses yt-dlp progress output to extract download percentage and status.
/// Uses regex to match progress patterns like "[download] 45.3% of 100.00MiB"
import gleam/float
import gleam/int
import gleam/option.{Some}
import gleam/regexp

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

/// Simple float parsing - handles both float strings ("45.3") and integer strings ("100")
fn parse_float_string(str: String) -> Result(Float, Nil) {
  case float.parse(str) {
    Ok(f) -> Ok(f)
    Error(_) -> {
      // If not a float, try parsing as integer and convert
      case int.parse(str) {
        Ok(i) -> Ok(int.to_float(i))
        Error(_) -> Error(Nil)
      }
    }
  }
}

/// Round a float to nearest integer
@external(erlang, "erlang", "round")
fn round_float(f: Float) -> Int
