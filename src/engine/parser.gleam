/// Yt-dlp output parser
///
/// Parses yt-dlp progress output to extract download percentage and status.
/// Uses regex to match progress patterns like "[download] 45.3% of 100.00MiB"
import domain/core_types.{
  type FormatOption, type VideoMetadata, FormatOption, VideoMetadata,
}
import gleam/list
import gleam/option.{type Option, Some}
import gleam/regexp
import gleam/string

/// Parsed progress information from yt-dlp output
pub type ProgressInfo {
  ProgressInfo(percentage: Int, message: String)
}

/// Enhanced progress information with all available fields
pub type EnhancedProgressInfo {
  EnhancedProgressInfo(
    percentage: Int,
    file_size: Option(String),
    speed: Option(String),
    eta: Option(String),
    message: String,
  )
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

/// Parse enhanced download progress from yt-dlp output line
///
/// Example input: "[download]  45.3% of  100.00MiB at  1.23MiB/s ETA 00:42"
/// Returns: Ok(EnhancedProgressInfo(45, Some("100.00MiB"), Some("1.23MiB/s"), Some("00:42"), "..."))
pub fn parse_enhanced_progress(
  line: String,
) -> Result(EnhancedProgressInfo, String) {
  // Full pattern for yt-dlp progress line:
  // [download]  45.3% of  100.00MiB at  1.23MiB/s ETA 00:42
  let full_pattern =
    "\\[download\\]\\s+(\\d+(?:\\.\\d+)?)%\\s+of\\s+(\\S+)\\s+at\\s+(\\S+)\\s+ETA\\s+(\\S+)"

  case regexp.from_string(full_pattern) {
    Ok(re) -> {
      case regexp.scan(re, line) {
        [match, ..] -> {
          case match.submatches {
            [Some(pct_str), Some(size), Some(speed), Some(eta)] -> {
              case parse_float_string(pct_str) {
                Ok(pct) -> {
                  let pct_int = round_float(pct)
                  Ok(EnhancedProgressInfo(
                    percentage: pct_int,
                    file_size: Some(size),
                    speed: Some(speed),
                    eta: Some(eta),
                    message: line,
                  ))
                }
                Error(_) -> Error("Failed to parse percentage: " <> pct_str)
              }
            }
            _ -> parse_partial_progress(line)
          }
        }
        [] -> parse_partial_progress(line)
      }
    }
    Error(_) -> Error("Invalid regex pattern")
  }
}

/// Parse partial progress (when not all fields are available)
fn parse_partial_progress(line: String) -> Result(EnhancedProgressInfo, String) {
  // Try simpler pattern: [download]  45.3% of  100.00MiB
  let simple_pattern = "\\[download\\]\\s+(\\d+(?:\\.\\d+)?)%\\s+of\\s+(\\S+)"

  case regexp.from_string(simple_pattern) {
    Ok(re) -> {
      case regexp.scan(re, line) {
        [match, ..] -> {
          case match.submatches {
            [Some(pct_str), Some(size), ..] -> {
              case parse_float_string(pct_str) {
                Ok(pct) -> {
                  let pct_int = round_float(pct)
                  // Try to extract speed and ETA separately
                  let speed = extract_speed(line)
                  let eta = extract_eta(line)
                  Ok(EnhancedProgressInfo(
                    percentage: pct_int,
                    file_size: Some(size),
                    speed: speed,
                    eta: eta,
                    message: line,
                  ))
                }
                Error(_) -> Error("Failed to parse percentage: " <> pct_str)
              }
            }
            _ -> parse_percentage_only(line)
          }
        }
        [] -> parse_percentage_only(line)
      }
    }
    Error(_) -> Error("Invalid regex pattern")
  }
}

/// Parse just the percentage when other fields aren't present
fn parse_percentage_only(line: String) -> Result(EnhancedProgressInfo, String) {
  let pattern = "\\[download\\]\\s+(\\d+(?:\\.\\d+)?)%"

  case regexp.from_string(pattern) {
    Ok(re) -> {
      case regexp.scan(re, line) {
        [match, ..] -> {
          case match.submatches {
            [Some(pct_str), ..] -> {
              case parse_float_string(pct_str) {
                Ok(pct) -> {
                  let pct_int = round_float(pct)
                  Ok(EnhancedProgressInfo(
                    percentage: pct_int,
                    file_size: option.None,
                    speed: option.None,
                    eta: option.None,
                    message: line,
                  ))
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

/// Extract download speed from a progress line
fn extract_speed(line: String) -> Option(String) {
  // Pattern: "at  1.23MiB/s" or similar
  let pattern = "at\\s+(\\S+/s)"
  case regexp.from_string(pattern) {
    Ok(re) -> {
      case regexp.scan(re, line) {
        [match, ..] -> {
          case match.submatches {
            [Some(speed), ..] -> Some(speed)
            _ -> option.None
          }
        }
        [] -> option.None
      }
    }
    Error(_) -> option.None
  }
}

/// Extract ETA from a progress line
fn extract_eta(line: String) -> Option(String) {
  // Pattern: "ETA 00:42" or "ETA Unknown"
  let pattern = "ETA\\s+(\\S+)"
  case regexp.from_string(pattern) {
    Ok(re) -> {
      case regexp.scan(re, line) {
        [match, ..] -> {
          case match.submatches {
            [Some(eta), ..] -> Some(eta)
            _ -> option.None
          }
        }
        [] -> option.None
      }
    }
    Error(_) -> option.None
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

/// Parse format list output from yt-dlp -F command
///
/// Example format line:
/// "247          webm       1280x720    720p  679k , vp9, 30fps, video only"
/// Returns: FormatOption(code: "247", ext: "webm", resolution: "1280x720", note: "720p  679k , vp9, 30fps, video only")
pub fn parse_format_list(output: String) -> List(FormatOption) {
  output
  |> string.split("\n")
  |> list.filter_map(parse_format_line)
}

/// Parse a single format line from yt-dlp -F output
fn parse_format_line(line: String) -> Result(FormatOption, Nil) {
  // Skip header lines and separator lines
  case
    string.contains(line, "ID")
    || string.contains(line, "---")
    || string.is_empty(string.trim(line))
  {
    True -> Error(Nil)
    False -> {
      // Try to parse the format line using regex
      // Pattern: format_code (whitespace) extension (whitespace) resolution (whitespace) rest
      let pattern = "^(\\S+)\\s+(\\S+)\\s+(\\S+)\\s+(.*)$"
      case regexp.from_string(pattern) {
        Ok(re) -> {
          case regexp.scan(re, string.trim(line)) {
            [match, ..] -> {
              case match.submatches {
                [Some(code), Some(ext), Some(res), Some(note)] ->
                  Ok(FormatOption(
                    code: code,
                    ext: ext,
                    resolution: res,
                    note: string.trim(note),
                  ))
                _ -> Error(Nil)
              }
            }
            [] -> Error(Nil)
          }
        }
        Error(_) -> Error(Nil)
      }
    }
  }
}

/// Parse video metadata from yt-dlp --dump-json output
///
/// Example JSON fields:
/// - "title": "Video Title"
/// - "thumbnail": "https://i.ytimg.com/vi/..."
/// - "duration": 123 (seconds as integer)
/// - "uploader": "Channel Name"
/// - "view_count": 12345 (optional, may be null)
///
/// Returns: Result(VideoMetadata, String) with error message on failure
pub fn parse_video_info(json_string: String) -> Result(VideoMetadata, String) {
  // Use Erlang FFI which handles JSON parsing and field extraction
  case parse_video_metadata_ffi(json_string) {
    Ok(#(title, thumbnail, duration, uploader, view_count)) ->
      Ok(VideoMetadata(
        title: title,
        thumbnail: thumbnail,
        duration: duration,
        uploader: uploader,
        view_count: view_count,
      ))
    Error(err_msg) -> Error(err_msg)
  }
}

/// Parse video metadata using Erlang FFI
/// Returns tuple of (title, thumbnail, duration, uploader, view_count)
@external(erlang, "parser_ffi", "parse_video_metadata")
fn parse_video_metadata_ffi(
  json_string: String,
) -> Result(#(String, String, Int, String, Option(Int)), String)
