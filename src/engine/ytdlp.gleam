/// Yt-dlp command builder
///
/// Constructs safe yt-dlp commands with proper argument escaping.
/// CRITICAL: All URL inputs must be validated (INV-001).

import domain/types.{type JobId}
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import gleam/uri

/// Configuration for yt-dlp downloads
pub type DownloadConfig {
  DownloadConfig(
    output_directory: String,
    format: String,
    max_filesize: String,
  )
}

/// Build yt-dlp command arguments for downloading a video
pub fn build_download_args(
  url: String,
  job_id: JobId,
  config: DownloadConfig,
) -> Result(List(String), String) {
  // Validate URL to prevent injection
  use _ <- result.try(validate_url(url))

  let id_str = types.job_id_to_string(job_id)
  let output_template = config.output_directory <> "/" <> id_str <> ".%(ext)s"

  Ok([
    // Output template
    "--output",
    output_template,
    // Format selection (best quality)
    "--format",
    config.format,
    // Max filesize limit
    "--max-filesize",
    config.max_filesize,
    // Progress output
    "--newline",
    "--progress",
    // No playlist (single video only)
    "--no-playlist",
    // Retry and timeout settings
    "--retries",
    "3",
    "--socket-timeout",
    "30",
    // The URL (last argument)
    url,
  ])
}

/// Build yt-dlp command to get video info without downloading
pub fn build_info_args(url: String) -> Result(List(String), String) {
  use _ <- result.try(validate_url(url))

  Ok([
    "--dump-json",
    "--no-playlist",
    url,
  ])
}

/// Validate that a URL is safe and well-formed
fn validate_url(url: String) -> Result(Nil, String) {
  // Check for empty URL
  case string.is_empty(url) {
    True -> Error("URL cannot be empty")
    False -> {
      // Parse URL to validate structure
      case uri.parse(url) {
        Ok(parsed) -> {
          // Ensure it has a scheme (http/https)
          case parsed.scheme {
            Some(scheme) ->
              case scheme {
                "http" | "https" -> Ok(Nil)
                _ -> Error("URL must use http or https scheme")
              }
            None -> Error("URL must include a scheme (http:// or https://)")
          }
        }
        Error(_) -> Error("Invalid URL format")
      }
    }
  }
}

/// Default download configuration
pub fn default_config() -> DownloadConfig {
  DownloadConfig(
    output_directory: "./downloads",
    format: "bestvideo[ext=mp4]+bestaudio[ext=m4a]/best[ext=mp4]/best",
    max_filesize: "2G",
  )
}
