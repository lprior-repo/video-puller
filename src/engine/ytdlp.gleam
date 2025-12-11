/// Yt-dlp command builder
///
/// Constructs safe yt-dlp commands with proper argument escaping.
/// CRITICAL: All URL inputs must be validated (INV-001).
import domain/types.{type JobId}
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import gleam/uri

/// Common arguments needed for YouTube JS challenge solving
/// These are required for yt-dlp to work reliably with YouTube as of late 2025
fn youtube_js_args() -> List(String) {
  [
    // Enable remote EJS challenge solver from GitHub
    "--remote-components",
    "ejs:github",
  ]
}

/// Audio format options for audio-only downloads
pub type AudioFormat {
  MP3
  AAC
  OPUS
  BestAudio
}

/// Configuration for yt-dlp downloads
pub type DownloadConfig {
  DownloadConfig(
    output_directory: String,
    format: String,
    max_filesize: String,
    audio_only: Bool,
    audio_format: AudioFormat,
    allow_playlist: Bool,
    // Download timeout in milliseconds (default: 30 minutes)
    download_timeout_ms: Int,
    // Rate limiting: minimum delay between starting downloads (ms)
    // Helps avoid YouTube rate limits when downloading many videos
    rate_limit_delay_ms: Int,
    // Bandwidth limit per download (e.g., "5M" for 5MB/s, "" for unlimited)
    bandwidth_limit: String,
    // Organize downloads into channel subfolders: downloads/{channel}/{video}.mp4
    // For Plex TV show-style library organization
    use_channel_folders: Bool,
  )
}

/// Build yt-dlp command arguments for downloading a video
pub fn build_download_args(
  url: String,
  _job_id: JobId,
  config: DownloadConfig,
  format_code: option.Option(String),
) -> Result(List(String), String) {
  // Validate URL to prevent injection
  use _ <- result.try(validate_url(url))

  // Plex-friendly output template varies based on channel folder setting:
  // - With folders: {dir}/{channel}/{title} ({date}).{ext} - for TV show style libraries
  // - Without folders: {dir}/{title} - {channel} ({date}).{ext} - flat structure
  let output_template = case config.use_channel_folders {
    True ->
      config.output_directory
      <> "/%(channel)s/%(title)s (%(upload_date>%Y-%m-%d)s).%(ext)s"
    False ->
      config.output_directory
      <> "/%(title)s - %(channel)s (%(upload_date>%Y-%m-%d)s).%(ext)s"
  }

  // Build base args
  let base_args = [
    // Output template - Plex-friendly format (P-002)
    "--output",
    output_template,
    // Sanitize filenames for filesystem safety (P-002)
    "--restrict-filenames",
    // Embed metadata into video file (P-003)
    "--embed-metadata",
    "--add-metadata",
    // Embed thumbnail as cover art/poster (P-004)
    "--embed-thumbnail",
    // Save metadata sidecar file for Plex agents
    "--write-info-json",
    // Max filesize limit
    "--max-filesize",
    config.max_filesize,
    // Progress output
    "--newline",
    "--progress",
    // Retry and timeout settings
    "--retries",
    "3",
    "--socket-timeout",
    "30",
  ]

  // Add format args based on audio_only mode
  let format_args = case config.audio_only {
    True -> build_audio_args(config.audio_format)
    False -> {
      // Use provided format_code or fall back to config.format
      let format = case format_code {
        Some(code) -> code
        None -> config.format
      }
      ["--format", format]
    }
  }

  // Add playlist handling
  let playlist_args = case config.allow_playlist {
    True -> ["--yes-playlist"]
    False -> ["--no-playlist"]
  }

  // Add bandwidth limiting if configured (helps prevent rate limiting)
  let bandwidth_args = case config.bandwidth_limit {
    "" -> []
    limit -> ["--limit-rate", limit]
  }

  // Combine all args: JS runtime args first, then base args, format, playlist, bandwidth, and URL last
  Ok(
    list.flatten([
      youtube_js_args(),
      base_args,
      format_args,
      playlist_args,
      bandwidth_args,
      [url],
    ]),
  )
}

/// Build audio extraction arguments
fn build_audio_args(audio_format: AudioFormat) -> List(String) {
  let format_str = case audio_format {
    MP3 -> "mp3"
    AAC -> "aac"
    OPUS -> "opus"
    BestAudio -> "best"
  }

  case audio_format {
    BestAudio -> ["--format", "bestaudio", "--extract-audio"]
    _ -> [
      "--format",
      "bestaudio",
      "--extract-audio",
      "--audio-format",
      format_str,
    ]
  }
}

/// Build yt-dlp command to get video info without downloading
pub fn build_info_args(url: String) -> Result(List(String), String) {
  use _ <- result.try(validate_url(url))

  Ok(
    list.flatten([
      youtube_js_args(),
      ["--dump-json", "--no-playlist", url],
    ]),
  )
}

/// Build yt-dlp command to list available formats for a video
pub fn build_list_formats_args(url: String) -> Result(List(String), String) {
  use _ <- result.try(validate_url(url))

  Ok(
    list.flatten([
      youtube_js_args(),
      ["-F", url],
    ]),
  )
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
    audio_only: False,
    audio_format: BestAudio,
    allow_playlist: False,
    // Default to 30 minute timeout
    download_timeout_ms: 1_800_000,
    // Rate limiting defaults for massive downloads
    rate_limit_delay_ms: 500,
    bandwidth_limit: "",
    // Channel folders disabled by default for backwards compatibility
    use_channel_folders: False,
  )
}

/// Create audio-only download configuration
pub fn audio_config(_format: AudioFormat) -> DownloadConfig {
  DownloadConfig(
    output_directory: "./downloads",
    format: "bestaudio",
    max_filesize: "500M",
    audio_only: True,
    audio_format: BestAudio,
    allow_playlist: False,
    // Default to 30 minute timeout
    download_timeout_ms: 1_800_000,
    // Rate limiting defaults for massive downloads
    rate_limit_delay_ms: 500,
    bandwidth_limit: "",
    // Channel folders disabled by default
    use_channel_folders: False,
  )
}

/// Build yt-dlp command to get playlist info without downloading
pub fn build_playlist_info_args(url: String) -> Result(List(String), String) {
  use _ <- result.try(validate_url(url))

  Ok(
    list.flatten([
      youtube_js_args(),
      ["--dump-json", "--flat-playlist", "--yes-playlist", url],
    ]),
  )
}

/// Convert AudioFormat to string
pub fn audio_format_to_string(format: AudioFormat) -> String {
  case format {
    MP3 -> "mp3"
    AAC -> "aac"
    OPUS -> "opus"
    BestAudio -> "best"
  }
}

/// Parse string to AudioFormat
pub fn string_to_audio_format(s: String) -> AudioFormat {
  case string.lowercase(s) {
    "mp3" -> MP3
    "aac" -> AAC
    "opus" -> OPUS
    _ -> BestAudio
  }
}
