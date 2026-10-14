/// FractalVideoEater - BEAM-Optimized Video Download System
///
/// Main entry point for the application. Implements a proper BEAM architecture:
/// - Worker pool for massive parallel downloads
/// - BEAM-native timer scheduling
import core/service_supervisor
import core/startup
import engine/ytdlp
import envoy
import gleam/erlang/process
import gleam/int
import gleam/io
import gleam/string

/// Main entry point for the application
pub fn main() -> Nil {
  io.println("🎬 FractalVideoEater - BEAM-Optimized Video Download System")
  io.println("============================================================")
  io.println("")

  // Display BEAM optimization features
  io.println("🔧 BEAM Features Enabled:")
  io.println("   ✓ Supervised worker pool for parallel downloads")
  io.println("   ✓ Native timer scheduling")
  io.println("   ✓ Dynamic worker scaling")
  io.println("")

  // Initialize database and run startup sequence
  case startup.initialize() {
    Ok(db) -> {
      // Load configuration from environment
      let config = default_download_config()
      let poll_interval = get_env_int("POLL_INTERVAL_MS", 5000)
      let max_concurrency = get_env_int("MAX_CONCURRENCY", 10)

      io.println("📊 Configuration:")
      io.println("   Poll interval: " <> int.to_string(poll_interval) <> "ms")
      io.println("   Max concurrency: " <> int.to_string(max_concurrency))
      io.println("")

      case
        service_supervisor.start(db, config, poll_interval, max_concurrency)
      {
        Ok(_services) -> {
          io.println("")
          io.println("✅ Application started under OTP supervision!")
          io.println("")
          io.println("🚀 Ready for bounded parallel downloads")
          io.println("")
          io.println("Press Ctrl+C to stop")
          process.sleep_forever()
        }
        Error(err) -> {
          io.println("")
          io.println("❌ Failed to start supervised application services")
          io.println("Error: " <> string.inspect(err))
        }
      }
    }
    Error(err) -> {
      io.println("")
      io.println("❌ Failed to initialize application")
      io.println("Error: " <> string.inspect(err))
    }
  }
}

/// Default download configuration with environment overrides
fn default_download_config() -> ytdlp.DownloadConfig {
  let output_dir = get_env_string("OUTPUT_DIR", "./downloads")
  let format =
    get_env_string(
      "YTDLP_FORMAT",
      "bestvideo[ext=mp4]+bestaudio[ext=m4a]/best[ext=mp4]/best",
    )
  let max_filesize = get_env_string("MAX_FILESIZE", "2G")
  let audio_only = get_env_bool("AUDIO_ONLY", False)
  let audio_format =
    ytdlp.string_to_audio_format(get_env_string("AUDIO_FORMAT", "best"))
  let allow_playlist = get_env_bool("ALLOW_PLAYLIST", False)
  // Download timeout in minutes, default to 30 minutes
  let download_timeout_minutes = get_env_int("DOWNLOAD_TIMEOUT_MINUTES", 30)
  // Rate limiting for massive downloads
  let rate_limit_delay_ms = get_env_int("RATE_LIMIT_DELAY_MS", 500)
  let bandwidth_limit = get_env_string("BANDWIDTH_LIMIT", "")
  // Channel-based folder organization for Plex TV show style libraries
  let use_channel_folders = get_env_bool("USE_CHANNEL_FOLDERS", False)

  ytdlp.DownloadConfig(
    output_directory: output_dir,
    format: format,
    max_filesize: max_filesize,
    audio_only: audio_only,
    audio_format: audio_format,
    allow_playlist: allow_playlist,
    download_timeout_ms: download_timeout_minutes * 60_000,
    rate_limit_delay_ms: rate_limit_delay_ms,
    bandwidth_limit: bandwidth_limit,
    use_channel_folders: use_channel_folders,
  )
}

/// Get boolean from environment with default fallback
fn get_env_bool(key: String, default: Bool) -> Bool {
  case envoy.get(key) {
    Ok("true") | Ok("1") | Ok("yes") -> True
    Ok(_) -> False
    Error(_) -> default
  }
}

/// Get string from environment with default fallback
fn get_env_string(key: String, default: String) -> String {
  case envoy.get(key) {
    Ok(value) -> value
    Error(_) -> default
  }
}

/// Get integer from environment with default fallback
fn get_env_int(key: String, default: Int) -> Int {
  case envoy.get(key) {
    Ok(value) ->
      case int.parse(value) {
        Ok(n) -> n
        Error(_) -> default
      }
    Error(_) -> default
  }
}
