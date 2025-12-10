/// FractalVideoEater - BEAM-Optimized Video Download System
///
/// Main entry point for the application. Implements a proper BEAM architecture:
/// - Hierarchical supervision tree for fault tolerance
/// - Worker pool for massive parallel downloads
/// - BEAM-native scheduling (no infinite recursion)
/// - Circuit breaker for cascade failure prevention
/// - Exponential backoff retries
import core/manager
import core/startup
import core/subscription_manager
import engine/ytdlp
import gleam/erlang/os
import gleam/erlang/process
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import infra/subscription_repo
import web/server

/// Main entry point for the application
pub fn main() -> Nil {
  io.println("🎬 FractalVideoEater - BEAM-Optimized Video Download System")
  io.println("============================================================")
  io.println("")

  // Display BEAM optimization features
  io.println("🔧 BEAM Features Enabled:")
  io.println("   ✓ Supervised worker pool for parallel downloads")
  io.println("   ✓ Native timer scheduling (no recursion)")
  io.println("   ✓ Circuit breaker protection")
  io.println("   ✓ Exponential backoff retries")
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
      io.println(
        "   Min workers: " <> int.to_string(int.max(5, max_concurrency / 2)),
      )
      io.println(
        "   Max workers: " <> int.to_string(int.max(50, max_concurrency * 5)),
      )
      io.println("")

      // Start the manager actor (now includes worker pool and scheduling)
      case manager.start(db, config, poll_interval, max_concurrency) {
        Ok(_manager_subject) -> {
          io.println("✅ Manager started with BEAM optimizations")

          // Note: Polling is now handled internally by the manager
          // using BEAM's native timer:send_after - NO MORE RECURSION!

          // Start subscription manager
          let sub_manager = case subscription_manager.start(db) {
            Ok(sub_subject) -> {
              io.println("📺 Subscription manager started")
              // Check if subscriptions are enabled and trigger initial poll
              case subscription_repo.get_config(db) {
                Ok(sub_config) if sub_config.enabled -> {
                  io.println("   Auto-download enabled, scheduling polls")
                  process.send(
                    sub_subject,
                    subscription_manager.ScheduleNextPoll,
                  )
                }
                _ -> Nil
              }
              Some(sub_subject)
            }
            Error(_) -> {
              io.println("⚠️  Subscription manager failed to start (non-fatal)")
              None
            }
          }

          // Start the web server with subscription manager
          case server.start_with_subscription(db, sub_manager) {
            Ok(_) -> {
              io.println("")
              io.println("✅ Application started successfully!")
              io.println("")
              io.println(
                "🚀 Ready to handle MASSIVE parallel downloads (500+ videos)",
              )
              io.println("")
              io.println("Press Ctrl+C to stop")

              // Keep the main process alive
              // The BEAM will handle all scheduling and supervision
              process.sleep_forever()
            }
            Error(err) -> {
              io.println("")
              io.println("❌ Failed to start web server")
              io.println("Error: " <> err)
            }
          }
        }
        Error(_) -> {
          io.println("")
          io.println("❌ Failed to start manager")
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

  ytdlp.DownloadConfig(
    output_directory: output_dir,
    format: format,
    max_filesize: max_filesize,
    audio_only: audio_only,
    audio_format: audio_format,
    allow_playlist: allow_playlist,
  )
}

/// Get boolean from environment with default fallback
fn get_env_bool(key: String, default: Bool) -> Bool {
  case os.get_env(key) {
    Ok("true") | Ok("1") | Ok("yes") -> True
    Ok(_) -> False
    Error(_) -> default
  }
}

/// Get string from environment with default fallback
fn get_env_string(key: String, default: String) -> String {
  case os.get_env(key) {
    Ok(value) -> value
    Error(_) -> default
  }
}

/// Get integer from environment with default fallback
fn get_env_int(key: String, default: Int) -> Int {
  case os.get_env(key) {
    Ok(value) ->
      case int.parse(value) {
        Ok(n) -> n
        Error(_) -> default
      }
    Error(_) -> default
  }
}

/// Validates a URL string
///
/// ## Examples
///
/// ```gleam
/// validate_url("https://example.com")
/// // -> Ok("https://example.com")
///
/// validate_url("")
/// // -> Error("URL cannot be empty")
/// ```
pub fn validate_url(url: String) -> Result(String, String) {
  case string.is_empty(url) {
    True -> Error("URL cannot be empty")
    False ->
      case
        string.starts_with(url, "http://")
        || string.starts_with(url, "https://")
      {
        True -> Ok(url)
        False -> Error("URL must start with http:// or https://")
      }
  }
}

/// Extracts the domain from a URL
///
/// ## Examples
///
/// ```gleam
/// extract_domain("https://example.com/path")
/// // -> Ok("example.com")
/// ```
pub fn extract_domain(url: String) -> Result(String, String) {
  use validated_url <- result.try(validate_url(url))

  let without_protocol = case string.starts_with(validated_url, "https://") {
    True -> string.drop_start(validated_url, 8)
    False -> string.drop_start(validated_url, 7)
  }

  case string.split(without_protocol, "/") {
    [domain, ..] -> Ok(domain)
    [] -> Error("Could not extract domain")
  }
}

/// Filters a list of URLs to only include valid ones
pub fn filter_valid_urls(urls: List(String)) -> List(String) {
  urls
  |> list.filter_map(validate_url)
}

/// Safely gets an element from a list at the given index
pub fn safe_get(items: List(a), index: Int) -> Result(a, String) {
  case index < 0 || index >= list.length(items) {
    True -> Error("Index out of bounds")
    False -> {
      let item =
        items
        |> list.drop(index)
        |> list.first()

      case item {
        Ok(value) -> Ok(value)
        Error(_) -> Error("Index out of bounds")
      }
    }
  }
}
