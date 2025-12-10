import core/manager
import core/startup
import domain/types
import engine/ytdlp
import envoy
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import web/server

/// Main entry point for the application
pub fn main() -> Nil {
  io.println("🎬 FractalVideoEater - Video Download System")
  io.println("============================================")
  io.println("")

  // Initialize database and run startup sequence
  case startup.initialize() {
    Ok(db) -> {
      // Start the manager actor for processing downloads
      let config = default_download_config()
      let poll_interval = get_env_int("POLL_INTERVAL_MS", 5000)
      let max_concurrency = get_env_int("MAX_CONCURRENCY", 3)

      case manager.start(db, config, poll_interval, max_concurrency) {
        Ok(manager_subject) -> {
          io.println(
            "🎯 Manager started (poll="
            <> int.to_string(poll_interval)
            <> "ms, max_concurrent="
            <> int.to_string(max_concurrency)
            <> ")",
          )

          // Start polling loop in a separate process
          start_polling_loop(manager_subject, poll_interval)

          // Start the web server
          case server.start(db) {
            Ok(_) -> {
              io.println("")
              io.println("✅ Application started successfully!")
              io.println("")
              io.println("Press Ctrl+C to stop")

              // Keep the main process alive
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

/// Start a background process that periodically polls for jobs
fn start_polling_loop(
  manager_subject: Subject(types.ManagerMessage),
  interval_ms: Int,
) -> Nil {
  // Spawn a process that sends PollJobs messages periodically
  let _ = process.spawn(fn() { polling_loop(manager_subject, interval_ms) })
  Nil
}

/// Polling loop - sends PollJobs message and sleeps
fn polling_loop(
  manager_subject: Subject(types.ManagerMessage),
  interval_ms: Int,
) -> Nil {
  // Send poll message
  process.send(manager_subject, types.PollJobs)

  // Sleep for interval
  process.sleep(interval_ms)

  // Recurse
  polling_loop(manager_subject, interval_ms)
}

/// Default download configuration
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
