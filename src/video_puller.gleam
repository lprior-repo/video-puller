import core/supervisor
import gleam/erlang/process
import gleam/io
import gleam/list
import gleam/result
import gleam/string

/// Main entry point for the application
pub fn main() -> Nil {
  io.println("🎬 FractalVideoEater - Video Download System")
  io.println("============================================")
  io.println("")

  // Start the root supervisor tree
  case supervisor.start() {
    Ok(started) -> {
      io.println("")
      io.println("✅ Application started successfully!")
      io.println("PID: " <> string.inspect(started.pid))
      io.println("")
      io.println("Press Ctrl+C to stop")

      // Keep the main process alive
      process.sleep_forever()
    }
    Error(err) -> {
      io.println("")
      io.println("❌ Failed to start application")
      io.println("Error: " <> string.inspect(err))
    }
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
