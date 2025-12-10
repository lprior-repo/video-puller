/// Subscription Feed Fetcher
///
/// Fetches YouTube subscription feed using yt-dlp with browser cookies.
/// Parses the JSON output to extract video metadata.
import domain/subscription_types.{
  type DiscoveredVideo, type SubscriptionConfig, DiscoveredVideo,
}
import engine/shell
import gleam/dynamic/decode
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/string

/// YouTube subscription feed URL
const subscription_feed_url = "https://www.youtube.com/feed/subscriptions"

/// Fetch videos from subscription feed
pub fn fetch_feed(
  config: SubscriptionConfig,
) -> Result(List(DiscoveredVideo), String) {
  let args = build_feed_args(config)

  io.println("Fetching subscription feed...")

  case shell.run("yt-dlp", args) {
    Ok(result) -> {
      case result.exit_code {
        0 -> parse_feed_output(result.stdout)
        _ -> {
          // Check for common error messages
          case string.contains(result.stderr, "cookies") {
            True ->
              Error(
                "Cookie extraction failed. Make sure your browser is closed and try again.",
              )
            False ->
              Error("yt-dlp failed with exit code. stderr: " <> result.stderr)
          }
        }
      }
    }
    Error(shell.ExecutionError(msg)) -> Error("Execution error: " <> msg)
    Error(shell.InvalidCommand(msg)) -> Error("Invalid command: " <> msg)
  }
}

/// Build yt-dlp arguments for fetching subscription feed
pub fn build_feed_args(config: SubscriptionConfig) -> List(String) {
  let browser_arg = subscription_types.browser_to_string(config.browser)

  // Base args for fetching subscription feed metadata only
  let base_args = [
    "--cookies-from-browser",
    browser_arg,
    "--flat-playlist",
    "--dump-json",
    "--no-download",
    "--no-warnings",
    "--playlist-items",
    "1-50",
  ]

  // Add custom cookies path if specified
  let args = case config.cookies_path {
    Some(path) -> list.append(base_args, ["--cookies", path])
    None -> base_args
  }

  // Add the subscription feed URL last
  list.append(args, [subscription_feed_url])
}

/// Parse yt-dlp JSON output (one JSON object per line)
pub fn parse_feed_output(
  output: String,
) -> Result(List(DiscoveredVideo), String) {
  let lines =
    output
    |> string.split("\n")
    |> list.filter(fn(line) { !string.is_empty(string.trim(line)) })

  let results = list.map(lines, parse_video_json)

  // Collect successful parses, log errors
  let videos =
    list.fold(results, [], fn(acc, r) {
      case r {
        Ok(video) -> [video, ..acc]
        Error(err) -> {
          io.println("Failed to parse video JSON: " <> err)
          acc
        }
      }
    })
    |> list.reverse

  Ok(videos)
}

/// Parse a single JSON line into DiscoveredVideo
fn parse_video_json(json_str: String) -> Result(DiscoveredVideo, String) {
  case json.parse(json_str, video_decoder()) {
    Ok(video) -> Ok(video)
    Error(_) -> Error("Invalid JSON: " <> string.slice(json_str, 0, 100))
  }
}

/// JSON decoder for video data from yt-dlp
fn video_decoder() -> decode.Decoder(DiscoveredVideo) {
  use id <- decode.then(decode.at(["id"], decode.string))
  use title <- decode.then(decode.at(["title"], decode.string))
  use channel_id <- decode.then(
    decode.optional(decode.at(["channel_id"], decode.string)),
  )
  use channel <- decode.then(
    decode.optional(decode.at(["channel"], decode.string)),
  )
  use uploader <- decode.then(
    decode.optional(decode.at(["uploader"], decode.string)),
  )
  use duration <- decode.then(
    decode.optional(decode.at(["duration"], decode.int)),
  )
  use thumbnail <- decode.then(
    decode.optional(decode.at(["thumbnail"], decode.string)),
  )
  use timestamp <- decode.then(
    decode.optional(decode.at(["timestamp"], decode.int)),
  )

  // Use channel or uploader for channel_name
  let channel_name = case channel {
    Some(c) -> Some(c)
    None -> uploader
  }

  // Construct video URL
  let url = "https://www.youtube.com/watch?v=" <> id

  decode.success(DiscoveredVideo(
    video_id: id,
    channel_id: channel_id,
    channel_name: channel_name,
    title: title,
    url: url,
    published_at: timestamp,
    duration_seconds: duration,
    thumbnail_url: thumbnail,
  ))
}

/// Test if yt-dlp can access subscription feed with given config
pub fn test_feed_access(config: SubscriptionConfig) -> Result(String, String) {
  let browser_arg = subscription_types.browser_to_string(config.browser)

  let args = case config.cookies_path {
    Some(path) -> [
      "--cookies-from-browser",
      browser_arg,
      "--cookies",
      path,
      "--flat-playlist",
      "--dump-json",
      "--playlist-items",
      "1",
      "--no-download",
      subscription_feed_url,
    ]
    None -> [
      "--cookies-from-browser",
      browser_arg,
      "--flat-playlist",
      "--dump-json",
      "--playlist-items",
      "1",
      "--no-download",
      subscription_feed_url,
    ]
  }

  case shell.run("yt-dlp", args) {
    Ok(result) -> {
      case result.exit_code {
        0 -> Ok("Feed access successful")
        _ -> Error("Feed access failed: " <> result.stderr)
      }
    }
    Error(shell.ExecutionError(msg)) -> Error("Execution error: " <> msg)
    Error(shell.InvalidCommand(msg)) -> Error("Invalid command: " <> msg)
  }
}

/// Get single video info by URL
pub fn get_video_info(
  url: String,
  config: SubscriptionConfig,
) -> Result(DiscoveredVideo, String) {
  let browser_arg = subscription_types.browser_to_string(config.browser)

  let args = [
    "--cookies-from-browser",
    browser_arg,
    "--dump-json",
    "--no-download",
    "--no-playlist",
    url,
  ]

  case shell.run("yt-dlp", args) {
    Ok(result) -> {
      case result.exit_code {
        0 -> parse_video_json(string.trim(result.stdout))
        _ -> Error("Failed to get video info: " <> result.stderr)
      }
    }
    Error(shell.ExecutionError(msg)) -> Error("Execution error: " <> msg)
    Error(shell.InvalidCommand(msg)) -> Error("Invalid command: " <> msg)
  }
}
