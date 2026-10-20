/// Conservative request pacing with an optional, local YouTube cookie file.
import envoy
import gleam/json
import gleam/option.{type Option, None, Some}
import gleam/string

pub fn from_env() -> List(#(String, String)) {
  options(case envoy.get("YOUTUBE_COOKIE_FILE") {
    Ok(path) -> Some(path)
    Error(_) -> None
  })
}

/// JSON string quoting also produces a safe YAML scalar for local file paths.
pub fn options(cookie_file: Option(String)) -> List(#(String, String)) {
  let pacing = [
    #("sleep_interval_requests", "1.0"),
    #("sleep_interval", "5.0"),
    #("max_sleep_interval", "10.0"),
  ]
  case cookie_file {
    None -> pacing
    Some(path) ->
      case string.trim(path) {
        "" -> pacing
        path -> [#("cookiefile", json.string(path) |> json.to_string), ..pacing]
      }
  }
}
