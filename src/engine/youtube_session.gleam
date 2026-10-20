/// Refresh an explicitly enabled local browser session before polling.
/// The existing private export survives browser locks and refresh failures.
import engine/shell
import envoy
import gleam/dynamic/decode
import gleam/io
import gleam/json
import gleam/result
import gleam/string
import simplifile

pub fn refresh() -> Result(Nil, String) {
  let browser = setting("YOUTUBE_SESSION_BROWSER")
  case browser {
    "" -> Ok(Nil)
    _ -> {
      let file = setting("YOUTUBE_COOKIE_FILE")
      let python = setting("YOUTUBE_SESSION_PYTHON")
      let helper = setting("YOUTUBE_SESSION_HELPER")
      let exists = simplifile.is_file(file) |> result.unwrap(False)
      let execute = fn() {
        case file == "" || python == "" || helper == "" {
          True -> Error("automatic session refresh configuration is incomplete")
          False ->
            case
              shell.run_with_timeout(
                python,
                [helper, "--browser", browser, "--output", file],
                30_000,
              )
            {
              Ok(output) if output.exit_code == 0 -> Ok(Nil)
              _ -> Error("local browser session refresh failed")
            }
        }
      }
      case refresh_with(execute, exists) {
        Ok(state) -> {
          save_status(state)
          case state {
            "using_saved_session" ->
              io.println(
                "Browser session refresh failed; retaining the saved session and retrying next poll",
              )
            _ -> io.println("Local YouTube session refreshed")
          }
          Ok(Nil)
        }
        Error(message) -> {
          save_status("needs_browser_access")
          Error(message)
        }
      }
    }
  }
}

/// Injectable boundary for recovery tests, without reading any browser data.
pub fn refresh_with(
  execute: fn() -> Result(Nil, String),
  saved_session: Bool,
) -> Result(String, String) {
  case execute(), saved_session {
    Ok(_), _ -> Ok("refreshed")
    Error(_), True -> Ok("using_saved_session")
    Error(_), False ->
      Error("YouTube session refresh failed; local browser access is required")
  }
}

/// Renew just before each serial channel. Concurrent jobs share the poll's
/// export so one running downloader cannot overwrite another job's refresh.
pub fn before_channel(
  workers: Int,
  refresh: fn() -> Result(Nil, String),
) -> Result(Nil, String) {
  case workers {
    1 -> refresh()
    _ -> Ok(Nil)
  }
}

pub fn status() -> String {
  case simplifile.read(status_path()) {
    Ok(contents) ->
      json.parse(contents, status_decoder())
      |> result.unwrap("unknown")
    Error(_) ->
      case setting("YOUTUBE_SESSION_BROWSER") {
        "" -> "automatic_refresh_disabled"
        _ -> "not_refreshed_yet"
      }
  }
}

fn status_decoder() -> decode.Decoder(String) {
  use state <- decode.field("state", decode.string)
  decode.success(state)
}

fn save_status(state: String) -> Nil {
  let root = setting("DATA_DIR")
  case root {
    "" -> Nil
    _ -> {
      let _ = simplifile.create_directory_all(root <> "/cache")
      let path = status_path()
      let contents =
        json.object([
          #("state", json.string(state)),
          #("checked_at", json.int(now())),
        ])
        |> json.to_string
      case simplifile.write(path <> ".tmp", contents) {
        Ok(_) -> {
          let _ = simplifile.rename(path <> ".tmp", path)
          Nil
        }
        Error(_) -> io.println("Cannot persist YouTube session refresh status")
      }
    }
  }
}

fn status_path() -> String {
  setting("DATA_DIR") <> "/cache/youtube-session-status.json"
}

fn setting(name: String) -> String {
  envoy.get(name) |> result.unwrap("") |> string.trim
}

type TimeUnit {
  Second
}

@external(erlang, "erlang", "system_time")
fn now_ffi(unit: TimeUnit) -> Int

fn now() -> Int {
  now_ffi(Second)
}
