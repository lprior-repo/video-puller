/// Persistent download scope, concurrency and restricted-video handling.
import envoy
import gleam/dynamic/decode
import gleam/int
import gleam/json
import gleam/result
import gleam/string
import simplifile

pub type Settings {
  Settings(workers: Int, days: Int, skip_restricted: Bool)
}

pub fn load() -> Settings {
  let defaults =
    Settings(
      workers: bounded_env("SUBSCRIPTION_WORKERS", 1, 1, 16),
      days: bounded_env("SUBSCRIPTION_DAYS", 0, 0, 365),
      skip_restricted: bool_env("SUBSCRIPTION_SKIP_RESTRICTED", True),
    )
  case simplifile.read(path()) {
    Ok(content) ->
      json.parse(content, settings_decoder()) |> result.unwrap(defaults)
    Error(_) -> defaults
  }
}

pub fn save(
  workers: Int,
  days: Int,
  skip_restricted: Bool,
) -> Result(Nil, String) {
  case workers >= 1 && workers <= 16 && days >= 0 && days <= 365 {
    False -> Error("Use 1–16 parallel channels and a window of 0–365 days")
    True -> {
      let directory = data_dir() <> "/ytdl-sub"
      use _ <- result.try(
        simplifile.create_directory_all(directory)
        |> result.map_error(fn(_) {
          "Cannot prepare subscription settings directory"
        }),
      )
      let content =
        json.object([
          #("workers", json.int(workers)),
          #("days", json.int(days)),
          #("skip_restricted", json.bool(skip_restricted)),
        ])
        |> json.to_string
      use _ <- result.try(
        simplifile.write(path() <> ".tmp", content)
        |> result.map_error(fn(_) { "Cannot save subscription settings" }),
      )
      simplifile.rename(path() <> ".tmp", path())
      |> result.map_error(fn(_) { "Cannot replace subscription settings" })
    }
  }
}

fn settings_decoder() -> decode.Decoder(Settings) {
  use workers <- decode.field("workers", decode.int)
  use days <- decode.field("days", decode.int)
  use skip_restricted <- decode.optional_field(
    "skip_restricted",
    True,
    decode.bool,
  )
  case workers >= 1 && workers <= 16 && days >= 0 && days <= 365 {
    True -> decode.success(Settings(workers, days, skip_restricted))
    False -> decode.failure(Settings(1, 0, True), "valid subscription settings")
  }
}

fn data_dir() -> String {
  envoy.get("DATA_DIR") |> result.unwrap("./data")
}

fn path() -> String {
  data_dir() <> "/ytdl-sub/runtime.json"
}

fn bounded_env(name: String, default: Int, minimum: Int, maximum: Int) -> Int {
  let value =
    envoy.get(name)
    |> result.unwrap(int.to_string(default))
    |> int.parse
    |> result.unwrap(default)
  int.clamp(value, minimum, maximum)
}

fn bool_env(name: String, default: Bool) -> Bool {
  case envoy.get(name) {
    Ok(value) ->
      case string.lowercase(string.trim(value)) {
        "true" | "1" | "yes" | "on" -> True
        "false" | "0" | "no" | "off" -> False
        _ -> default
      }
    Error(_) -> default
  }
}
