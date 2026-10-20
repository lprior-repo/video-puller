/// Atomic, non-secret workflow status that survives service restarts.
import engine/subscription_recovery.{type State, State}
import envoy
import gleam/dynamic/decode
import gleam/int
import gleam/io
import gleam/json
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import simplifile

pub fn load() -> State {
  case path() {
    Some(file) -> load_from(file)
    None -> subscription_recovery.initial()
  }
}

pub fn load_from(file: String) -> State {
  case simplifile.read(file) {
    Ok(contents) ->
      json.parse(contents, decoder())
      |> result.unwrap(subscription_recovery.initial())
    Error(_) -> subscription_recovery.initial()
  }
}

pub fn save(state: State) -> Nil {
  case path() {
    None -> Nil
    Some(file) ->
      case save_to(file, state) {
        Ok(_) -> Nil
        Error(_) ->
          io.println("Recovery state could not be saved; retries remain active")
      }
  }
}

pub fn save_to(file: String, state: State) -> Result(Nil, String) {
  let parent = file |> string.split("/") |> list_drop_last |> string.join("/")
  use _ <- result.try(
    simplifile.create_directory_all(parent)
    |> result.map_error(fn(_) { "cannot prepare recovery status directory" }),
  )
  use _ <- result.try(
    simplifile.write(file <> ".tmp", json.to_string(to_json(state)))
    |> result.map_error(fn(_) { "cannot save recovery state" }),
  )
  simplifile.rename(file <> ".tmp", file)
  |> result.map_error(fn(_) { "cannot publish recovery state" })
}

fn list_drop_last(values: List(String)) -> List(String) {
  case values {
    [] | [_] -> []
    [first, ..rest] -> [first, ..list_drop_last(rest)]
  }
}

pub fn to_json(state: State) -> json.Json {
  json.object([
    #("state", json.string(state.status)),
    #("consecutive_failures", json.int(state.failures)),
    #("next_retry_at", json.int(state.retry_at)),
    #("last_success_at", json.int(state.last_success_at)),
  ])
}

fn decoder() -> decode.Decoder(State) {
  use status <- decode.field("state", decode.string)
  use failures <- decode.field("consecutive_failures", decode.int)
  use retry_at <- decode.field("next_retry_at", decode.int)
  use success <- decode.field("last_success_at", decode.int)
  decode.success(State(
    int.clamp(failures, 0, 20),
    int.max(retry_at, 0),
    int.max(success, 0),
    status,
  ))
}

fn path() {
  case envoy.get("DATA_DIR") {
    Ok(root) if root != "" -> Some(root <> "/cache/subscription-recovery.json")
    _ -> None
  }
}
