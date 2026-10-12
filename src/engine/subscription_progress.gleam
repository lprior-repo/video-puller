/// Durable per-worker ytdl-sub progress snapshots for the local UI.
///
/// A worker owns one file, so concurrent downloads never overwrite each
/// other's state. Each update writes a temporary file and atomically renames
/// it into place.
import gleam/dynamic/decode
import gleam/float
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import simplifile

pub type Snapshot {
  Snapshot(
    key: String,
    channel: String,
    phase: String,
    status: String,
    message: String,
    percent: Option(Int),
    phase_total: Int,
    started_at: Int,
    updated_at: Int,
  )
}

/// Prepare all workers for a phase. A new recent phase starts a fresh poll;
/// backfill keeps the recent phase's terminal rows visible to the UI.
pub fn prepare_phase(
  data_root: String,
  phase: String,
  workers: List(#(String, String)),
  phase_total: Int,
) -> Nil {
  let directory = progress_directory(data_root)
  let _ = simplifile.create_directory_all(directory)
  case phase {
    "recent" -> clear_snapshots(directory)
    _ -> Nil
  }
  let timestamp = now()
  list.each(workers, fn(worker) {
    let #(key, channel) = worker
    write_snapshot(
      directory,
      Snapshot(
        key: snapshot_key(phase, key),
        channel: channel,
        phase: phase,
        status: "queued",
        message: "Waiting for a worker",
        percent: None,
        phase_total: phase_total,
        started_at: timestamp,
        updated_at: timestamp,
      ),
    )
  })
}

/// Mark one worker active before its process starts emitting output.
pub fn start(
  data_root: String,
  phase: String,
  key: String,
  channel: String,
  phase_total: Int,
) -> Nil {
  update(data_root, phase, key, fn(snapshot) {
    Snapshot(
      ..snapshot,
      channel: channel,
      status: "running",
      message: "Starting ytdl-sub",
      percent: None,
      phase_total: phase_total,
      started_at: now(),
      updated_at: now(),
    )
  })
}

/// Record a useful engine output line, ignoring repetitive diagnostics.
pub fn observe(
  data_root: String,
  phase: String,
  key: String,
  line: String,
) -> Nil {
  case meaningful_line(line) {
    False -> Nil
    True ->
      update(data_root, phase, key, fn(snapshot) {
        Snapshot(
          ..snapshot,
          message: truncate(string.trim(line), 240),
          percent: progress_percent(line, snapshot.percent),
          updated_at: now(),
        )
      })
  }
}

/// Mark a worker's terminal outcome while preserving its last useful line.
pub fn finish(
  data_root: String,
  phase: String,
  key: String,
  outcome: Result(value, String),
) -> Nil {
  update(data_root, phase, key, fn(snapshot) {
    let #(status, message) = case outcome {
      Ok(_) -> #("completed", snapshot.message)
      Error(error) -> #("failed", truncate(error, 240))
    }
    Snapshot(..snapshot, status: status, message: message, updated_at: now())
  })
}

/// Close a phase after its runner returns, including when an outer pool
/// deadline stopped workers before they could record their own outcomes.
pub fn close_phase(data_root: String, phase: String, reason: String) -> Nil {
  let directory = progress_directory(data_root)
  case simplifile.read_directory(directory) {
    Error(_) -> Nil
    Ok(files) ->
      list.each(files, fn(file) {
        case string.ends_with(file, ".json") {
          False -> Nil
          True ->
            case simplifile.read(directory <> "/" <> file) {
              Error(_) -> Nil
              Ok(content) ->
                case json.parse(content, snapshot_decoder()) {
                  Error(_) -> Nil
                  Ok(snapshot) ->
                    case
                      snapshot.phase == phase
                      && list.any(["running", "queued"], fn(status) {
                        snapshot.status == status
                      })
                    {
                      True ->
                        write_snapshot(
                          directory,
                          Snapshot(
                            ..snapshot,
                            status: "failed",
                            message: truncate(reason, 240),
                            updated_at: now(),
                          ),
                        )
                      False -> Nil
                    }
                }
            }
        }
      })
  }
}

/// Read all snapshots. Running rows older than the maximum poll lifetime are
/// exposed as stale so a killed application cannot look permanently active.
pub fn read_snapshots(data_root: String) -> List(Snapshot) {
  let directory = progress_directory(data_root)
  case simplifile.read_directory(directory) {
    Error(_) -> []
    Ok(files) ->
      files
      |> list.filter(fn(file) { string.ends_with(file, ".json") })
      |> list.filter_map(fn(file) {
        case simplifile.read(directory <> "/" <> file) {
          Error(_) -> Error(Nil)
          Ok(content) ->
            case json.parse(content, snapshot_decoder()) {
              Error(_) -> Error(Nil)
              Ok(snapshot) -> Ok(mark_stale(snapshot))
            }
        }
      })
  }
}

fn mark_stale(snapshot: Snapshot) -> Snapshot {
  case
    { snapshot.status == "running" || snapshot.status == "queued" }
    && now() - snapshot.updated_at > 43_200_000
  {
    True ->
      Snapshot(..snapshot, status: "stale", message: "Worker stopped reporting")
    False -> snapshot
  }
}

fn update(
  data_root: String,
  phase: String,
  key: String,
  change: fn(Snapshot) -> Snapshot,
) -> Nil {
  let directory = progress_directory(data_root)
  let file = directory <> "/" <> snapshot_key(phase, key) <> ".json"
  case simplifile.read(file) {
    Error(_) -> Nil
    Ok(content) ->
      case json.parse(content, snapshot_decoder()) {
        Error(_) -> Nil
        Ok(snapshot) -> write_snapshot(directory, change(snapshot))
      }
  }
}

fn write_snapshot(directory: String, snapshot: Snapshot) -> Nil {
  let path = directory <> "/" <> snapshot.key <> ".json"
  let _ = simplifile.create_directory_all(directory)
  case simplifile.write(path <> ".tmp", encode_snapshot(snapshot)) {
    Error(_) -> Nil
    Ok(_) -> {
      let _ = simplifile.rename(path <> ".tmp", path)
      Nil
    }
  }
}

fn encode_snapshot(snapshot: Snapshot) -> String {
  let percent = case snapshot.percent {
    Some(value) -> json.int(value)
    None -> json.int(-1)
  }
  json.object([
    #("key", json.string(snapshot.key)),
    #("channel", json.string(snapshot.channel)),
    #("phase", json.string(snapshot.phase)),
    #("status", json.string(snapshot.status)),
    #("message", json.string(snapshot.message)),
    #("percent", percent),
    #("phase_total", json.int(snapshot.phase_total)),
    #("started_at", json.int(snapshot.started_at)),
    #("updated_at", json.int(snapshot.updated_at)),
  ])
  |> json.to_string
}

fn snapshot_decoder() -> decode.Decoder(Snapshot) {
  use key <- decode.field("key", decode.string)
  use channel <- decode.field("channel", decode.string)
  use phase <- decode.field("phase", decode.string)
  use status <- decode.field("status", decode.string)
  use message <- decode.field("message", decode.string)
  use percent <- decode.field("percent", decode.int)
  use phase_total <- decode.field("phase_total", decode.int)
  use started_at <- decode.field("started_at", decode.int)
  use updated_at <- decode.field("updated_at", decode.int)
  decode.success(Snapshot(
    key: key,
    channel: channel,
    phase: phase,
    status: status,
    message: message,
    percent: case percent < 0 {
      True -> None
      False -> Some(percent)
    },
    phase_total: phase_total,
    started_at: started_at,
    updated_at: updated_at,
  ))
}

fn clear_snapshots(directory: String) -> Nil {
  case simplifile.read_directory(directory) {
    Error(_) -> Nil
    Ok(files) ->
      list.each(files, fn(file) {
        case
          string.ends_with(file, ".json") || string.ends_with(file, ".json.tmp")
        {
          True -> {
            let _ = simplifile.delete(directory <> "/" <> file)
            Nil
          }
          False -> Nil
        }
      })
  }
}

fn progress_directory(data_root: String) -> String {
  data_root <> "/ytdl-sub/progress"
}

fn snapshot_key(phase: String, key: String) -> String {
  phase <> "-" <> key
}

fn meaningful_line(line: String) -> Bool {
  let line = string.trim(line)
  !string.is_empty(line)
  && !string.contains(line, "X-Plex-Token")
  && list.any(
    [
      "[download]",
      "Downloading",
      "Destination:",
      "Extracting URL:",
      "[youtube]",
      "ERROR",
      "Error:",
      "WARNING",
      "Files created:",
      "already been downloaded",
      "Post-processing",
    ],
    fn(marker) { string.contains(line, marker) },
  )
}

fn progress_percent(line: String, current: Option(Int)) -> Option(Int) {
  case starts_new_item(line) {
    True -> None
    False ->
      case parse_percent(line) {
        Some(percent) -> Some(percent)
        None -> current
      }
  }
}

fn starts_new_item(line: String) -> Bool {
  list.any(
    ["Extracting URL:", "[youtube]", "Downloading webpage", "Destination:"],
    fn(marker) { string.contains(line, marker) },
  )
}

fn parse_percent(line: String) -> Option(Int) {
  case string.split(line, "%") {
    [before, ..] ->
      case list.last(string.split(before, " ")) {
        Error(_) -> None
        Ok(number) ->
          case float.parse(number) {
            Ok(value) -> Some(int.clamp(float.round(value), 0, 100))
            Error(_) -> None
          }
      }
    [] -> None
  }
}

fn truncate(value: String, limit: Int) -> String {
  case string.length(value) > limit {
    True -> string.slice(value, 0, limit) <> "…"
    False -> value
  }
}

type TimeUnit {
  Millisecond
}

@external(erlang, "erlang", "system_time")
fn system_time(unit: TimeUnit) -> Int

fn now() -> Int {
  system_time(Millisecond)
}
