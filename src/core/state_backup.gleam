/// Durable daily snapshots of the SQLite state and subscription files.
/// The database copy is made by SQLite itself with VACUUM INTO, so committed
/// rows still in the WAL are included in one consistent snapshot.
import engine/download_history
import envoy
import gleam/erlang/process
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import gleam/time/timestamp
import infra/db.{type Db}
import simplifile

const retry_seconds = 3600

const daily_seconds = 86_400

/// Start an immediate snapshot worker when STATE_BACKUP_DIR is configured.
/// Failures retry hourly; successful daily snapshots run once per day.
pub fn start(database: Db) -> process.Pid {
  case backup_directory() {
    Some(backup_dir) -> {
      let data_root = data_root()
      process.spawn(fn() { backup_loop(database, data_root, backup_dir) })
    }
    None -> process.spawn(fn() { process.sleep_forever() })
  }
}

/// Create today's snapshot. This entry point is injectable for tests.
/// Returns the completed snapshot directory path.
pub fn run_once(
  database: Db,
  data_root: String,
  backup_dir: String,
  now_seconds: Int,
) -> Result(String, String) {
  let day = day_key(now_seconds)
  case backup_prepare(data_root, backup_dir, day) {
    Error(message) -> Error(message)
    Ok(#(False, _, final_dir)) -> {
      case backup_prune(backup_dir) {
        Ok(_) -> Ok(final_dir)
        Error(message) ->
          Error("snapshot exists but retention failed: " <> message)
      }
    }
    Ok(#(True, stage_dir, final_dir)) ->
      case
        create_snapshot(database, data_root, stage_dir, final_dir, backup_dir)
      {
        Ok(path) -> Ok(path)
        Error(message) -> Error(message)
      }
  }
}

fn create_snapshot(
  database: Db,
  data_root: String,
  stage_dir: String,
  final_dir: String,
  backup_dir: String,
) -> Result(String, String) {
  let database_copy = stage_dir <> "/database.sqlite"
  let sql = "VACUUM INTO '" <> string.replace(database_copy, "'", "''") <> "'"
  case db.exec_raw(database, sql) {
    Error(error) -> {
      let _ = backup_abort(stage_dir)
      Error("SQLite snapshot failed: " <> db_error_message(error))
    }
    Ok(_) ->
      case backup_sync_file(database_copy) {
        Error(message) -> fail_stage(stage_dir, message)
        Ok(_) ->
          case copy_state_files(data_root, stage_dir) {
            Error(message) -> fail_stage(stage_dir, message)
            Ok(_) ->
              case backup_publish(stage_dir, final_dir) {
                Error(message) -> fail_stage(stage_dir, message)
                Ok(_) ->
                  case backup_prune(backup_dir) {
                    Ok(_) -> Ok(final_dir)
                    Error(message) ->
                      Error("snapshot saved but retention failed: " <> message)
                  }
              }
          }
      }
  }
}

fn copy_state_files(
  data_root: String,
  stage_dir: String,
) -> Result(Nil, String) {
  let files = [
    #(data_root <> "/ytdl-sub/channels.txt", "channels.txt"),
    #(data_root <> "/ytdl-sub/desired-channels.txt", "desired-channels.txt"),
    #(data_root <> "/ytdl-sub/channel_ids.txt", "channel_ids.txt"),
  ]
  use _ <- result.try(
    list.try_each(files, fn(file) {
      backup_copy_file(file.0, stage_dir <> "/" <> file.1)
      |> result.map(fn(_) { Nil })
    }),
  )
  use ids <- result.try(download_history.known_ids(data_root))
  let archive = case ids {
    [] -> ""
    ids ->
      ids
      |> list.map(fn(id) { "youtube " <> id })
      |> string.join("\n")
      |> fn(contents) { contents <> "\n" }
  }
  let path = stage_dir <> "/archive.txt"
  case simplifile.write(path, archive) {
    Ok(_) -> backup_sync_file(path)
    Error(_) -> Error("cannot write backup download archive")
  }
}

fn db_error_message(error: db.DbError) -> String {
  case error {
    db.ConnectionError(message) -> message
    db.QueryError(message) -> message
    db.MigrationError(message) -> message
  }
}

fn fail_stage(stage_dir: String, message: String) -> Result(String, String) {
  let _ = backup_abort(stage_dir)
  Error(message)
}

fn backup_loop(database: Db, data_root: String, backup_dir: String) -> Nil {
  let next_delay = case
    run_once(database, data_root, backup_dir, now_seconds())
  {
    // Snapshot folders use UTC days. Sleep to the next day boundary so a
    // same-day service restart cannot postpone tomorrow's snapshot by 24h.
    Ok(_) -> seconds_until_next_day(now_seconds())
    Error(message) -> {
      io.println("State backup failed; retrying in one hour: " <> message)
      retry_seconds
    }
  }
  process.sleep(next_delay * 1000)
  backup_loop(database, data_root, backup_dir)
}

pub fn seconds_until_next_day(now_seconds: Int) -> Int {
  daily_seconds - now_seconds % daily_seconds
}

fn data_root() -> String {
  case envoy.get("DATA_DIR") {
    Ok(path) -> path
    Error(_) -> "./data"
  }
}

fn backup_directory() -> Option(String) {
  case envoy.get("STATE_BACKUP_DIR") {
    Ok(path) ->
      case string.trim(path) {
        "" -> None
        path -> Some(path)
      }
    _ -> None
  }
}

fn now_seconds() -> Int {
  let #(seconds, _) =
    timestamp.system_time() |> timestamp.to_unix_seconds_and_nanoseconds
  seconds
}

@external(erlang, "state_backup_ffi", "day_key")
fn day_key(seconds: Int) -> String

@external(erlang, "state_backup_ffi", "prepare")
fn backup_prepare(
  data_root: String,
  backup_dir: String,
  day: String,
) -> Result(#(Bool, String, String), String)

@external(erlang, "state_backup_ffi", "copy_file")
fn backup_copy_file(source: String, destination: String) -> Result(Bool, String)

@external(erlang, "state_backup_ffi", "sync_file")
fn backup_sync_file(path: String) -> Result(Nil, String)

@external(erlang, "state_backup_ffi", "publish")
fn backup_publish(stage_dir: String, final_dir: String) -> Result(Nil, String)

@external(erlang, "state_backup_ffi", "abort")
fn backup_abort(stage_dir: String) -> Result(Nil, String)

@external(erlang, "state_backup_ffi", "prune")
fn backup_prune(backup_dir: String) -> Result(Nil, String)
