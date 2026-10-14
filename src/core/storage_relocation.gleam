/// Safely retarget absolute paths in a copied Video Puller database.
///
/// The source tree is only inspected for path validation. All writes go to
/// the already-copied database beneath new_root.
import gleam/list
import gleam/result
import gleam/string
import infra/db
import simplifile

pub type Summary {
  Summary(old_root: String, new_root: String, database: String)
}

/// Update persisted file paths in new_root/video_eater.db that still point
/// beneath old_root. Video metadata, IDs, URLs, scheduler settings, and
/// subscription state are left untouched.
pub fn relocate(old_root: String, new_root: String) -> Result(Summary, String) {
  use old_root <- result.try(validate_root(old_root))
  use new_root <- result.try(validate_root(new_root))
  use _ <- result.try(validate_pair(old_root, new_root))
  use _ <- result.try(require_directory(old_root, "source"))
  use _ <- result.try(require_directory(new_root, "destination"))
  let source_database = old_root <> "/video_eater.db"
  use _ <- result.try(require_file(source_database, "source"))

  let database = new_root <> "/video_eater.db"
  use _ <- result.try(require_file(database, "destination"))
  use _ <- result.try(reject_same_database(source_database, database))
  use conn <- result.try(
    db.connect(database)
    |> result.map_error(fn(_) { "cannot open copied video_eater.db" }),
  )

  let outcome = update_database_paths(conn, old_root, new_root)
  let _ = db.close(conn)
  use _ <- result.try(outcome)
  use _ <- result.try(invalidate_generated_configs(new_root))
  Ok(Summary(old_root, new_root, database))
}

/// Validate a canonical absolute path without resolving or following symlinks.
pub fn validate_root(path: String) -> Result(String, String) {
  case string.starts_with(path, "/") && path != "/" {
    False -> Error("root must be a canonical absolute directory path")
    True -> {
      let parts = string.split(string.drop_start(path, 1), "/")
      case
        list.any(parts, fn(part) { part == "" || part == "." || part == ".." })
      {
        True -> Error("root must not contain empty, '.' or '..' path segments")
        False -> Ok(path)
      }
    }
  }
}

fn validate_pair(old_root: String, new_root: String) -> Result(Nil, String) {
  case
    old_root == new_root
    || path_is_within(old_root, new_root)
    || path_is_within(new_root, old_root)
  {
    True -> Error("source and destination roots must be separate trees")
    False -> Ok(Nil)
  }
}

fn path_is_within(path: String, parent: String) -> Bool {
  string.starts_with(path, parent <> "/")
}

fn require_directory(path: String, label: String) -> Result(Nil, String) {
  case simplifile.is_symlink(path), simplifile.is_directory(path) {
    Ok(False), Ok(True) -> Ok(Nil)
    Ok(True), _ -> Error(label <> " root must not be a symbolic link")
    _, _ -> Error(label <> " root does not exist or is not a directory")
  }
}

fn require_file(path: String, label: String) -> Result(Nil, String) {
  case simplifile.is_symlink(path), simplifile.is_file(path) {
    Ok(False), Ok(True) -> Ok(Nil)
    Ok(True), _ -> Error(label <> " database must not be a symbolic link")
    _, _ -> Error(label <> " root must contain video_eater.db")
  }
}

fn reject_same_database(
  source: String,
  destination: String,
) -> Result(Nil, String) {
  use source_info <- result.try(
    simplifile.file_info(source)
    |> result.map_error(fn(_) { "cannot stat source database" }),
  )
  use destination_info <- result.try(
    simplifile.file_info(destination)
    |> result.map_error(fn(_) { "cannot stat destination database" }),
  )
  case
    source_info.dev == destination_info.dev
    && source_info.inode == destination_info.inode
  {
    True -> Error("source and destination point to the same physical database")
    False -> Ok(Nil)
  }
}

/// Remove generated configs with embedded absolute paths. Startup recreates
/// config.yaml, and the first coordinator poll renders both subscription files
/// and all per-channel job configs from the relocated layout.
fn invalidate_generated_configs(root: String) -> Result(Nil, String) {
  let directory = root <> "/ytdl-sub"
  let files = [
    directory <> "/config.yaml",
    directory <> "/subscriptions.yaml",
    directory <> "/subscriptions.yaml.jobs",
    directory <> "/subscriptions-recent.yaml",
    directory <> "/subscriptions-recent.yaml.jobs",
  ]
  list.try_map(files, fn(path) {
    case simplifile.is_file(path) {
      Ok(True) ->
        simplifile.delete(path)
        |> result.map_error(fn(_) {
          "cannot remove a stale generated ytdl-sub config in destination"
        })
      _ -> Ok(Nil)
    }
  })
  |> result.map(fn(_) { Nil })
}

fn update_database_paths(
  conn: db.Db,
  old_root: String,
  new_root: String,
) -> Result(Nil, String) {
  use _ <- result.try(
    db.exec_raw(conn, "BEGIN IMMEDIATE")
    |> result.map_error(fn(_) { "cannot start database path update" }),
  )
  let paths = [
    #("video_jobs", "path"),
    #("download_config", "output_directory"),
  ]
  let update =
    list.try_map(paths, fn(path) {
      let #(table, column) = path
      db.exec_raw(conn, path_update_sql(table, column, old_root, new_root))
      |> result.map_error(fn(_) {
        "database schema is incomplete; path update was rolled back"
      })
    })
  case update {
    Ok(_) ->
      db.exec_raw(conn, "COMMIT")
      |> result.map_error(fn(_) { "cannot commit database path update" })
    Error(message) -> {
      let _ = db.exec_raw(conn, "ROLLBACK")
      Error(message)
    }
  }
}

fn path_update_sql(
  table: String,
  column: String,
  old_root: String,
  new_root: String,
) -> String {
  let old_root = sql_quote(old_root)
  let new_root = sql_quote(new_root)
  "UPDATE "
  <> table
  <> " SET "
  <> column
  <> " = '"
  <> new_root
  <> "' || substr("
  <> column
  <> ", length('"
  <> old_root
  <> "') + 1) WHERE "
  <> column
  <> " = '"
  <> old_root
  <> "' OR substr("
  <> column
  <> ", 1, length('"
  <> old_root
  <> "') + 1) = '"
  <> old_root
  <> "/'"
}

fn sql_quote(value: String) -> String {
  string.replace(value, "'", "''")
}
