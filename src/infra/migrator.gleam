/// Database migration runner
///
/// Handles running SQL migrations from the priv/migrations directory.
/// Migrations are run in order and tracked to prevent re-running.
import gleam/dynamic/decode
import gleam/list
import gleam/result
import gleam/string
import infra/db.{type Db, type DbError}
import simplifile
import sqlight

/// Run all pending migrations
pub fn run_migrations(conn: Db) -> Result(Nil, DbError) {
  use _ <- result.try(create_migrations_table(conn))
  use migrations <- result.try(load_migration_files())
  run_migration_list(conn, migrations)
}

/// Create the migrations tracking table if it doesn't exist
fn create_migrations_table(conn: Db) -> Result(Nil, DbError) {
  let sql =
    "
    CREATE TABLE IF NOT EXISTS schema_migrations (
      version TEXT PRIMARY KEY NOT NULL,
      applied_at INTEGER NOT NULL
    );
  "
  db.exec(conn, sql)
}

/// Load all SQL migration files from priv/migrations
fn load_migration_files() -> Result(List(#(String, String)), DbError) {
  let migrations_dir = get_migrations_path()
  case simplifile.read_directory(migrations_dir) {
    Ok(files) -> {
      files
      |> list.filter(string.ends_with(_, ".sql"))
      |> list.sort(string.compare)
      |> list.try_map(fn(filename) {
        case simplifile.read(migrations_dir <> "/" <> filename) {
          Ok(content) -> Ok(#(filename, content))
          Error(_) ->
            Error(db.MigrationError(
              "Failed to read migration file: " <> filename,
            ))
        }
      })
    }
    Error(_) -> Error(db.MigrationError("Failed to read migrations directory"))
  }
}

/// Run a list of migrations in order
fn run_migration_list(
  conn: Db,
  migrations: List(#(String, String)),
) -> Result(Nil, DbError) {
  list.try_fold(migrations, Nil, fn(_, migration) {
    let #(filename, sql) = migration
    run_migration(conn, filename, sql)
  })
}

/// Run a single migration if it hasn't been applied yet
fn run_migration(
  conn: Db,
  filename: String,
  sql: String,
) -> Result(Nil, DbError) {
  use already_applied <- result.try(is_migration_applied(conn, filename))

  case already_applied {
    True -> Ok(Nil)
    False -> {
      use _ <- result.try(db.exec(conn, sql))
      record_migration(conn, filename)
    }
  }
}

/// Check if a migration has already been applied
fn is_migration_applied(conn: Db, filename: String) -> Result(Bool, DbError) {
  let sql = "SELECT COUNT(*) as count FROM schema_migrations WHERE version = ?"

  use rows <- result.try(db.query(
    conn,
    sql,
    [sqlight.text(filename)],
    decode.at([0], decode.int),
  ))

  case list.first(rows) {
    Ok(count) -> Ok(count > 0)
    Error(_) -> Ok(False)
  }
}

/// Record that a migration has been applied
fn record_migration(conn: Db, filename: String) -> Result(Nil, DbError) {
  let sql = "INSERT INTO schema_migrations (version, applied_at) VALUES (?, ?)"

  let timestamp = get_timestamp()

  db.query(
    conn,
    sql,
    [sqlight.text(filename), sqlight.int(timestamp)],
    decode.at([0], decode.int),
  )
  |> result.replace_error(db.MigrationError(
    "Failed to record migration: " <> filename,
  ))
  |> result.replace(Nil)
}

/// Get current Unix timestamp
@external(erlang, "os", "system_time")
fn get_timestamp() -> Int

/// Get the path to the migrations directory
/// In development, uses priv/migrations
/// In release, uses the Erlang application priv directory
@external(erlang, "video_puller_ffi", "priv_directory")
fn get_priv_directory(app: String) -> String

fn get_migrations_path() -> String {
  // Try to use Erlang's code:priv_dir first (for releases)
  // Fall back to local priv/migrations for development
  case get_priv_directory("video_puller") {
    "" -> "priv/migrations"
    priv_dir -> priv_dir <> "/migrations"
  }
}
