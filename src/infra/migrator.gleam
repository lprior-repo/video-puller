/// Database migration runner
///
/// Handles running SQL migrations from the priv/migrations directory.
/// Migrations are run in order and tracked to prevent re-running.
/// Uses Cake query builder for tracking queries.
import cake/insert
import cake/select
import cake/where
import gleam/dynamic/decode
import gleam/erlang
import gleam/list
import gleam/result
import gleam/string
import gleam_time
import infra/db.{type Db, type DbError}
import simplifile

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
  db.exec_raw(conn, sql)
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
      use _ <- result.try(db.exec_raw(conn, sql))
      record_migration(conn, filename)
    }
  }
}

/// Check if a migration has already been applied
fn is_migration_applied(conn: Db, filename: String) -> Result(Bool, DbError) {
  let query =
    select.new()
    |> select.from_table("schema_migrations")
    |> select.select_cols(["COUNT(*)"])
    |> select.where(where.eq(where.col("version"), where.string(filename)))
    |> select.to_query()

  use rows <- result.try(db.run_read(conn, query, decode.at([0], decode.int)))

  case list.first(rows) {
    Ok(count) -> Ok(count > 0)
    Error(_) -> Ok(False)
  }
}

/// Record that a migration has been applied
fn record_migration(conn: Db, filename: String) -> Result(Nil, DbError) {
  let timestamp = get_timestamp()

  let query =
    insert.from_values(
      table_name: "schema_migrations",
      columns: ["version", "applied_at"],
      values: [insert.row([insert.string(filename), insert.int(timestamp)])],
    )
    |> insert.to_query()

  db.run_write(conn, query, decode.dynamic)
  |> result.replace_error(db.MigrationError(
    "Failed to record migration: " <> filename,
  ))
  |> result.replace(Nil)
}

/// Get current Unix timestamp in seconds
fn get_timestamp() -> Int {
  gleam_time.now_utc()
  |> gleam_time.to_unix_utc()
}

/// Get the priv directory for the application
/// Returns empty string on error (for fallback to development path)
fn get_priv_directory(app: String) -> String {
  case erlang.priv_directory(app) {
    Ok(path) -> path
    Error(_) -> ""
  }
}

fn get_migrations_path() -> String {
  // Try to use Erlang's code:priv_dir first (for releases)
  // Fall back to local priv/migrations for development
  case get_priv_directory("video_puller") {
    "" -> "priv/migrations"
    priv_dir -> priv_dir <> "/migrations"
  }
}
