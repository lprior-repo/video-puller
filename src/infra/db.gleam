/// Database connection factory and utilities
///
/// This module handles SQLite connections using Cake query builder
/// with proper WAL mode configuration for concurrency.
import cake.{type ReadQuery, type WriteQuery}
import cake/adapter/sqlite
import gleam/dynamic/decode
import gleam/result
import sqlight

/// Database connection type
pub type Db =
  sqlight.Connection

/// Database error type
pub type DbError {
  ConnectionError(String)
  QueryError(String)
  MigrationError(String)
}

/// Open a connection to the SQLite database
/// The database file will be created if it doesn't exist
pub fn connect(path: String) -> Result(Db, DbError) {
  case sqlight.open(path) {
    Ok(conn) -> Ok(conn)
    Error(err) ->
      Error(ConnectionError("Failed to open database: " <> sqlight_error(err)))
  }
}

/// Close a database connection
pub fn close(conn: Db) -> Result(Nil, DbError) {
  sqlight.close(conn)
  |> result.map_error(fn(err) {
    ConnectionError("Failed to close database: " <> sqlight_error(err))
  })
}

/// Initialize a database with WAL mode enabled
pub fn init_db(path: String) -> Result(Db, DbError) {
  use conn <- result.try(connect(path))

  // Enable WAL mode for better concurrency
  case exec_raw(conn, "PRAGMA journal_mode=WAL;") {
    Ok(_) -> Ok(conn)
    Error(e) -> {
      let _ = close(conn)
      Error(e)
    }
  }
}

// ---- Cake Query Builder API ----

/// Execute a Cake read query (SELECT) and return decoded rows
pub fn run_read(
  conn: Db,
  query: ReadQuery,
  decoder: decode.Decoder(a),
) -> Result(List(a), DbError) {
  sqlite.run_read_query(query, decoder, conn)
  |> result.map_error(fn(err) {
    QueryError("Read query failed: " <> sqlight_error(err))
  })
}

/// Execute a Cake write query (INSERT/UPDATE/DELETE)
pub fn run_write(
  conn: Db,
  query: WriteQuery(a),
  decoder: decode.Decoder(b),
) -> Result(List(b), DbError) {
  sqlite.run_write_query(query, decoder, conn)
  |> result.map_error(fn(err) {
    QueryError("Write query failed: " <> sqlight_error(err))
  })
}

/// Execute raw SQL without returning rows (for DDL/migrations)
pub fn exec_raw(conn: Db, sql: String) -> Result(Nil, DbError) {
  sqlite.execute_raw_sql(sql, conn)
  |> result.map_error(fn(err) {
    QueryError("Raw SQL failed: " <> sqlight_error(err))
  })
}

// ---- Error Handling ----

/// Convert sqlight error to string
pub fn sqlight_error(err: sqlight.Error) -> String {
  case err {
    sqlight.SqlightError(_code, message, _offset) -> message
  }
}

/// Re-export sqlight error type for pattern matching in repos
pub type SqliteError =
  sqlight.Error
