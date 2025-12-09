/// Database connection factory and utilities
///
/// This module handles SQLite connections using sqlight,
/// with proper WAL mode configuration for concurrency.
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
      Error(ConnectionError("Failed to open database: " <> string_error(err)))
  }
}

/// Close a database connection
pub fn close(conn: Db) -> Result(Nil, DbError) {
  sqlight.close(conn)
  |> result.map_error(fn(err) {
    ConnectionError("Failed to close database: " <> string_error(err))
  })
}

/// Execute a SQL statement without returning rows
pub fn exec(conn: Db, sql: String) -> Result(Nil, DbError) {
  sqlight.exec(sql, conn)
  |> result.map(fn(_) { Nil })
  |> result.map_error(fn(err) {
    QueryError("Failed to execute SQL: " <> string_error(err))
  })
}

/// Execute a SQL query and return rows
pub fn query(
  conn: Db,
  sql: String,
  args: List(sqlight.Value),
  decoder: decode.Decoder(a),
) -> Result(List(a), DbError) {
  case sqlight.query(sql, conn, args, decoder) {
    Ok(rows) -> Ok(rows)
    Error(err) ->
      Error(QueryError("Failed to execute query: " <> string_error(err)))
  }
}

/// Convert sqlight error to string
fn string_error(err: sqlight.Error) -> String {
  case err {
    sqlight.SqlightError(_code, message, _offset) -> message
  }
}

/// Initialize a database with WAL mode enabled
pub fn init_db(path: String) -> Result(Db, DbError) {
  use conn <- result.try(connect(path))

  // Enable WAL mode for better concurrency
  case exec(conn, "PRAGMA journal_mode=WAL;") {
    Ok(_) -> Ok(conn)
    Error(e) -> {
      let _ = close(conn)
      Error(e)
    }
  }
}
