/// Database migration atomicity tests.
import gleam/dynamic/decode
import gleam/list
import gleam/result
import gleeunit
import gleeunit/should
import infra/db
import infra/migrator
import simplifile
import sqlight

pub fn main() {
  gleeunit.main()
}

pub fn migration_ddl_and_tracking_row_roll_back_together_test() {
  let path = "/tmp/test_migrator_atomicity.db"
  remove_database(path)
  let conn = db.init_db(path) |> should.be_ok()

  // Migration 002 adds title before it reaches the conflicting column.
  // The failure must roll back that earlier ALTER as well as its tracking row.
  db.exec_raw(
    conn,
    "
    CREATE TABLE video_jobs (
      id TEXT PRIMARY KEY NOT NULL,
      url TEXT NOT NULL,
      status TEXT NOT NULL,
      created_at INTEGER NOT NULL,
      updated_at INTEGER NOT NULL,
      thumbnail_url TEXT
    );
    ",
  )
  |> should.be_ok()

  migrator.run_migrations(conn) |> should.be_error()
  column_count(conn, "title") |> should.equal(0)
  migration_count(conn, "002_add_metadata.sql") |> should.equal(0)
  migration_count(conn, "001_init.sql") |> should.equal(1)

  // Remove the deliberate conflict, then verify retry completes and subsequent
  // startup is a no-op with one tracking row per shipped migration.
  db.exec_raw(conn, "ALTER TABLE video_jobs DROP COLUMN thumbnail_url")
  |> should.be_ok()
  migrator.run_migrations(conn) |> should.be_ok()
  migrator.run_migrations(conn) |> should.be_ok()
  // Historical migration 001 must not downgrade durable connection settings.
  sqlight.query("PRAGMA synchronous", conn, [], decode.at([0], decode.int))
  |> should.equal(Ok([2]))
  column_count(conn, "title") |> should.equal(1)
  migration_count(conn, "002_add_metadata.sql") |> should.equal(1)
  sqlight.query(
    "SELECT COUNT(*) FROM schema_migrations",
    conn,
    [],
    decode.at([0], decode.int),
  )
  |> should.equal(Ok([5]))

  db.close(conn) |> should.be_ok()
  remove_database(path)
}

fn column_count(conn: db.Db, column: String) -> Int {
  sqlight.query(
    "SELECT COUNT(*) FROM pragma_table_info('video_jobs') WHERE name = ?",
    conn,
    [sqlight.text(column)],
    decode.at([0], decode.int),
  )
  |> should.be_ok
  |> list.first
  |> result.unwrap(0)
}

fn migration_count(conn: db.Db, version: String) -> Int {
  sqlight.query(
    "SELECT COUNT(*) FROM schema_migrations WHERE version = ?",
    conn,
    [sqlight.text(version)],
    decode.at([0], decode.int),
  )
  |> should.be_ok
  |> list.first
  |> result.unwrap(0)
}

fn remove_database(path: String) {
  let _ = simplifile.delete(path)
  let _ = simplifile.delete(path <> "-wal")
  let _ = simplifile.delete(path <> "-shm")
  Nil
}
