/// Database initialization tests.
import gleam/dynamic/decode
import gleeunit
import gleeunit/should
import infra/db
import simplifile
import sqlight

pub fn main() {
  gleeunit.main()
}

pub fn init_sets_durable_wal_pragmas_test() {
  let path = "/tmp/test_db_durability.db"
  remove_database(path)
  let conn = db.init_db(path) |> should.be_ok()

  sqlight.query("PRAGMA journal_mode;", conn, [], decode.at([0], decode.string))
  |> should.equal(Ok(["wal"]))
  sqlight.query("PRAGMA synchronous;", conn, [], decode.at([0], decode.int))
  |> should.equal(Ok([2]))
  sqlight.query("PRAGMA busy_timeout;", conn, [], decode.at([0], decode.int))
  |> should.equal(Ok([5000]))
  sqlight.query("PRAGMA quick_check;", conn, [], decode.at([0], decode.string))
  |> should.equal(Ok(["ok"]))

  db.close(conn) |> should.be_ok()
  remove_database(path)
}

pub fn init_rejects_corrupt_database_test() {
  let path = "/tmp/test_db_corrupt.db"
  remove_database(path)
  simplifile.write(path, "this is not a SQLite database") |> should.be_ok()

  db.init_db(path) |> should.be_error()
  remove_database(path)
}

pub fn init_rejects_unopenable_database_path_test() {
  let path = "/dev/null/video-puller.sqlite"
  db.init_db(path) |> should.be_error()
}

fn remove_database(path: String) {
  let _ = simplifile.delete(path)
  let _ = simplifile.delete(path <> "-wal")
  let _ = simplifile.delete(path <> "-shm")
  Nil
}
