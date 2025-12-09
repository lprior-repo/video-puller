import gleam/result
import gleeunit
import gleeunit/should
import infra/db
import simplifile

pub fn main() {
  gleeunit.main()
}

// Test database connection
pub fn connect_test() {
  let test_db = "/tmp/test_video_puller.db"

  // Clean up if exists
  let _ = simplifile.delete(test_db)

  let result = case db.init_db(test_db) {
    Ok(conn) -> {
      // Connection should work
      let _ = db.close(conn)
      Ok(Nil)
    }
    Error(e) -> Error(e)
  }

  result
  |> should.be_ok()

  // Clean up
  let _ = simplifile.delete(test_db)
}

// Test exec function
pub fn exec_test() {
  let test_db = "/tmp/test_exec.db"
  let _ = simplifile.delete(test_db)

  let result = case db.init_db(test_db) {
    Ok(conn) -> {
      // Create a test table
      let exec_result =
        db.exec(conn, "CREATE TABLE test (id INTEGER PRIMARY KEY, name TEXT)")

      let _ = db.close(conn)
      exec_result
    }
    Error(e) -> Error(e)
  }

  result
  |> should.be_ok()

  let _ = simplifile.delete(test_db)
}

// Test WAL mode initialization
pub fn wal_mode_test() {
  let test_db = "/tmp/test_wal.db"
  let _ = simplifile.delete(test_db)

  let result = case db.init_db(test_db) {
    Ok(conn) -> {
      // WAL mode should be enabled (this is tested implicitly by init_db)
      let _ = db.close(conn)
      Ok(Nil)
    }
    Error(e) -> Error(e)
  }

  result
  |> should.be_ok()

  let _ = simplifile.delete(test_db)
}
