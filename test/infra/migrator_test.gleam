import gleeunit
import gleeunit/should
import infra/db
import infra/migrator
import simplifile

pub fn main() {
  gleeunit.main()
}

// Test migration runner
pub fn run_migrations_test() {
  let test_db = "/tmp/test_migrations.db"
  let _ = simplifile.delete(test_db)

  let result = case db.init_db(test_db) {
    Ok(conn) -> {
      // Run migrations
      let mig_result = migrator.run_migrations(conn)

      let _ = db.close(conn)
      mig_result
    }
    Error(e) -> Error(e)
  }

  result
  |> should.be_ok()

  let _ = simplifile.delete(test_db)
}

// Test idempotency - running migrations twice should work
pub fn run_migrations_idempotent_test() {
  let test_db = "/tmp/test_migrations_idempotent.db"
  let _ = simplifile.delete(test_db)

  let result = case db.init_db(test_db) {
    Ok(conn) -> {
      // Run migrations first time
      let _ = migrator.run_migrations(conn)

      // Run migrations second time - should not error
      let second_result = migrator.run_migrations(conn)

      let _ = db.close(conn)
      second_result
    }
    Error(e) -> Error(e)
  }

  result
  |> should.be_ok()

  let _ = simplifile.delete(test_db)
}
