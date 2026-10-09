/// Video Job Repository Tests
///
/// Integration tests for video job data access layer.
import domain/types
import gleam/option
import gleeunit
import gleeunit/should
import infra/db
import infra/migrator
import infra/repo
import simplifile

pub fn main() {
  gleeunit.main()
}

// ============================================================================
// Test Fixtures
// ============================================================================

fn setup_test_db(name: String) {
  let test_db = "/tmp/test_repo_" <> name <> ".db"
  let _ = simplifile.delete(test_db)

  let conn = db.init_db(test_db) |> should.be_ok()
  migrator.run_migrations(conn) |> should.be_ok()

  #(conn, test_db)
}

fn cleanup(conn, path) {
  let _ = db.close(conn)
  let _ = simplifile.delete(path)
}

// ============================================================================
// Insert Job Tests
// ============================================================================

/// Test insert_job with duplicate job_id returns error
pub fn insert_duplicate_job_id_returns_error_test() {
  let #(conn, path) = setup_test_db("duplicate_job_id")

  let job_id = types.new_job_id("test-job-123")
  let url = "https://www.youtube.com/watch?v=test123"
  let created_at = 1_700_000_000

  // Insert the first job
  repo.insert_job(conn, job_id, url, created_at) |> should.be_ok()

  // Verify the job was inserted
  let first_job = repo.get_job(conn, job_id) |> should.be_ok()

  case first_job {
    option.Some(job) -> {
      job.url |> should.equal(url)
      types.job_id_to_string(job.id) |> should.equal("test-job-123")
    }
    option.None -> should.fail()
  }

  // Try to insert a job with the same job_id but different URL
  let duplicate_url = "https://www.youtube.com/watch?v=different456"
  let duplicate_created_at = 1_700_000_100

  let result =
    repo.insert_job(conn, job_id, duplicate_url, duplicate_created_at)

  // Should return an error due to PRIMARY KEY constraint violation
  result |> should.be_error()

  // Verify the original job was not modified
  let original_job = repo.get_job(conn, job_id) |> should.be_ok()

  case original_job {
    option.Some(job) -> {
      // URL should still be the original one
      job.url |> should.equal(url)
      // Created_at should still be the original one
      job.created_at |> should.equal(created_at)
      // Updated_at should still be the original one (no update happened)
      job.updated_at |> should.equal(created_at)
    }
    option.None -> should.fail()
  }

  cleanup(conn, path)
}
