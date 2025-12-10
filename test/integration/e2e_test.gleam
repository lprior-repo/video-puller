/// End-to-End Integration Test
///
/// Tests the complete happy path from job creation to completion.
/// I-001: End-to-End Test (Happy Path)
import domain/types
import gleam/list
import gleam/option.{None, Some}
import gleeunit
import gleeunit/should
import infra/db
import infra/migrator
import infra/repo
import simplifile

pub fn main() {
  gleeunit.main()
}

/// Test the complete happy path workflow:
/// 1. Create a job (pending)
/// 2. List jobs and verify
/// 3. Update to downloading
/// 4. Update to completed
/// 5. Verify final state
pub fn happy_path_workflow_test() {
  let test_db = "/tmp/test_e2e_happy_path.db"
  let _ = simplifile.delete(test_db)

  // Initialize database
  let conn = db.init_db(test_db) |> should.be_ok()

  // Run migrations
  migrator.run_migrations(conn) |> should.be_ok()

  // 1. Create a job
  let job_id = types.new_job_id("test-job-001")
  let url = "https://www.youtube.com/watch?v=dQw4w9WgXcQ"
  let timestamp = 1_000_000

  repo.insert_job(conn, job_id, url, timestamp) |> should.be_ok()

  // 2. List jobs and verify the job is there
  let jobs = repo.list_jobs(conn, None) |> should.be_ok()
  list.length(jobs) |> should.equal(1)

  let job = list.first(jobs) |> should.be_ok()
  types.job_id_to_string(job.id) |> should.equal("test-job-001")
  job.url |> should.equal(url)
  job.status |> should.equal(types.Pending)

  // 3. Update to downloading with progress
  repo.update_status(conn, job_id, types.Downloading(50), timestamp + 100)
  |> should.be_ok()

  let jobs_downloading =
    repo.list_jobs(conn, Some("downloading")) |> should.be_ok()
  list.length(jobs_downloading) |> should.equal(1)

  let downloading_job = list.first(jobs_downloading) |> should.be_ok()
  case downloading_job.status {
    types.Downloading(progress) -> progress |> should.equal(50)
    _ -> should.fail()
  }

  // 4. Update to completed
  repo.update_status(conn, job_id, types.Completed, timestamp + 200)
  |> should.be_ok()

  // Set the output path
  repo.update_path(conn, job_id, "/downloads/video.mp4", timestamp + 200)
  |> should.be_ok()

  // 5. Verify final state
  let jobs_completed = repo.list_jobs(conn, Some("completed")) |> should.be_ok()
  list.length(jobs_completed) |> should.equal(1)

  let completed_job = list.first(jobs_completed) |> should.be_ok()
  completed_job.status |> should.equal(types.Completed)
  completed_job.path |> should.equal(Some("/downloads/video.mp4"))

  // Cleanup
  let _ = db.close(conn)
  let _ = simplifile.delete(test_db)
}

/// Test multiple jobs in the queue
pub fn multiple_jobs_test() {
  let test_db = "/tmp/test_e2e_multiple_jobs.db"
  let _ = simplifile.delete(test_db)

  let conn = db.init_db(test_db) |> should.be_ok()
  migrator.run_migrations(conn) |> should.be_ok()

  // Create multiple jobs
  let job1 = types.new_job_id("job-001")
  let job2 = types.new_job_id("job-002")
  let job3 = types.new_job_id("job-003")

  repo.insert_job(conn, job1, "https://youtube.com/1", 1000) |> should.be_ok()
  repo.insert_job(conn, job2, "https://youtube.com/2", 1001) |> should.be_ok()
  repo.insert_job(conn, job3, "https://youtube.com/3", 1002) |> should.be_ok()

  // Verify all jobs are pending
  let pending_jobs = repo.list_jobs(conn, Some("pending")) |> should.be_ok()
  list.length(pending_jobs) |> should.equal(3)

  // Complete one job
  repo.update_status(conn, job1, types.Completed, 2000) |> should.be_ok()

  // Fail one job
  repo.update_status(conn, job2, types.Failed("Network error"), 2000)
  |> should.be_ok()

  // Verify counts
  repo.list_jobs(conn, Some("pending"))
  |> should.be_ok()
  |> list.length()
  |> should.equal(1)
  repo.list_jobs(conn, Some("completed"))
  |> should.be_ok()
  |> list.length()
  |> should.equal(1)
  repo.list_jobs(conn, Some("failed"))
  |> should.be_ok()
  |> list.length()
  |> should.equal(1)

  // Total should still be 3
  repo.list_jobs(conn, None)
  |> should.be_ok()
  |> list.length()
  |> should.equal(3)

  // Cleanup
  let _ = db.close(conn)
  let _ = simplifile.delete(test_db)
}

/// Test zombie job recovery (jobs stuck in downloading state)
pub fn zombie_recovery_test() {
  let test_db = "/tmp/test_e2e_zombies.db"
  let _ = simplifile.delete(test_db)

  let conn = db.init_db(test_db) |> should.be_ok()
  migrator.run_migrations(conn) |> should.be_ok()

  // Create jobs in different states
  let job1 = types.new_job_id("zombie-001")
  let job2 = types.new_job_id("zombie-002")
  let job3 = types.new_job_id("normal-001")

  repo.insert_job(conn, job1, "https://youtube.com/1", 1000) |> should.be_ok()
  repo.insert_job(conn, job2, "https://youtube.com/2", 1000) |> should.be_ok()
  repo.insert_job(conn, job3, "https://youtube.com/3", 1000) |> should.be_ok()

  // Set two jobs to "downloading" (simulating crash mid-download)
  repo.update_status(conn, job1, types.Downloading(30), 1100) |> should.be_ok()
  repo.update_status(conn, job2, types.Downloading(75), 1100) |> should.be_ok()
  // job3 stays pending

  // Verify we have 2 downloading jobs
  repo.list_jobs(conn, Some("downloading"))
  |> should.be_ok()
  |> list.length()
  |> should.equal(2)

  // Run zombie recovery (simulates startup sequence)
  let reset_count = repo.reset_zombies(conn, 2000) |> should.be_ok()
  reset_count |> should.equal(2)

  // Verify all jobs are now pending
  repo.list_jobs(conn, Some("downloading"))
  |> should.be_ok()
  |> list.length()
  |> should.equal(0)

  repo.list_jobs(conn, Some("pending"))
  |> should.be_ok()
  |> list.length()
  |> should.equal(3)

  // Cleanup
  let _ = db.close(conn)
  let _ = simplifile.delete(test_db)
}

/// Test idempotency of migrations and operations
pub fn idempotency_test() {
  let test_db = "/tmp/test_e2e_idempotency.db"
  let _ = simplifile.delete(test_db)

  let conn = db.init_db(test_db) |> should.be_ok()

  // Run migrations multiple times
  migrator.run_migrations(conn) |> should.be_ok()
  migrator.run_migrations(conn) |> should.be_ok()
  migrator.run_migrations(conn) |> should.be_ok()

  // Create a job
  let job_id = types.new_job_id("idem-001")
  repo.insert_job(conn, job_id, "https://test.com", 1000) |> should.be_ok()

  // Update status multiple times to same value
  repo.update_status(conn, job_id, types.Downloading(50), 1100)
  |> should.be_ok()
  repo.update_status(conn, job_id, types.Downloading(50), 1101)
  |> should.be_ok()

  // Verify only one job exists
  repo.list_jobs(conn, None)
  |> should.be_ok()
  |> list.length()
  |> should.equal(1)

  // Cleanup
  let _ = db.close(conn)
  let _ = simplifile.delete(test_db)
}
