/// Stress Test - Queue Fill
///
/// I-002: Tests system behavior under high load conditions.
/// Fills the queue with many jobs to verify:
/// - Database handles large numbers of records
/// - List operations remain performant
/// - Status transitions work correctly at scale
/// - Memory usage stays reasonable
import domain/core_types
import gleam/int
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

/// Test filling the queue with 100 jobs
pub fn queue_fill_100_jobs_test() {
  let test_db = "/tmp/test_stress_100.db"
  let _ = simplifile.delete(test_db)

  let conn = db.init_db(test_db) |> should.be_ok()
  migrator.run_migrations(conn) |> should.be_ok()

  // Create 100 jobs
  let job_count = 100
  let base_timestamp = 1_000_000

  list.range(1, job_count)
  |> list.each(fn(i) {
    let job_id = core_types.new_job_id("stress-" <> int.to_string(i))
    let url = "https://youtube.com/watch?v=video" <> int.to_string(i)
    repo.insert_job(conn, job_id, url, base_timestamp + i) |> should.be_ok()
  })

  // Verify all jobs were created
  let all_jobs = repo.list_jobs(conn, None) |> should.be_ok()
  list.length(all_jobs) |> should.equal(job_count)

  // All should be pending
  let pending_jobs = repo.list_jobs(conn, Some("pending")) |> should.be_ok()
  list.length(pending_jobs) |> should.equal(job_count)

  // Cleanup
  let _ = db.close(conn)
  let _ = simplifile.delete(test_db)
}

/// Test filling the queue with 500 jobs (larger scale)
pub fn queue_fill_500_jobs_test() {
  let test_db = "/tmp/test_stress_500.db"
  let _ = simplifile.delete(test_db)

  let conn = db.init_db(test_db) |> should.be_ok()
  migrator.run_migrations(conn) |> should.be_ok()

  // Create 500 jobs
  let job_count = 500
  let base_timestamp = 1_000_000

  list.range(1, job_count)
  |> list.each(fn(i) {
    let job_id = core_types.new_job_id("bulk-" <> int.to_string(i))
    let url = "https://vimeo.com/" <> int.to_string(i)
    repo.insert_job(conn, job_id, url, base_timestamp + i) |> should.be_ok()
  })

  // Verify count
  let all_jobs = repo.list_jobs(conn, None) |> should.be_ok()
  list.length(all_jobs) |> should.equal(job_count)

  // Cleanup
  let _ = db.close(conn)
  let _ = simplifile.delete(test_db)
}

/// Test mixed status transitions at scale
pub fn mixed_status_transitions_test() {
  let test_db = "/tmp/test_stress_transitions.db"
  let _ = simplifile.delete(test_db)

  let conn = db.init_db(test_db) |> should.be_ok()
  migrator.run_migrations(conn) |> should.be_ok()

  // Create 50 jobs
  let job_count = 50
  let base_timestamp = 1_000_000

  // Insert all jobs
  list.range(1, job_count)
  |> list.each(fn(i) {
    let job_id = core_types.new_job_id("trans-" <> int.to_string(i))
    let url = "https://test.com/" <> int.to_string(i)
    repo.insert_job(conn, job_id, url, base_timestamp) |> should.be_ok()
  })

  // Transition jobs to different states:
  // - Jobs 1-15: Completed
  // - Jobs 16-30: Failed
  // - Jobs 31-40: Downloading
  // - Jobs 41-50: Pending (unchanged)

  // Complete jobs 1-15
  list.range(1, 15)
  |> list.each(fn(i) {
    let job_id = core_types.new_job_id("trans-" <> int.to_string(i))
    repo.update_status(
      conn,
      job_id,
      core_types.Completed,
      base_timestamp + 1000,
    )
    |> should.be_ok()
  })

  // Fail jobs 16-30
  list.range(16, 30)
  |> list.each(fn(i) {
    let job_id = core_types.new_job_id("trans-" <> int.to_string(i))
    repo.update_status(
      conn,
      job_id,
      core_types.Failed("Test failure " <> int.to_string(i)),
      base_timestamp + 1000,
    )
    |> should.be_ok()
  })

  // Set jobs 31-40 to downloading with varying progress
  list.range(31, 40)
  |> list.each(fn(i) {
    let job_id = core_types.new_job_id("trans-" <> int.to_string(i))
    let progress = { i - 30 } * 10
    // 10%, 20%, ... 100%
    repo.update_status(
      conn,
      job_id,
      core_types.Downloading(progress),
      base_timestamp + 1000,
    )
    |> should.be_ok()
  })

  // Verify counts by status
  repo.list_jobs(conn, Some("completed"))
  |> should.be_ok()
  |> list.length()
  |> should.equal(15)

  repo.list_jobs(conn, Some("failed"))
  |> should.be_ok()
  |> list.length()
  |> should.equal(15)

  repo.list_jobs(conn, Some("downloading"))
  |> should.be_ok()
  |> list.length()
  |> should.equal(10)

  repo.list_jobs(conn, Some("pending"))
  |> should.be_ok()
  |> list.length()
  |> should.equal(10)

  // Total should still be 50
  repo.list_jobs(conn, None)
  |> should.be_ok()
  |> list.length()
  |> should.equal(job_count)

  // Cleanup
  let _ = db.close(conn)
  let _ = simplifile.delete(test_db)
}

/// Test zombie reset at scale
pub fn zombie_reset_at_scale_test() {
  let test_db = "/tmp/test_stress_zombies.db"
  let _ = simplifile.delete(test_db)

  let conn = db.init_db(test_db) |> should.be_ok()
  migrator.run_migrations(conn) |> should.be_ok()

  // Create 30 jobs, all in downloading state (simulating crash)
  let zombie_count = 30
  let base_timestamp = 1_000_000

  list.range(1, zombie_count)
  |> list.each(fn(i) {
    let job_id = core_types.new_job_id("zombie-" <> int.to_string(i))
    let url = "https://zombie.test/" <> int.to_string(i)
    repo.insert_job(conn, job_id, url, base_timestamp) |> should.be_ok()

    // Set to downloading with random progress
    let progress = { i * 3 } % 100
    repo.update_status(
      conn,
      job_id,
      core_types.Downloading(progress),
      base_timestamp + 100,
    )
    |> should.be_ok()
  })

  // Verify all are downloading
  repo.list_jobs(conn, Some("downloading"))
  |> should.be_ok()
  |> list.length()
  |> should.equal(zombie_count)

  // Reset zombies
  let reset = repo.reset_zombies(conn, base_timestamp + 1000) |> should.be_ok()
  reset |> should.equal(zombie_count)

  // All should now be pending
  repo.list_jobs(conn, Some("pending"))
  |> should.be_ok()
  |> list.length()
  |> should.equal(zombie_count)

  repo.list_jobs(conn, Some("downloading"))
  |> should.be_ok()
  |> list.length()
  |> should.equal(0)

  // Cleanup
  let _ = db.close(conn)
  let _ = simplifile.delete(test_db)
}

/// Test rapid sequential status updates
pub fn rapid_status_updates_test() {
  let test_db = "/tmp/test_stress_rapid.db"
  let _ = simplifile.delete(test_db)

  let conn = db.init_db(test_db) |> should.be_ok()
  migrator.run_migrations(conn) |> should.be_ok()

  // Create one job and rapidly update its status
  let job_id = core_types.new_job_id("rapid-001")
  repo.insert_job(conn, job_id, "https://rapid.test/video", 1_000_000)
  |> should.be_ok()

  // Simulate download progress updates (0% to 100% in steps of 1%)
  list.range(0, 100)
  |> list.each(fn(progress) {
    repo.update_status(
      conn,
      job_id,
      core_types.Downloading(progress),
      1_000_000 + progress,
    )
    |> should.be_ok()
  })

  // Complete the job
  repo.update_status(conn, job_id, core_types.Completed, 1_000_101)
  |> should.be_ok()

  // Verify final state
  let jobs = repo.list_jobs(conn, Some("completed")) |> should.be_ok()
  list.length(jobs) |> should.equal(1)

  let job = list.first(jobs) |> should.be_ok()
  job.status |> should.equal(core_types.Completed)

  // Cleanup
  let _ = db.close(conn)
  let _ = simplifile.delete(test_db)
}

/// Test metadata updates at scale
pub fn metadata_updates_at_scale_test() {
  let test_db = "/tmp/test_stress_metadata.db"
  let _ = simplifile.delete(test_db)

  let conn = db.init_db(test_db) |> should.be_ok()
  migrator.run_migrations(conn) |> should.be_ok()

  // Create 25 jobs with metadata
  let job_count = 25
  let base_timestamp = 1_000_000

  list.range(1, job_count)
  |> list.each(fn(i) {
    let job_id = core_types.new_job_id("meta-" <> int.to_string(i))
    let url = "https://meta.test/" <> int.to_string(i)
    repo.insert_job(conn, job_id, url, base_timestamp) |> should.be_ok()

    // Add metadata
    repo.update_metadata(
      conn,
      job_id,
      Some("Video Title " <> int.to_string(i)),
      Some("https://thumb.test/" <> int.to_string(i) <> ".jpg"),
      Some(i * 60),
      // Duration in seconds
      base_timestamp + 100,
    )
    |> should.be_ok()
  })

  // Verify all jobs have metadata
  let jobs = repo.list_jobs(conn, None) |> should.be_ok()
  list.length(jobs) |> should.equal(job_count)

  // Check first job has metadata
  let first_job = list.first(jobs) |> should.be_ok()
  first_job.title |> should.be_some()
  first_job.thumbnail_url |> should.be_some()
  first_job.duration_seconds |> should.be_some()

  // Cleanup
  let _ = db.close(conn)
  let _ = simplifile.delete(test_db)
}
