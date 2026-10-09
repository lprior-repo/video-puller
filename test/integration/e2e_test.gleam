/// End-to-End Integration Test
///
/// Tests the complete happy path from job creation to completion.
/// I-001: End-to-End Test (Happy Path)
import domain/core_types
import domain/subscription_types
import engine/ytdl_sub
import gleam/list
import gleam/option.{None, Some}
import gleeunit
import gleeunit/should
import infra/db
import infra/migrator
import infra/repo
import infra/subscription_repo
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
  let job_id = core_types.new_job_id("test-job-001")
  let url = "https://www.youtube.com/watch?v=dQw4w9WgXcQ"
  let timestamp = 1_000_000

  repo.insert_job(conn, job_id, url, timestamp) |> should.be_ok()

  // 2. List jobs and verify the job is there
  let jobs = repo.list_jobs(conn, None) |> should.be_ok()
  list.length(jobs) |> should.equal(1)

  let job = list.first(jobs) |> should.be_ok()
  core_types.job_id_to_string(job.id) |> should.equal("test-job-001")
  job.url |> should.equal(url)
  job.status |> should.equal(core_types.Pending)

  // 3. Update to downloading with progress
  repo.update_status(conn, job_id, core_types.Downloading(50), timestamp + 100)
  |> should.be_ok()

  let jobs_downloading =
    repo.list_jobs(conn, Some("downloading")) |> should.be_ok()
  list.length(jobs_downloading) |> should.equal(1)

  let downloading_job = list.first(jobs_downloading) |> should.be_ok()
  case downloading_job.status {
    core_types.Downloading(progress) -> progress |> should.equal(50)
    _ -> should.fail()
  }

  // 4. Update to completed
  repo.update_status(conn, job_id, core_types.Completed, timestamp + 200)
  |> should.be_ok()

  // Set the output path
  repo.update_path(conn, job_id, "/downloads/video.mp4", timestamp + 200)
  |> should.be_ok()

  // 5. Verify final state
  let jobs_completed = repo.list_jobs(conn, Some("completed")) |> should.be_ok()
  list.length(jobs_completed) |> should.equal(1)

  let completed_job = list.first(jobs_completed) |> should.be_ok()
  completed_job.status |> should.equal(core_types.Completed)
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
  let job1 = core_types.new_job_id("job-001")
  let job2 = core_types.new_job_id("job-002")
  let job3 = core_types.new_job_id("job-003")

  repo.insert_job(conn, job1, "https://youtube.com/1", 1000) |> should.be_ok()
  repo.insert_job(conn, job2, "https://youtube.com/2", 1001) |> should.be_ok()
  repo.insert_job(conn, job3, "https://youtube.com/3", 1002) |> should.be_ok()

  // Verify all jobs are pending
  let pending_jobs = repo.list_jobs(conn, Some("pending")) |> should.be_ok()
  list.length(pending_jobs) |> should.equal(3)

  // Complete one job
  repo.update_status(conn, job1, core_types.Completed, 2000) |> should.be_ok()

  // Fail one job
  repo.update_status(conn, job2, core_types.Failed("Network error"), 2000)
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
  let job1 = core_types.new_job_id("zombie-001")
  let job2 = core_types.new_job_id("zombie-002")
  let job3 = core_types.new_job_id("normal-001")

  repo.insert_job(conn, job1, "https://youtube.com/1", 1000) |> should.be_ok()
  repo.insert_job(conn, job2, "https://youtube.com/2", 1000) |> should.be_ok()
  repo.insert_job(conn, job3, "https://youtube.com/3", 1000) |> should.be_ok()

  // Set two jobs to "downloading" (simulating crash mid-download)
  repo.update_status(conn, job1, core_types.Downloading(30), 1100)
  |> should.be_ok()
  repo.update_status(conn, job2, core_types.Downloading(75), 1100)
  |> should.be_ok()
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
  let job_id = core_types.new_job_id("idem-001")
  repo.insert_job(conn, job_id, "https://test.com", 1000) |> should.be_ok()

  // Update status multiple times to same value
  repo.update_status(conn, job_id, core_types.Downloading(50), 1100)
  |> should.be_ok()
  repo.update_status(conn, job_id, core_types.Downloading(50), 1101)
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

/// Test E2E download flow with mocked yt-dlp output
///
/// Tests the complete download workflow from job creation to completion
/// with simulated yt-dlp progress output. This test verifies:
/// 1. Job creation in pending state
/// 2. Progress updates via mocked yt-dlp output parsing
/// 3. Successful completion with file path
/// 4. Database state consistency throughout the flow
pub fn full_download_flow_with_mocked_ytdlp_test() {
  let test_db = "/tmp/test_e2e_full_download.db"
  let _ = simplifile.delete(test_db)

  let conn = db.init_db(test_db) |> should.be_ok()
  migrator.run_migrations(conn) |> should.be_ok()

  // 1. Create a job (simulating user submitting a download)
  let job_id = core_types.new_job_id("test-download-001")
  let url = "https://www.youtube.com/watch?v=dQw4w9WgXcQ"
  let timestamp = 1_000_000

  repo.insert_job(conn, job_id, url, timestamp) |> should.be_ok()

  // Verify job is pending
  let job = repo.get_job(conn, job_id) |> should.be_ok() |> should.be_some()
  job.status |> should.equal(core_types.Pending)

  // 2. Simulate download starting (worker picks up job)
  repo.update_status(conn, job_id, core_types.Downloading(0), timestamp + 10)
  |> should.be_ok()

  let job_downloading =
    repo.get_job(conn, job_id) |> should.be_ok() |> should.be_some()
  case job_downloading.status {
    core_types.Downloading(progress) -> progress |> should.equal(0)
    _ -> should.fail()
  }

  // 3. Simulate progress updates (parsing mocked yt-dlp output)
  // These would come from parser.parse_progress() in real flow
  let progress_updates = [
    #(10, "[download]  10.5% of  100.00MiB at  1.23MiB/s ETA 01:15"),
    #(25, "[download]  25.3% of  100.00MiB at  1.45MiB/s ETA 00:52"),
    #(50, "[download]  50.8% of  100.00MiB at  1.67MiB/s ETA 00:30"),
    #(75, "[download]  75.2% of  100.00MiB at  1.89MiB/s ETA 00:13"),
    #(95, "[download]  95.7% of  100.00MiB at  2.01MiB/s ETA 00:02"),
  ]

  list.each(progress_updates, fn(update) {
    let #(progress, _line) = update
    repo.update_status(
      conn,
      job_id,
      core_types.Downloading(progress),
      timestamp + 100,
    )
    |> should.be_ok()

    // Verify progress was updated
    let job_progress =
      repo.get_job(conn, job_id) |> should.be_ok() |> should.be_some()
    case job_progress.status {
      core_types.Downloading(p) -> p |> should.equal(progress)
      _ -> should.fail()
    }
  })

  // 4. Simulate download completion
  repo.update_status(conn, job_id, core_types.Completed, timestamp + 200)
  |> should.be_ok()

  // Set output path (simulating file system result)
  let output_path = "/downloads/test-download-001.mp4"
  repo.update_path(conn, job_id, output_path, timestamp + 200)
  |> should.be_ok()

  // 5. Verify final state
  let completed_job =
    repo.get_job(conn, job_id) |> should.be_ok() |> should.be_some()
  completed_job.status |> should.equal(core_types.Completed)
  completed_job.path |> should.equal(Some(output_path))

  // Verify job appears in completed list
  let completed_jobs = repo.list_jobs(conn, Some("completed")) |> should.be_ok()
  list.length(completed_jobs) |> should.equal(1)

  // Verify job does not appear in pending or downloading lists
  let pending_jobs = repo.list_jobs(conn, Some("pending")) |> should.be_ok()
  list.length(pending_jobs) |> should.equal(0)

  let downloading_jobs =
    repo.list_jobs(conn, Some("downloading")) |> should.be_ok()
  list.length(downloading_jobs) |> should.equal(0)

  // Cleanup
  let _ = db.close(conn)
  let _ = simplifile.delete(test_db)
}

/// Test E2E subscription pull flow with mocked ytdl-sub output
///
/// Tests the retained subscription workflow with simulated engine output:
/// 1. Subscription configuration setup
/// 2. Engine transaction log parsed into added media files
/// 3. Library records written for each added file
/// 4. Idempotency when the same files are reported again
pub fn subscription_pull_flow_with_mocked_engine_test() {
  let test_db = "/tmp/test_e2e_subscription_pull.db"
  let _ = simplifile.delete(test_db)

  let conn = db.init_db(test_db) |> should.be_ok()
  migrator.run_migrations(conn) |> should.be_ok()

  let timestamp = 1_705_276_800

  // 1. Set up subscription configuration
  let config =
    subscription_types.SubscriptionConfig(
      enabled: True,
      poll_interval_minutes: 360,
      last_poll_at: None,
    )

  subscription_repo.update_config(conn, config, timestamp) |> should.be_ok()

  let loaded_config = subscription_repo.get_config(conn) |> should.be_ok()
  loaded_config.enabled |> should.be_true()
  loaded_config.poll_interval_minutes |> should.equal(360)

  // 2. Mocked ytdl-sub transaction log (media files plus metadata sidecars)
  let engine_output =
    "INFO | ytdl-sub | beginning subscription download\n"
    <> "Files created:\n"
    <> "/tmp/e2e-library/Tech Tutorials\n"
    <> "  s2026.e091101 - Video One.mp4\n"
    <> "  s2026.e091101 - Video One.info.json\n"
    <> "/tmp/e2e-library/Education Hub\n"
    <> "  Video Two.mkv\n"
    <> "  Video Two.jpg\n"
    <> "ERROR | ytdl-sub | one video failed permanently\n"

  let summary = ytdl_sub.parse_pull_output(engine_output)
  summary.downloaded |> should.equal(2)
  list.length(summary.added_files) |> should.equal(2)
  list.length(summary.errors) |> should.equal(1)

  // 3. Process the added files exactly as the manager does
  let library_dir = "/tmp/e2e-library"
  list.each(summary.added_files, fn(path) {
    let video = ytdl_sub.to_discovered_video(library_dir, path)
    subscription_repo.mark_seen(conn, video, True, None, None, timestamp)
    |> should.be_ok()
  })

  // 4. Verify records
  subscription_repo.count_seen_videos(conn)
  |> should.be_ok()
  |> should.equal(2)

  subscription_repo.count_downloaded(conn)
  |> should.be_ok()
  |> should.equal(2)

  let video_one =
    subscription_repo.get_seen_video(
      conn,
      "Tech Tutorials/s2026.e091101 - Video One.mp4",
    )
    |> should.be_ok()
  case video_one {
    Some(v) -> {
      v.channel_name |> should.equal(Some("Tech Tutorials"))
      v.title |> should.equal("Video One")
      v.published_at |> should.equal(Some(1_789_084_800))
      v.downloaded |> should.be_true()
      v.url
      |> should.equal(
        "file:///tmp/e2e-library/Tech Tutorials/s2026.e091101 - Video One.mp4",
      )
    }
    None -> should.fail()
  }

  let video_two =
    subscription_repo.get_seen_video(conn, "Education Hub/Video Two.mkv")
    |> should.be_ok()
  case video_two {
    Some(v) -> {
      v.title |> should.equal("Video Two")
      v.published_at |> should.equal(None)
    }
    None -> should.fail()
  }

  // 5. Second pull reporting the same files must not duplicate records
  list.each(summary.added_files, fn(path) {
    let video = ytdl_sub.to_discovered_video(library_dir, path)
    subscription_repo.mark_seen(conn, video, True, None, None, timestamp + 3600)
    |> should.be_ok()
  })

  subscription_repo.list_seen_videos(conn, 50, 0)
  |> should.be_ok()
  |> list.length()
  |> should.equal(2)

  // 6. Sidecar metadata is never recorded as a video
  subscription_repo.is_seen(
    conn,
    "Tech Tutorials/s2026.e091101 - Video One.info.json",
  )
  |> should.be_ok()
  |> should.be_false()

  // Cleanup
  let _ = db.close(conn)
  let _ = simplifile.delete(test_db)
}
