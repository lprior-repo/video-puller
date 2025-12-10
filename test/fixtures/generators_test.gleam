/// Tests for Type Generators
import domain/types
import fixtures/generators
import gleam/list
import gleam/option.{None, Some}
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

// ============================================================================
// JobId Generator Tests
// ============================================================================

pub fn job_id_test() {
  let id = generators.job_id("my-custom-id")
  types.job_id_to_string(id) |> should.equal("my-custom-id")
}

pub fn job_id_n_test() {
  let id = generators.job_id_n(42)
  types.job_id_to_string(id) |> should.equal("test-job-42")
}

pub fn unique_job_id_test() {
  let id = generators.unique_job_id("download", 123)
  types.job_id_to_string(id) |> should.equal("download-123")
}

// ============================================================================
// VideoStatus Generator Tests
// ============================================================================

pub fn pending_status_test() {
  generators.pending_status() |> should.equal(types.Pending)
}

pub fn downloading_status_test() {
  generators.downloading_status(75) |> should.equal(types.Downloading(75))
}

pub fn completed_status_test() {
  generators.completed_status() |> should.equal(types.Completed)
}

pub fn failed_status_test() {
  generators.failed_status("Network error")
  |> should.equal(types.Failed("Network error"))
}

// ============================================================================
// VideoJob Generator Tests
// ============================================================================

pub fn video_job_test() {
  let job = generators.video_job("test-1", "https://example.com/video")

  types.job_id_to_string(job.id) |> should.equal("test-1")
  job.url |> should.equal("https://example.com/video")
  job.status |> should.equal(types.Pending)
  job.path |> should.equal(None)
}

pub fn pending_job_test() {
  let job = generators.pending_job("p-1", "https://youtube.com/watch?v=abc")

  job.status |> should.equal(types.Pending)
}

pub fn downloading_job_test() {
  let job = generators.downloading_job("d-1", "https://vimeo.com/123", 45)

  case job.status {
    types.Downloading(progress) -> progress |> should.equal(45)
    _ -> should.fail()
  }
}

pub fn completed_job_test() {
  let job =
    generators.completed_job("c-1", "https://url.com", "/downloads/c-1.mp4")

  job.status |> should.equal(types.Completed)
  job.path |> should.equal(Some("/downloads/c-1.mp4"))
}

pub fn failed_job_test() {
  let job = generators.failed_job("f-1", "https://url.com", "404 Not Found")

  case job.status {
    types.Failed(reason) -> reason |> should.equal("404 Not Found")
    _ -> should.fail()
  }
}

// ============================================================================
// DownloadCommand Generator Tests
// ============================================================================

pub fn start_download_cmd_test() {
  let job_id = generators.job_id("cmd-test")
  let cmd = generators.start_download_cmd(job_id, "https://test.com")

  case cmd {
    types.StartDownload(id, url) -> {
      types.job_id_to_string(id) |> should.equal("cmd-test")
      url |> should.equal("https://test.com")
    }
    _ -> should.fail()
  }
}

pub fn cancel_download_cmd_test() {
  let job_id = generators.job_id("cancel-test")
  let cmd = generators.cancel_download_cmd(job_id)

  case cmd {
    types.CancelDownload(id) ->
      types.job_id_to_string(id) |> should.equal("cancel-test")
    _ -> should.fail()
  }
}

// ============================================================================
// DownloadResult Generator Tests
// ============================================================================

pub fn download_started_test() {
  let job_id = generators.job_id("started-1")
  let result = generators.download_started(job_id)

  case result {
    types.DownloadStarted(id) ->
      types.job_id_to_string(id) |> should.equal("started-1")
    _ -> should.fail()
  }
}

pub fn download_progress_test() {
  let job_id = generators.job_id("progress-1")
  let result = generators.download_progress(job_id, 50)

  case result {
    types.DownloadProgress(id, progress) -> {
      types.job_id_to_string(id) |> should.equal("progress-1")
      progress |> should.equal(50)
    }
    _ -> should.fail()
  }
}

pub fn download_complete_test() {
  let job_id = generators.job_id("complete-1")
  let result = generators.download_complete(job_id, "/path/to/video.mp4")

  case result {
    types.DownloadComplete(id, path) -> {
      types.job_id_to_string(id) |> should.equal("complete-1")
      path |> should.equal("/path/to/video.mp4")
    }
    _ -> should.fail()
  }
}

// ============================================================================
// ManagerMessage Generator Tests
// ============================================================================

pub fn poll_jobs_msg_test() {
  generators.poll_jobs_msg() |> should.equal(types.PollJobs)
}

pub fn shutdown_msg_test() {
  generators.shutdown_msg() |> should.equal(types.Shutdown)
}

pub fn status_update_msg_test() {
  let job_id = generators.job_id("update-1")
  let msg = generators.status_update_msg(job_id, types.Completed)

  case msg {
    types.JobStatusUpdate(id, status) -> {
      types.job_id_to_string(id) |> should.equal("update-1")
      status |> should.equal(types.Completed)
    }
    _ -> should.fail()
  }
}

// ============================================================================
// DownloaderConfig Generator Tests
// ============================================================================

pub fn default_config_test() {
  let config = generators.default_config()

  config.max_concurrency |> should.equal(3)
  config.poll_interval_ms |> should.equal(1000)
  config.output_directory |> should.equal("/tmp/downloads")
}

pub fn custom_config_test() {
  let config = generators.downloader_config(5, 500, "/data/videos")

  config.max_concurrency |> should.equal(5)
  config.poll_interval_ms |> should.equal(500)
  config.output_directory |> should.equal("/data/videos")
}

// ============================================================================
// URL Generator Tests
// ============================================================================

pub fn youtube_url_test() {
  generators.youtube_url("dQw4w9WgXcQ")
  |> should.equal("https://www.youtube.com/watch?v=dQw4w9WgXcQ")
}

pub fn vimeo_url_test() {
  generators.vimeo_url("123456")
  |> should.equal("https://vimeo.com/123456")
}

pub fn test_url_test() {
  generators.test_url(42)
  |> should.equal("https://example.com/video/42")
}

// ============================================================================
// Batch Generator Tests
// ============================================================================

pub fn pending_jobs_test() {
  let jobs = generators.pending_jobs(5)

  list.length(jobs) |> should.equal(5)

  // All should be pending
  jobs
  |> list.all(fn(job) { job.status == types.Pending })
  |> should.be_true()
}

pub fn mixed_status_jobs_test() {
  let jobs = generators.mixed_status_jobs(8)

  list.length(jobs) |> should.equal(8)

  // Should have variety of statuses
  let pending_count =
    jobs |> list.filter(fn(j) { j.status == types.Pending }) |> list.length()
  let completed_count =
    jobs |> list.filter(fn(j) { j.status == types.Completed }) |> list.length()

  pending_count |> should.equal(2)
  completed_count |> should.equal(2)
}

// ============================================================================
// Timestamp Generator Tests
// ============================================================================

pub fn timestamp_test() {
  generators.timestamp(0) |> should.equal(generators.base_timestamp)
  generators.timestamp(1000) |> should.equal(generators.base_timestamp + 1000)
}

pub fn timestamps_test() {
  let ts = generators.timestamps(3)

  list.length(ts) |> should.equal(3)
  list.first(ts) |> should.be_ok() |> should.equal(generators.base_timestamp)
}
