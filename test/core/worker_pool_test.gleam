/// Regression tests for immutable per-job worker settings snapshots.
import core/worker_pool
import domain/core_types
import engine/ytdlp
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

fn config(
  output_directory: String,
  audio_only: Bool,
  format: String,
) -> ytdlp.DownloadConfig {
  ytdlp.DownloadConfig(
    output_directory: output_directory,
    format: format,
    max_filesize: "2G",
    audio_only: audio_only,
    audio_format: ytdlp.MP3,
    allow_playlist: False,
    download_timeout_ms: 60_000,
    rate_limit_delay_ms: 0,
    bandwidth_limit: "",
    use_channel_folders: False,
  )
}

pub fn pending_work_carries_submission_config_test() {
  let second = config("/tmp/second", True, "worst")
  let work =
    worker_pool.PendingWork(
      job_id: core_types.new_job_id("job-1"),
      url: "https://example.com/video",
      config: second,
      queued_at: 0,
    )

  work.config.output_directory |> should.equal(second.output_directory)
  work.config.audio_only |> should.equal(second.audio_only)
  work.config.format |> should.equal(second.format)
  work.config.output_directory |> should.equal("/tmp/second")
}
