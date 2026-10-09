/// Regression tests for manual yt-dlp command construction.
import domain/core_types
import engine/ytdlp
import gleam/list
import gleam/option.{None}
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

fn test_config(use_channel_folders: Bool) -> ytdlp.DownloadConfig {
  ytdlp.DownloadConfig(
    output_directory: "/tmp/video-puller-tests",
    format: "best",
    max_filesize: "2G",
    audio_only: False,
    audio_format: ytdlp.BestAudio,
    allow_playlist: False,
    download_timeout_ms: 60_000,
    rate_limit_delay_ms: 0,
    bandwidth_limit: "",
    use_channel_folders: use_channel_folders,
  )
}

pub fn command_reports_final_media_path_test() {
  let args =
    ytdlp.build_download_args(
      "https://example.com/video",
      core_types.new_job_id("job-123"),
      test_config(False),
      None,
    )
    |> should.be_ok()

  list.contains(args, "--print") |> should.be_true()
  list.contains(args, "after_move:filepath") |> should.be_true()
}

pub fn command_keeps_title_channel_date_template_test() {
  let args =
    ytdlp.build_download_args(
      "https://example.com/video",
      core_types.new_job_id("job-123"),
      test_config(True),
      None,
    )
    |> should.be_ok()

  list.contains(
    args,
    "/tmp/video-puller-tests/%(channel)s/%(title)s (%(upload_date>%Y-%m-%d)s).%(ext)s",
  )
  |> should.be_true()
}

/// The default "./downloads" must land inside the writable data root
pub fn resolve_output_directory_absolutizes_relative_paths_test() {
  let config = test_config(False)
  let relative =
    ytdlp.resolve_output_directory(
      ytdlp.DownloadConfig(..config, output_directory: "./downloads"),
      "/var/lib/video-puller",
    )

  relative.output_directory
  |> should.equal("/var/lib/video-puller/downloads")

  let bare =
    ytdlp.resolve_output_directory(
      ytdlp.DownloadConfig(..config, output_directory: "mine"),
      "/var/lib/video-puller",
    )
  bare.output_directory |> should.equal("/var/lib/video-puller/mine")

  let absolute = ytdlp.resolve_output_directory(config, "/var/lib/video-puller")
  absolute.output_directory |> should.equal("/tmp/video-puller-tests")
}
