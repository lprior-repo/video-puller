import domain/types
import engine/ytdlp
import gleam/list
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

// Test URL validation
pub fn valid_url_test() {
  let config = ytdlp.default_config()
  let job_id = types.new_job_id("test-1")

  ytdlp.build_download_args("https://example.com/video", job_id, config)
  |> should.be_ok()
}

pub fn invalid_url_empty_test() {
  let config = ytdlp.default_config()
  let job_id = types.new_job_id("test-1")

  ytdlp.build_download_args("", job_id, config)
  |> should.be_error()
}

pub fn invalid_url_no_scheme_test() {
  let config = ytdlp.default_config()
  let job_id = types.new_job_id("test-1")

  ytdlp.build_download_args("example.com/video", job_id, config)
  |> should.be_error()
}

pub fn invalid_url_bad_scheme_test() {
  let config = ytdlp.default_config()
  let job_id = types.new_job_id("test-1")

  ytdlp.build_download_args("ftp://example.com/video", job_id, config)
  |> should.be_error()
}

// Test command building
pub fn build_download_args_test() {
  let config =
    ytdlp.DownloadConfig(
      output_directory: "/tmp/downloads",
      format: "best",
      max_filesize: "1G",
    )
  let job_id = types.new_job_id("test-123")

  case
    ytdlp.build_download_args(
      "https://youtube.com/watch?v=test",
      job_id,
      config,
    )
  {
    Ok(args) -> {
      // Should contain the URL as last argument
      args
      |> should.not_equal([])

      // Should contain output flag
      args
      |> list.contains("--output")
      |> should.be_true()
    }
    Error(_) -> should.fail()
  }
}

// Test info args
pub fn build_info_args_test() {
  ytdlp.build_info_args("https://youtube.com/watch?v=test")
  |> should.be_ok()
}

// Test default config
pub fn default_config_test() {
  let config = ytdlp.default_config()

  config.output_directory
  |> should.equal("./downloads")

  config.max_filesize
  |> should.equal("2G")
}
