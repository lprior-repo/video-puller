import domain/types
import engine/ytdlp
import gleam/list
import gleam/option.{None, Some}
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

// Test URL validation
pub fn valid_url_test() {
  let config = ytdlp.default_config()
  let job_id = types.new_job_id("test-1")

  ytdlp.build_download_args("https://example.com/video", job_id, config, None)
  |> should.be_ok()
}

pub fn invalid_url_empty_test() {
  let config = ytdlp.default_config()
  let job_id = types.new_job_id("test-1")

  ytdlp.build_download_args("", job_id, config, None)
  |> should.be_error()
}

pub fn invalid_url_no_scheme_test() {
  let config = ytdlp.default_config()
  let job_id = types.new_job_id("test-1")

  ytdlp.build_download_args("example.com/video", job_id, config, None)
  |> should.be_error()
}

pub fn invalid_url_bad_scheme_test() {
  let config = ytdlp.default_config()
  let job_id = types.new_job_id("test-1")

  ytdlp.build_download_args("ftp://example.com/video", job_id, config, None)
  |> should.be_error()
}

// Test command building
pub fn build_download_args_test() {
  let config =
    ytdlp.DownloadConfig(
      output_directory: "/tmp/downloads",
      format: "best",
      max_filesize: "1G",
      audio_only: False,
      audio_format: ytdlp.BestAudio,
      allow_playlist: False,
    )
  let job_id = types.new_job_id("test-123")

  case
    ytdlp.build_download_args(
      "https://youtube.com/watch?v=test",
      job_id,
      config,
      None,
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

// Test format selection
pub fn build_download_args_with_format_test() {
  let config = ytdlp.default_config()
  let job_id = types.new_job_id("test-123")

  case
    ytdlp.build_download_args(
      "https://youtube.com/watch?v=test",
      job_id,
      config,
      Some("247"),
    )
  {
    Ok(args) -> {
      // Should contain the format flag
      args
      |> list.contains("--format")
      |> should.be_true()

      // Should contain the custom format code
      args
      |> list.contains("247")
      |> should.be_true()
    }
    Error(_) -> should.fail()
  }
}

// Test list formats args
pub fn build_list_formats_args_test() {
  ytdlp.build_list_formats_args("https://youtube.com/watch?v=test")
  |> should.be_ok()
}

pub fn build_list_formats_args_invalid_url_test() {
  ytdlp.build_list_formats_args("")
  |> should.be_error()
}

// Test audio-only download configuration
pub fn audio_config_mp3_test() {
  let config = ytdlp.audio_config(ytdlp.MP3)

  config.audio_only
  |> should.be_true()

  config.audio_format
  |> should.equal(ytdlp.MP3)
}

pub fn audio_config_args_test() {
  let config = ytdlp.audio_config(ytdlp.MP3)
  let job_id = types.new_job_id("audio-test")

  case
    ytdlp.build_download_args(
      "https://youtube.com/watch?v=test",
      job_id,
      config,
      None,
    )
  {
    Ok(args) -> {
      // Should contain extract-audio flag
      args
      |> list.contains("--extract-audio")
      |> should.be_true()

      // Should contain audio-format flag
      args
      |> list.contains("--audio-format")
      |> should.be_true()

      // Should contain mp3 format
      args
      |> list.contains("mp3")
      |> should.be_true()
    }
    Error(_) -> should.fail()
  }
}

// Test playlist support
pub fn playlist_config_test() {
  let config =
    ytdlp.DownloadConfig(
      output_directory: "./downloads",
      format: "best",
      max_filesize: "2G",
      audio_only: False,
      audio_format: ytdlp.BestAudio,
      allow_playlist: True,
    )
  let job_id = types.new_job_id("playlist-test")

  case
    ytdlp.build_download_args(
      "https://youtube.com/playlist?list=PLtest",
      job_id,
      config,
      None,
    )
  {
    Ok(args) -> {
      // Should contain yes-playlist flag
      args
      |> list.contains("--yes-playlist")
      |> should.be_true()

      // Should NOT contain no-playlist flag
      args
      |> list.contains("--no-playlist")
      |> should.be_false()
    }
    Error(_) -> should.fail()
  }
}

pub fn build_playlist_info_args_test() {
  case
    ytdlp.build_playlist_info_args("https://youtube.com/playlist?list=PLtest")
  {
    Ok(args) -> {
      args
      |> list.contains("--flat-playlist")
      |> should.be_true()

      args
      |> list.contains("--yes-playlist")
      |> should.be_true()
    }
    Error(_) -> should.fail()
  }
}

// Test audio format conversion
pub fn audio_format_to_string_test() {
  ytdlp.audio_format_to_string(ytdlp.MP3)
  |> should.equal("mp3")

  ytdlp.audio_format_to_string(ytdlp.AAC)
  |> should.equal("aac")

  ytdlp.audio_format_to_string(ytdlp.OPUS)
  |> should.equal("opus")

  ytdlp.audio_format_to_string(ytdlp.BestAudio)
  |> should.equal("best")
}

pub fn string_to_audio_format_test() {
  ytdlp.string_to_audio_format("mp3")
  |> should.equal(ytdlp.MP3)

  ytdlp.string_to_audio_format("aac")
  |> should.equal(ytdlp.AAC)

  ytdlp.string_to_audio_format("opus")
  |> should.equal(ytdlp.OPUS)

  ytdlp.string_to_audio_format("unknown")
  |> should.equal(ytdlp.BestAudio)
}
