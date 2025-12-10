import engine/parser
import gleam/list
import gleam/option
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

// Test progress parsing
pub fn parse_progress_success_test() {
  let line = "[download]  45.3% of  100.00MiB at  1.23MiB/s ETA 00:42"

  case parser.parse_progress(line) {
    Ok(progress) -> {
      progress.percentage
      |> should.equal(45)

      progress.message
      |> should.equal(line)
    }
    Error(_) -> should.fail()
  }
}

pub fn parse_progress_100_test() {
  let line = "[download] 100% of 50.00MiB in 00:30"

  case parser.parse_progress(line) {
    Ok(progress) -> {
      progress.percentage
      |> should.equal(100)
    }
    Error(_) -> should.fail()
  }
}

pub fn parse_progress_no_match_test() {
  let line = "Some other output"

  parser.parse_progress(line)
  |> should.be_error()
}

// Test completion detection
pub fn is_complete_100_percent_test() {
  parser.is_complete("[download] 100% of 50.00MiB")
  |> should.be_true()
}

pub fn is_complete_already_downloaded_test() {
  parser.is_complete("[download] video.mp4 has already been downloaded")
  |> should.be_true()
}

pub fn is_complete_false_test() {
  parser.is_complete("[download] 50% of 100.00MiB")
  |> should.be_false()
}

// Test error detection
pub fn is_error_uppercase_test() {
  parser.is_error("ERROR: Video not available")
  |> should.be_true()
}

pub fn is_error_lowercase_test() {
  parser.is_error("error: Connection timeout")
  |> should.be_true()
}

pub fn is_error_failed_test() {
  parser.is_error("Failed to download video")
  |> should.be_true()
}

pub fn is_error_false_test() {
  parser.is_error("[download] 50% of 100.00MiB")
  |> should.be_false()
}

// Test error extraction
pub fn extract_error_test() {
  parser.extract_error("ERROR: Video not available")
  |> should.equal("Video not available")

  parser.extract_error("error: Connection failed")
  |> should.equal("Connection failed")

  parser.extract_error("Some other message")
  |> should.equal("Some other message")
}

// Test format list parsing
pub fn parse_format_list_test() {
  let output =
    "ID EXT   RESOLUTION FPS |   FILESIZE   TBR PROTO | VCODEC        VBR ACODEC      ABR     ASR MORE INFO
--------------------------------------------------------------------------------------------------------------------------
sb2 mhtml  48x27      1   |                           | images                                  storyboard
sb1 mhtml  80x45      1   |                           | images                                  storyboard
139 m4a   audio only      |  488.88KiB  48k https     | audio only        mp4a.40.5   48k 22050Hz low, m4a_dash
249 webm  audio only      |  514.87KiB  50k https     | audio only        opus        50k 48000Hz low, webm_dash
250 webm  audio only      |  685.90KiB  67k https     | audio only        opus        67k 48000Hz low, webm_dash
140 m4a   audio only      | 1.28MiB    129k https     | audio only        mp4a.40.2  129k 44100Hz medium, m4a_dash
251 webm  audio only      | 1.30MiB    131k https     | audio only        opus       131k 48000Hz medium, webm_dash
278 webm  256x144    25   | 1.21MiB    121k https     | vp9          121k video only              144p, webm_dash
160 mp4   256x144    25   | 512.41KiB  50k https      | avc1.4d400c   50k video only              144p, mp4_dash
242 webm  426x240    25   | 2.24MiB    225k https     | vp9          225k video only              240p, webm_dash"

  let formats = parser.parse_format_list(output)

  // Should parse multiple formats
  list.length(formats)
  |> should.equal(10)

  // Check first format (sb2 storyboard comes first in this sample)
  case list.first(formats) {
    Ok(first) -> {
      first.code
      |> should.equal("sb2")

      first.ext
      |> should.equal("mhtml")
    }
    Error(_) -> should.fail()
  }
}

pub fn parse_format_list_single_line_test() {
  let output =
    "247          webm       1280x720    720p  679k , vp9, 30fps, video only"

  let formats = parser.parse_format_list(output)

  list.length(formats)
  |> should.equal(1)

  case list.first(formats) {
    Ok(format) -> {
      format.code
      |> should.equal("247")

      format.ext
      |> should.equal("webm")

      format.resolution
      |> should.equal("1280x720")

      format.note
      |> should.equal("720p  679k , vp9, 30fps, video only")
    }
    Error(_) -> should.fail()
  }
}

pub fn parse_format_list_empty_test() {
  let output = ""

  let formats = parser.parse_format_list(output)

  list.length(formats)
  |> should.equal(0)
}

// Test video metadata parsing
pub fn parse_video_info_success_test() {
  let json =
    "{
    \"title\": \"Test Video Title\",
    \"thumbnail\": \"https://example.com/thumb.jpg\",
    \"duration\": 300,
    \"uploader\": \"Test Channel\",
    \"view_count\": 12345
  }"

  case parser.parse_video_info(json) {
    Ok(metadata) -> {
      metadata.title
      |> should.equal("Test Video Title")

      metadata.thumbnail
      |> should.equal("https://example.com/thumb.jpg")

      metadata.duration
      |> should.equal(300)

      metadata.uploader
      |> should.equal("Test Channel")

      metadata.view_count
      |> should.equal(option.Some(12_345))
    }
    Error(_) -> should.fail()
  }
}

pub fn parse_video_info_null_view_count_test() {
  let json =
    "{
    \"title\": \"Test Video\",
    \"thumbnail\": \"https://example.com/thumb.jpg\",
    \"duration\": 180,
    \"uploader\": \"Channel Name\",
    \"view_count\": null
  }"

  case parser.parse_video_info(json) {
    Ok(metadata) -> {
      metadata.title
      |> should.equal("Test Video")

      // view_count should be None for null
      option.is_none(metadata.view_count)
      |> should.be_true()
    }
    Error(_) -> should.fail()
  }
}

pub fn parse_video_info_missing_view_count_test() {
  let json =
    "{
    \"title\": \"Test Video\",
    \"thumbnail\": \"https://example.com/thumb.jpg\",
    \"duration\": 180,
    \"uploader\": \"Channel Name\"
  }"

  case parser.parse_video_info(json) {
    Ok(metadata) -> {
      // view_count should be None when missing
      option.is_none(metadata.view_count)
      |> should.be_true()
    }
    Error(_) -> should.fail()
  }
}

pub fn parse_video_info_invalid_json_test() {
  let json = "not valid json"

  parser.parse_video_info(json)
  |> should.be_error()
}

pub fn parse_video_info_missing_field_test() {
  let json =
    "{
    \"title\": \"Test Video\",
    \"thumbnail\": \"https://example.com/thumb.jpg\"
  }"

  parser.parse_video_info(json)
  |> should.be_error()
}

// Enhanced progress parsing tests
pub fn parse_enhanced_progress_full_test() {
  let line = "[download]  45.3% of  100.00MiB at  1.23MiB/s ETA 00:42"

  case parser.parse_enhanced_progress(line) {
    Ok(progress) -> {
      progress.percentage
      |> should.equal(45)

      progress.file_size
      |> should.equal(option.Some("100.00MiB"))

      progress.speed
      |> should.equal(option.Some("1.23MiB/s"))

      progress.eta
      |> should.equal(option.Some("00:42"))

      progress.message
      |> should.equal(line)
    }
    Error(_) -> should.fail()
  }
}

pub fn parse_enhanced_progress_partial_no_eta_test() {
  let line = "[download]  50.0% of  200.00MiB at  2.50MiB/s"

  case parser.parse_enhanced_progress(line) {
    Ok(progress) -> {
      progress.percentage
      |> should.equal(50)

      progress.file_size
      |> should.equal(option.Some("200.00MiB"))

      progress.speed
      |> should.equal(option.Some("2.50MiB/s"))

      // ETA should be None when not present
      option.is_none(progress.eta)
      |> should.be_true()
    }
    Error(_) -> should.fail()
  }
}

pub fn parse_enhanced_progress_percentage_only_test() {
  let line = "[download]  75%"

  case parser.parse_enhanced_progress(line) {
    Ok(progress) -> {
      progress.percentage
      |> should.equal(75)

      option.is_none(progress.file_size)
      |> should.be_true()

      option.is_none(progress.speed)
      |> should.be_true()

      option.is_none(progress.eta)
      |> should.be_true()
    }
    Error(_) -> should.fail()
  }
}

pub fn parse_enhanced_progress_100_percent_test() {
  let line = "[download] 100% of 512.00MiB in 00:30"

  case parser.parse_enhanced_progress(line) {
    Ok(progress) -> {
      progress.percentage
      |> should.equal(100)

      progress.file_size
      |> should.equal(option.Some("512.00MiB"))
    }
    Error(_) -> should.fail()
  }
}

pub fn parse_enhanced_progress_no_match_test() {
  let line = "Some other output"

  parser.parse_enhanced_progress(line)
  |> should.be_error()
}
