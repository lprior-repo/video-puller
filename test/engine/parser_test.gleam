import engine/parser
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
