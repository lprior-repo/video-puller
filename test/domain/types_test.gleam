import domain/types
import gleam/option.{None}
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

// Test JobId creation and conversion
pub fn job_id_test() {
  let id = types.new_job_id("test-123")
  types.job_id_to_string(id)
  |> should.equal("test-123")
}

// Test status conversions
pub fn status_to_string_test() {
  types.status_to_string(types.Pending)
  |> should.equal("pending")

  types.status_to_string(types.Downloading(45))
  |> should.equal("downloading")

  types.status_to_string(types.Completed)
  |> should.equal("completed")

  types.status_to_string(types.Failed("error"))
  |> should.equal("failed")
}

pub fn string_to_status_test() {
  types.string_to_status("pending")
  |> should.equal(types.Pending)

  types.string_to_status("downloading")
  |> should.equal(types.Downloading(0))

  types.string_to_status("completed")
  |> should.equal(types.Completed)

  types.string_to_status("failed")
  |> should.equal(types.Failed("Unknown error"))
}

// Test terminal status detection
pub fn is_terminal_status_test() {
  types.is_terminal_status(types.Pending)
  |> should.be_false()

  types.is_terminal_status(types.Downloading(50))
  |> should.be_false()

  types.is_terminal_status(types.Completed)
  |> should.be_true()

  types.is_terminal_status(types.Failed("error"))
  |> should.be_true()
}

// Test VideoJob creation
pub fn video_job_test() {
  let job =
    types.VideoJob(
      id: types.new_job_id("job-1"),
      url: "https://example.com/video",
      status: types.Pending,
      path: None,
      title: None,
      thumbnail_url: None,
      duration_seconds: None,
      format_code: None,
      created_at: 1000,
      updated_at: 1000,
    )

  types.job_id_to_string(job.id)
  |> should.equal("job-1")

  job.url
  |> should.equal("https://example.com/video")
}
