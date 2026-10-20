import engine/youtube_session
import gleeunit/should

pub fn successful_refresh_is_reported_test() {
  youtube_session.refresh_with(fn() { Ok(Nil) }, False)
  |> should.equal(Ok("refreshed"))
}

pub fn locked_browser_keeps_the_existing_session_test() {
  youtube_session.refresh_with(fn() { Error("browser locked") }, True)
  |> should.equal(Ok("using_saved_session"))
}

pub fn failed_refresh_without_a_session_requires_recovery_test() {
  youtube_session.refresh_with(fn() { Error("sensitive browser error") }, False)
  |> should.equal(Error(
    "YouTube session refresh failed; local browser access is required",
  ))
}
