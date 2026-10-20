import engine/youtube_request_options
import gleam/list
import gleam/option.{None, Some}
import gleeunit/should

pub fn anonymous_requests_have_pacing_without_authentication_test() {
  youtube_request_options.options(None)
  |> should.equal([
    #("sleep_interval_requests", "1.0"),
    #("sleep_interval", "5.0"),
    #("max_sleep_interval", "10.0"),
  ])
}

pub fn cookie_path_cannot_inject_yaml_options_test() {
  youtube_request_options.options(Some("/private/a\"\nother: true\\cookies"))
  |> list.first()
  |> should.equal(
    Ok(#("cookiefile", "\"/private/a\\\"\\nother: true\\\\cookies\"")),
  )
}

pub fn empty_cookie_path_keeps_anonymous_requests_test() {
  youtube_request_options.options(Some("  "))
  |> should.equal(youtube_request_options.options(None))
}
