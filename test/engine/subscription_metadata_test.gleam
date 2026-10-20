import engine/subscription_metadata
import gleam/string
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

pub fn recent_preset_keeps_metadata_and_requests_english_srt_test() {
  let block = subscription_metadata.preset_block(14)
  block |> string.contains("after: \"today-14days\"") |> should.be_true()
  block |> string.contains("breaks: True") |> should.be_true()
  block |> string.contains("type: upload_date") |> should.be_true()
  block |> string.contains("subtitles_type: srt") |> should.be_true()
  block |> string.contains("embed_subtitles: True") |> should.be_true()
  block
  |> string.contains("languages: [\"en.*\"]")
  |> should.be_true()
  block |> string.contains("\".*-orig\"") |> should.be_false()
  block
  |> string.contains("allow_auto_generated_subtitles: True")
  |> should.be_true()
  block
  |> string.contains("{episode_file_path}.{lang}.{subtitles_ext}")
  |> should.be_true()
  block |> string.contains("languages: [\"all\"]") |> should.be_false()
}

pub fn unrestricted_preset_omits_date_filter_test() {
  let block = subscription_metadata.preset_block(0)
  block |> string.contains("date_range:") |> should.be_false()
  block |> string.contains("subtitles:") |> should.be_true()
}

pub fn episode_title_override_uses_raw_video_title_test() {
  subscription_metadata.overrides_block()
  |> string.contains("episode_title: \"{title}\"")
  |> should.be_true()
}

pub fn restricted_filter_passes_missing_availability_metadata_test() {
  let enabled = subscription_metadata.restricted_filter_block(True)
  enabled
  |> string.contains(
    "availability !=? premium_only & availability !=? subscriber_only",
  )
  |> should.be_true()
  let disabled = subscription_metadata.restricted_filter_block(False)
  disabled |> should.equal("")
}
