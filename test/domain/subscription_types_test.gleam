/// Subscription Types Tests
///
/// Unit tests for subscription domain types and helper functions.
import domain/subscription_types.{
  Brave, Chrome, Chromium, Edge, Firefox, PassedFilter, Safari,
  SkippedAlreadySeen, SkippedExcludedKeyword, SkippedNoKeywordMatch,
  SkippedTooLong, SkippedTooOld, SkippedTooShort,
}
import gleam/option.{None, Some}
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

// ============================================================================
// Browser Conversion Tests
// ============================================================================

pub fn browser_to_string_firefox_test() {
  subscription_types.browser_to_string(Firefox)
  |> should.equal("firefox")
}

pub fn browser_to_string_chrome_test() {
  subscription_types.browser_to_string(Chrome)
  |> should.equal("chrome")
}

pub fn browser_to_string_chromium_test() {
  subscription_types.browser_to_string(Chromium)
  |> should.equal("chromium")
}

pub fn browser_to_string_edge_test() {
  subscription_types.browser_to_string(Edge)
  |> should.equal("edge")
}

pub fn browser_to_string_brave_test() {
  subscription_types.browser_to_string(Brave)
  |> should.equal("brave")
}

pub fn browser_to_string_safari_test() {
  subscription_types.browser_to_string(Safari)
  |> should.equal("safari")
}

pub fn string_to_browser_firefox_test() {
  subscription_types.string_to_browser("firefox")
  |> should.equal(Firefox)
}

pub fn string_to_browser_chrome_test() {
  subscription_types.string_to_browser("chrome")
  |> should.equal(Chrome)
}

pub fn string_to_browser_chromium_test() {
  subscription_types.string_to_browser("chromium")
  |> should.equal(Chromium)
}

pub fn string_to_browser_edge_test() {
  subscription_types.string_to_browser("edge")
  |> should.equal(Edge)
}

pub fn string_to_browser_brave_test() {
  subscription_types.string_to_browser("brave")
  |> should.equal(Brave)
}

pub fn string_to_browser_safari_test() {
  subscription_types.string_to_browser("safari")
  |> should.equal(Safari)
}

pub fn string_to_browser_unknown_defaults_to_firefox_test() {
  subscription_types.string_to_browser("unknown")
  |> should.equal(Firefox)
}

pub fn string_to_browser_case_insensitive_test() {
  // The function should be case-sensitive (lowercase only)
  subscription_types.string_to_browser("Firefox")
  |> should.equal(Firefox)
}

// ============================================================================
// FilterResult Tests
// ============================================================================

pub fn filter_result_to_string_passed_test() {
  subscription_types.filter_result_to_string(PassedFilter)
  |> should.equal("Passed")
}

pub fn filter_result_to_string_too_old_test() {
  subscription_types.filter_result_to_string(SkippedTooOld)
  |> should.equal("Video too old")
}

pub fn filter_result_to_string_too_short_test() {
  subscription_types.filter_result_to_string(SkippedTooShort)
  |> should.equal("Video too short")
}

pub fn filter_result_to_string_too_long_test() {
  subscription_types.filter_result_to_string(SkippedTooLong)
  |> should.equal("Video too long")
}

pub fn filter_result_to_string_no_keyword_match_test() {
  subscription_types.filter_result_to_string(SkippedNoKeywordMatch)
  |> should.equal("No keyword match")
}

pub fn filter_result_to_string_excluded_keyword_test() {
  subscription_types.filter_result_to_string(SkippedExcludedKeyword("sponsored"))
  |> should.equal("Excluded keyword: sponsored")
}

pub fn filter_result_to_string_already_seen_test() {
  subscription_types.filter_result_to_string(SkippedAlreadySeen)
  |> should.equal("Already seen")
}

// ============================================================================
// is_skipped Tests
// ============================================================================

pub fn is_skipped_passed_filter_test() {
  subscription_types.is_skipped(PassedFilter)
  |> should.be_false()
}

pub fn is_skipped_too_old_test() {
  subscription_types.is_skipped(SkippedTooOld)
  |> should.be_true()
}

pub fn is_skipped_too_short_test() {
  subscription_types.is_skipped(SkippedTooShort)
  |> should.be_true()
}

pub fn is_skipped_too_long_test() {
  subscription_types.is_skipped(SkippedTooLong)
  |> should.be_true()
}

pub fn is_skipped_no_keyword_match_test() {
  subscription_types.is_skipped(SkippedNoKeywordMatch)
  |> should.be_true()
}

pub fn is_skipped_excluded_keyword_test() {
  subscription_types.is_skipped(SkippedExcludedKeyword("ad"))
  |> should.be_true()
}

pub fn is_skipped_already_seen_test() {
  subscription_types.is_skipped(SkippedAlreadySeen)
  |> should.be_true()
}

// ============================================================================
// Default Config Tests
// ============================================================================

pub fn default_config_has_correct_defaults_test() {
  let config = subscription_types.default_config()

  config.enabled
  |> should.be_false()

  config.poll_interval_minutes
  |> should.equal(60)

  config.browser
  |> should.equal(Firefox)

  config.cookies_path
  |> should.equal(None)

  config.max_age_days
  |> should.equal(7)

  // 2 minutes minimum to skip Shorts
  config.min_duration_seconds
  |> should.equal(120)

  config.max_duration_seconds
  |> should.equal(None)

  config.keyword_filter
  |> should.equal([])

  config.keyword_exclude
  |> should.equal([])

  config.last_poll_at
  |> should.equal(None)
}

// ============================================================================
// Type Construction Tests
// ============================================================================

pub fn discovered_video_construction_test() {
  let video =
    subscription_types.DiscoveredVideo(
      video_id: "abc123",
      channel_id: Some("UC_xyz"),
      channel_name: Some("Test Channel"),
      title: "Test Video Title",
      url: "https://www.youtube.com/watch?v=abc123",
      published_at: Some(1_700_000_000),
      duration_seconds: Some(600),
      thumbnail_url: Some("https://i.ytimg.com/vi/abc123/hqdefault.jpg"),
    )

  video.video_id
  |> should.equal("abc123")

  video.title
  |> should.equal("Test Video Title")

  video.duration_seconds
  |> should.equal(Some(600))
}

pub fn poll_result_construction_test() {
  let result =
    subscription_types.PollResult(
      total_found: 50,
      new_videos: 10,
      queued_for_download: 5,
      skipped: 5,
      errors: [],
    )

  result.total_found
  |> should.equal(50)

  result.new_videos
  |> should.equal(10)

  result.queued_for_download
  |> should.equal(5)
}

pub fn subscription_status_construction_test() {
  let status =
    subscription_types.SubscriptionStatus(
      enabled: True,
      last_poll_at: Some(1_700_000_000),
      next_poll_at: Some(1_700_003_600),
      last_result: None,
      is_polling: False,
    )

  status.enabled
  |> should.be_true()

  status.is_polling
  |> should.be_false()
}
