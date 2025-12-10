/// Video Filter Tests
///
/// Unit tests for the video filtering logic used in subscription auto-download.
import domain/subscription_types.{
  type DiscoveredVideo, DiscoveredVideo, PassedFilter, SkippedExcludedKeyword,
  SkippedNoKeywordMatch, SkippedTooLong, SkippedTooOld, SkippedTooShort,
}
import engine/video_filter
import gleam/option.{None, Some}
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

// ============================================================================
// Test Fixtures
// ============================================================================

fn test_video(
  video_id: String,
  title: String,
  duration: option.Option(Int),
  published_at: option.Option(Int),
) -> DiscoveredVideo {
  DiscoveredVideo(
    video_id: video_id,
    channel_id: Some("UC123"),
    channel_name: Some("Test Channel"),
    title: title,
    url: "https://www.youtube.com/watch?v=" <> video_id,
    published_at: published_at,
    duration_seconds: duration,
    thumbnail_url: None,
  )
}

// Current timestamp for tests (2024-01-15 00:00:00 UTC)
const test_timestamp = 1_705_276_800

// ============================================================================
// Age Filter Tests
// ============================================================================

pub fn age_filter_passes_recent_video_test() {
  // Video published 1 day ago should pass 7-day filter
  let one_day_ago = test_timestamp - 86_400
  video_filter.check_age_filter(Some(one_day_ago), 7, test_timestamp)
  |> should.equal(PassedFilter)
}

pub fn age_filter_passes_video_at_limit_test() {
  // Video published exactly 7 days ago should pass 7-day filter
  let seven_days_ago = test_timestamp - { 7 * 86_400 }
  video_filter.check_age_filter(Some(seven_days_ago), 7, test_timestamp)
  |> should.equal(PassedFilter)
}

pub fn age_filter_rejects_old_video_test() {
  // Video published 8 days ago should fail 7-day filter
  let eight_days_ago = test_timestamp - { 8 * 86_400 }
  video_filter.check_age_filter(Some(eight_days_ago), 7, test_timestamp)
  |> should.equal(SkippedTooOld)
}

pub fn age_filter_passes_when_no_publish_date_test() {
  // Videos without publish date should pass (benefit of doubt)
  video_filter.check_age_filter(None, 7, test_timestamp)
  |> should.equal(PassedFilter)
}

// ============================================================================
// Duration Filter Tests
// ============================================================================

pub fn duration_filter_passes_within_bounds_test() {
  // 5 minute video should pass 2min-30min filter
  video_filter.check_duration_filter(Some(300), Some(120), Some(1800))
  |> should.equal(PassedFilter)
}

pub fn duration_filter_rejects_too_short_test() {
  // 60 second video should fail 2min minimum filter
  video_filter.check_duration_filter(Some(60), Some(120), None)
  |> should.equal(SkippedTooShort)
}

pub fn duration_filter_rejects_too_long_test() {
  // 2 hour video should fail 30min maximum filter
  video_filter.check_duration_filter(Some(7200), None, Some(1800))
  |> should.equal(SkippedTooLong)
}

pub fn duration_filter_passes_at_exact_minimum_test() {
  // 120 second video should pass 120s minimum filter
  video_filter.check_duration_filter(Some(120), Some(120), None)
  |> should.equal(PassedFilter)
}

pub fn duration_filter_passes_at_exact_maximum_test() {
  // 1800 second video should pass 1800s maximum filter
  video_filter.check_duration_filter(Some(1800), None, Some(1800))
  |> should.equal(PassedFilter)
}

pub fn duration_filter_passes_when_no_duration_test() {
  // Videos without duration should pass (benefit of doubt)
  video_filter.check_duration_filter(None, Some(120), Some(1800))
  |> should.equal(PassedFilter)
}

pub fn duration_filter_passes_with_no_limits_test() {
  // Any duration passes when no limits set
  video_filter.check_duration_filter(Some(36_000), None, None)
  |> should.equal(PassedFilter)
}

// ============================================================================
// Keyword Inclusion Filter Tests
// ============================================================================

pub fn keyword_filter_passes_with_empty_list_test() {
  // Empty keyword list means all videos pass
  video_filter.check_keyword_filter("Any Title Here", [])
  |> should.equal(PassedFilter)
}

pub fn keyword_filter_passes_with_matching_keyword_test() {
  video_filter.check_keyword_filter("My Tutorial Video", ["tutorial"])
  |> should.equal(PassedFilter)
}

pub fn keyword_filter_is_case_insensitive_test() {
  video_filter.check_keyword_filter("MY TUTORIAL VIDEO", ["tutorial"])
  |> should.equal(PassedFilter)
}

pub fn keyword_filter_matches_any_keyword_test() {
  // Should pass if ANY keyword matches (OR logic)
  video_filter.check_keyword_filter("Gaming Highlights", [
    "tutorial",
    "review",
    "highlights",
  ])
  |> should.equal(PassedFilter)
}

pub fn keyword_filter_rejects_no_match_test() {
  video_filter.check_keyword_filter("Random Video Title", [
    "tutorial",
    "review",
  ])
  |> should.equal(SkippedNoKeywordMatch)
}

pub fn keyword_filter_matches_partial_words_test() {
  // "gaming" contains "gam"
  video_filter.check_keyword_filter("Gaming Session", ["gam"])
  |> should.equal(PassedFilter)
}

// ============================================================================
// Keyword Exclusion Filter Tests
// ============================================================================

pub fn exclusion_filter_passes_with_empty_list_test() {
  video_filter.check_exclusion_filter("Any Title", [])
  |> should.equal(PassedFilter)
}

pub fn exclusion_filter_passes_when_no_match_test() {
  video_filter.check_exclusion_filter("Regular Video", ["sponsored", "ad"])
  |> should.equal(PassedFilter)
}

pub fn exclusion_filter_rejects_matching_keyword_test() {
  video_filter.check_exclusion_filter("This Video Is Sponsored", ["sponsored"])
  |> should.equal(SkippedExcludedKeyword("sponsored"))
}

pub fn exclusion_filter_is_case_insensitive_test() {
  video_filter.check_exclusion_filter("SPONSORED CONTENT", ["sponsored"])
  |> should.equal(SkippedExcludedKeyword("sponsored"))
}

pub fn exclusion_filter_returns_first_match_test() {
  // Should return the first matching excluded keyword
  video_filter.check_exclusion_filter("Sponsored Ad Content", [
    "sponsored",
    "ad",
  ])
  |> should.equal(SkippedExcludedKeyword("sponsored"))
}

// ============================================================================
// Full Filter Pipeline Tests (should_download)
// ============================================================================

pub fn should_download_passes_valid_video_test() {
  let config =
    subscription_types.SubscriptionConfig(
      enabled: True,
      poll_interval_minutes: 60,
      browser: subscription_types.Firefox,
      cookies_path: None,
      max_age_days: 7,
      min_duration_seconds: 120,
      max_duration_seconds: None,
      keyword_filter: [],
      keyword_exclude: [],
      last_poll_at: None,
    )

  let video =
    test_video("abc123", "Good Video", Some(600), Some(test_timestamp - 86_400))

  video_filter.should_download(video, config, None, test_timestamp)
  |> should.equal(PassedFilter)
}

pub fn should_download_rejects_short_video_test() {
  let config =
    subscription_types.SubscriptionConfig(
      enabled: True,
      poll_interval_minutes: 60,
      browser: subscription_types.Firefox,
      cookies_path: None,
      max_age_days: 7,
      min_duration_seconds: 120,
      max_duration_seconds: None,
      keyword_filter: [],
      keyword_exclude: [],
      last_poll_at: None,
    )

  // 30 second video (YouTube Short)
  let video =
    test_video("short123", "Short Video", Some(30), Some(test_timestamp - 3600))

  video_filter.should_download(video, config, None, test_timestamp)
  |> should.equal(SkippedTooShort)
}

pub fn should_download_rejects_old_video_test() {
  let config =
    subscription_types.SubscriptionConfig(
      enabled: True,
      poll_interval_minutes: 60,
      browser: subscription_types.Firefox,
      cookies_path: None,
      max_age_days: 7,
      min_duration_seconds: 120,
      max_duration_seconds: None,
      keyword_filter: [],
      keyword_exclude: [],
      last_poll_at: None,
    )

  // Video from 10 days ago
  let video =
    test_video(
      "old123",
      "Old Video",
      Some(600),
      Some(test_timestamp - { 10 * 86_400 }),
    )

  video_filter.should_download(video, config, None, test_timestamp)
  |> should.equal(SkippedTooOld)
}

pub fn should_download_with_keyword_filter_test() {
  let config =
    subscription_types.SubscriptionConfig(
      enabled: True,
      poll_interval_minutes: 60,
      browser: subscription_types.Firefox,
      cookies_path: None,
      max_age_days: 7,
      min_duration_seconds: 120,
      max_duration_seconds: None,
      keyword_filter: ["tutorial", "guide"],
      keyword_exclude: [],
      last_poll_at: None,
    )

  // Video with matching keyword should pass
  let good_video =
    test_video(
      "tut123",
      "Python Tutorial",
      Some(600),
      Some(test_timestamp - 3600),
    )
  video_filter.should_download(good_video, config, None, test_timestamp)
  |> should.equal(PassedFilter)

  // Video without keyword should be skipped
  let bad_video =
    test_video("bad123", "Random Vlog", Some(600), Some(test_timestamp - 3600))
  video_filter.should_download(bad_video, config, None, test_timestamp)
  |> should.equal(SkippedNoKeywordMatch)
}

pub fn should_download_with_exclusion_filter_test() {
  let config =
    subscription_types.SubscriptionConfig(
      enabled: True,
      poll_interval_minutes: 60,
      browser: subscription_types.Firefox,
      cookies_path: None,
      max_age_days: 7,
      min_duration_seconds: 120,
      max_duration_seconds: None,
      keyword_filter: [],
      keyword_exclude: ["sponsored", "ad"],
      last_poll_at: None,
    )

  // Video with excluded keyword should be skipped
  let bad_video =
    test_video(
      "sp123",
      "Sponsored Content",
      Some(600),
      Some(test_timestamp - 3600),
    )
  video_filter.should_download(bad_video, config, None, test_timestamp)
  |> should.equal(SkippedExcludedKeyword("sponsored"))

  // Clean video should pass
  let good_video =
    test_video(
      "good123",
      "Regular Content",
      Some(600),
      Some(test_timestamp - 3600),
    )
  video_filter.should_download(good_video, config, None, test_timestamp)
  |> should.equal(PassedFilter)
}

// ============================================================================
// Formatting Tests
// ============================================================================

pub fn format_duration_seconds_only_test() {
  video_filter.format_duration(45)
  |> should.equal("0:45")
}

pub fn format_duration_minutes_and_seconds_test() {
  video_filter.format_duration(185)
  |> should.equal("3:05")
}

pub fn format_duration_hours_test() {
  video_filter.format_duration(3665)
  |> should.equal("1:01:05")
}

pub fn format_age_minutes_test() {
  video_filter.format_age(test_timestamp - 1800, test_timestamp)
  |> should.equal("30 min ago")
}

pub fn format_age_hours_test() {
  video_filter.format_age(test_timestamp - 7200, test_timestamp)
  |> should.equal("2 hours ago")
}

pub fn format_age_days_test() {
  video_filter.format_age(test_timestamp - 259_200, test_timestamp)
  |> should.equal("3 days ago")
}

pub fn format_age_singular_day_test() {
  video_filter.format_age(test_timestamp - 86_400, test_timestamp)
  |> should.equal("1 day ago")
}
