/// Domain types for subscription auto-download feature
///
/// This module defines types for managing YouTube subscription feeds,
/// filtering criteria, and tracking discovered videos.
import gleam/option.{type Option}

/// Subscription feature configuration
/// Stored as singleton row in subscription_config table
pub type SubscriptionConfig {
  SubscriptionConfig(
    enabled: Bool,
    poll_interval_minutes: Int,
    browser: Browser,
    cookies_path: Option(String),
    max_age_days: Int,
    min_duration_seconds: Int,
    max_duration_seconds: Option(Int),
    keyword_filter: List(String),
    keyword_exclude: List(String),
    last_poll_at: Option(Int),
  )
}

/// Supported browsers for cookie extraction
/// Maps to yt-dlp --cookies-from-browser values
pub type Browser {
  Firefox
  Chrome
  Chromium
  Edge
  Brave
  Safari
  Opera
}

/// Video discovered from subscription feed
/// Represents raw data from yt-dlp --flat-playlist output
pub type DiscoveredVideo {
  DiscoveredVideo(
    video_id: String,
    channel_id: Option(String),
    channel_name: Option(String),
    title: String,
    url: String,
    published_at: Option(Int),
    duration_seconds: Option(Int),
    thumbnail_url: Option(String),
  )
}

/// Result of filtering a video
/// Used to decide whether to queue for download and to record skip reasons
pub type FilterResult {
  PassedFilter
  SkippedTooOld
  SkippedTooShort
  SkippedTooLong
  SkippedNoKeywordMatch
  SkippedExcludedKeyword(String)
  SkippedAlreadySeen
  SkippedAlreadyDownloaded
}

/// Subscription poll result summary
/// Returned after each poll operation for status display
pub type PollResult {
  PollResult(
    total_found: Int,
    new_videos: Int,
    queued_for_download: Int,
    skipped: Int,
    errors: List(String),
  )
}

/// Seen video record from database
/// Tracks all videos discovered from subscription feed
pub type SeenVideo {
  SeenVideo(
    video_id: String,
    channel_id: Option(String),
    channel_name: Option(String),
    title: String,
    url: String,
    published_at: Option(Int),
    duration_seconds: Option(Int),
    thumbnail_url: Option(String),
    first_seen_at: Int,
    downloaded: Bool,
    skipped: Bool,
    skip_reason: Option(String),
    job_id: Option(String),
  )
}

/// Per-channel settings override
/// Allows customizing filter rules per channel
pub type ChannelSettings {
  ChannelSettings(
    channel_id: String,
    channel_name: String,
    enabled: Bool,
    priority: Int,
    max_age_days: Option(Int),
    min_duration_seconds: Option(Int),
    max_duration_seconds: Option(Int),
    keyword_filter: List(String),
    keyword_exclude: List(String),
  )
}

/// Subscription status for UI display
pub type SubscriptionStatus {
  SubscriptionStatus(
    enabled: Bool,
    last_poll_at: Option(Int),
    next_poll_at: Option(Int),
    last_result: Option(PollResult),
    is_polling: Bool,
  )
}

// Helper functions

/// Convert Browser to yt-dlp argument string
pub fn browser_to_string(browser: Browser) -> String {
  case browser {
    Firefox -> "firefox"
    Chrome -> "chrome"
    Chromium -> "chromium"
    Edge -> "edge"
    Brave -> "brave"
    Safari -> "safari"
    Opera -> "opera"
  }
}

/// Parse string to Browser type
pub fn string_to_browser(s: String) -> Browser {
  case s {
    "firefox" -> Firefox
    "chrome" -> Chrome
    "chromium" -> Chromium
    "edge" -> Edge
    "brave" -> Brave
    "safari" -> Safari
    "opera" -> Opera
    _ -> Firefox
  }
}

/// Convert FilterResult to human-readable skip reason
pub fn filter_result_to_string(result: FilterResult) -> String {
  case result {
    PassedFilter -> "Passed"
    SkippedTooOld -> "Video too old"
    SkippedTooShort -> "Video too short"
    SkippedTooLong -> "Video too long"
    SkippedNoKeywordMatch -> "No keyword match"
    SkippedExcludedKeyword(kw) -> "Excluded keyword: " <> kw
    SkippedAlreadySeen -> "Already seen"
    SkippedAlreadyDownloaded -> "Already downloaded"
  }
}

/// Check if a filter result means the video was skipped
pub fn is_skipped(result: FilterResult) -> Bool {
  case result {
    PassedFilter -> False
    _ -> True
  }
}

/// Create a default subscription config
pub fn default_config() -> SubscriptionConfig {
  SubscriptionConfig(
    enabled: False,
    poll_interval_minutes: 60,
    browser: Firefox,
    cookies_path: option.None,
    max_age_days: 7,
    min_duration_seconds: 120,
    max_duration_seconds: option.None,
    keyword_filter: [],
    keyword_exclude: [],
    last_poll_at: option.None,
  )
}

/// Create an empty poll result
pub fn empty_poll_result() -> PollResult {
  PollResult(
    total_found: 0,
    new_videos: 0,
    queued_for_download: 0,
    skipped: 0,
    errors: [],
  )
}
