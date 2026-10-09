/// Domain types for the subscription auto-download feature
///
/// The subscription feature drives the ytdl-sub engine over a list of public
/// channel URLs; it never reads browser cookies.
import gleam/option.{type Option, None}

/// Subscription feature configuration
/// Stored as singleton row in subscription_config table
pub type SubscriptionConfig {
  SubscriptionConfig(
    enabled: Bool,
    poll_interval_minutes: Int,
    last_poll_at: Option(Int),
  )
}

/// Video discovered or downloaded by the subscription engine
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

/// Download record from the library
/// Tracks every file the subscription engine has added
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

/// Create a default subscription config
pub fn default_config() -> SubscriptionConfig {
  SubscriptionConfig(
    enabled: False,
    poll_interval_minutes: 360,
    last_poll_at: None,
  )
}
