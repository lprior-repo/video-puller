/// Video Filter Engine
///
/// Filtering logic to decide which videos to download based on configured
/// criteria: age, duration, keywords inclusion/exclusion.
import domain/subscription_types.{
  type ChannelSettings, type DiscoveredVideo, type FilterResult,
  type SubscriptionConfig, PassedFilter, SkippedExcludedKeyword,
  SkippedNoKeywordMatch, SkippedTooLong, SkippedTooOld, SkippedTooShort,
}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

/// Seconds per day constant
const seconds_per_day = 86_400

/// Check if a video passes all configured filters
pub fn should_download(
  video: DiscoveredVideo,
  config: SubscriptionConfig,
  channel_override: Option(ChannelSettings),
  current_timestamp: Int,
) -> FilterResult {
  // Get effective filter values (channel override or global config)
  let max_age_days =
    get_effective_int(
      channel_override
        |> option.then(fn(c) { c.max_age_days }),
      config.max_age_days,
    )

  let min_duration =
    get_effective_option(
      channel_override
        |> option.then(fn(c) { c.min_duration_seconds }),
      Some(config.min_duration_seconds),
    )

  let max_duration =
    get_effective_option(
      channel_override
        |> option.then(fn(c) { c.max_duration_seconds }),
      config.max_duration_seconds,
    )

  let keyword_filter =
    get_effective_list(
      channel_override
        |> option.map(fn(c) { c.keyword_filter }),
      config.keyword_filter,
    )

  let keyword_exclude =
    get_effective_list(
      channel_override
        |> option.map(fn(c) { c.keyword_exclude }),
      config.keyword_exclude,
    )

  // Apply filters in order of priority
  // 1. Age filter
  case check_age_filter(video.published_at, max_age_days, current_timestamp) {
    SkippedTooOld -> SkippedTooOld
    _ -> {
      // 2. Duration filter
      case
        check_duration_filter(
          video.duration_seconds,
          min_duration,
          max_duration,
        )
      {
        SkippedTooShort -> SkippedTooShort
        SkippedTooLong -> SkippedTooLong
        _ -> {
          // 3. Keyword inclusion filter
          case check_keyword_filter(video.title, keyword_filter) {
            SkippedNoKeywordMatch -> SkippedNoKeywordMatch
            _ -> {
              // 4. Keyword exclusion filter
              case check_exclusion_filter(video.title, keyword_exclude) {
                SkippedExcludedKeyword(kw) -> SkippedExcludedKeyword(kw)
                _ -> PassedFilter
              }
            }
          }
        }
      }
    }
  }
}

/// Check if video was published within max_age_days
pub fn check_age_filter(
  published_at: Option(Int),
  max_age_days: Int,
  current_timestamp: Int,
) -> FilterResult {
  case published_at {
    None ->
      // If no publish date, pass the filter (give benefit of doubt)
      PassedFilter
    Some(timestamp) -> {
      let age_seconds = current_timestamp - timestamp
      let max_age_seconds = max_age_days * seconds_per_day

      case age_seconds > max_age_seconds {
        True -> SkippedTooOld
        False -> PassedFilter
      }
    }
  }
}

/// Check if video duration is within bounds
pub fn check_duration_filter(
  duration: Option(Int),
  min_seconds: Option(Int),
  max_seconds: Option(Int),
) -> FilterResult {
  case duration {
    None ->
      // If no duration info, pass the filter
      PassedFilter
    Some(d) -> {
      // Check minimum duration
      let passes_min = case min_seconds {
        None -> True
        Some(min) -> d >= min
      }

      // Check maximum duration
      let passes_max = case max_seconds {
        None -> True
        Some(max) -> d <= max
      }

      case passes_min, passes_max {
        False, _ -> SkippedTooShort
        _, False -> SkippedTooLong
        True, True -> PassedFilter
      }
    }
  }
}

/// Check if title matches any required keywords (OR logic)
/// Empty keyword list means all videos pass
pub fn check_keyword_filter(
  title: String,
  keywords: List(String),
) -> FilterResult {
  case list.is_empty(keywords) {
    True -> PassedFilter
    False -> {
      let title_lower = string.lowercase(title)

      let matches =
        list.any(keywords, fn(kw) {
          string.contains(title_lower, string.lowercase(kw))
        })

      case matches {
        True -> PassedFilter
        False -> SkippedNoKeywordMatch
      }
    }
  }
}

/// Check if title contains any excluded keywords
pub fn check_exclusion_filter(
  title: String,
  excluded: List(String),
) -> FilterResult {
  case list.is_empty(excluded) {
    True -> PassedFilter
    False -> {
      let title_lower = string.lowercase(title)

      let found_excluded =
        list.find(excluded, fn(kw) {
          string.contains(title_lower, string.lowercase(kw))
        })

      case found_excluded {
        Ok(kw) -> SkippedExcludedKeyword(kw)
        Error(_) -> PassedFilter
      }
    }
  }
}

/// Get effective int value (override or default)
fn get_effective_int(override: Option(Int), default: Int) -> Int {
  case override {
    Some(value) -> value
    None -> default
  }
}

/// Get effective Option value (override or default)
fn get_effective_option(
  override: Option(Int),
  default: Option(Int),
) -> Option(Int) {
  case override {
    Some(_) -> override
    None -> default
  }
}

/// Get effective list value (override if non-empty, else default)
fn get_effective_list(
  override: Option(List(String)),
  default: List(String),
) -> List(String) {
  case override {
    Some(items) ->
      case list.is_empty(items) {
        True -> default
        False -> items
      }
    None -> default
  }
}

/// Format duration as human-readable string
pub fn format_duration(seconds: Int) -> String {
  let hours = seconds / 3600
  let remaining = seconds % 3600
  let minutes = remaining / 60
  let secs = remaining % 60

  case hours > 0 {
    True ->
      int.to_string(hours) <> ":" <> pad_zero(minutes) <> ":" <> pad_zero(secs)
    False -> int.to_string(minutes) <> ":" <> pad_zero(secs)
  }
}

/// Pad single digit with leading zero
fn pad_zero(n: Int) -> String {
  case n < 10 {
    True -> "0" <> int.to_string(n)
    False -> int.to_string(n)
  }
}

/// Format age as human-readable string
pub fn format_age(published_at: Int, current_timestamp: Int) -> String {
  let age_seconds = current_timestamp - published_at

  let days = age_seconds / seconds_per_day
  let hours = age_seconds % seconds_per_day / 3600
  let minutes = age_seconds % 3600 / 60

  case days > 0 {
    True -> int.to_string(days) <> " day" <> pluralize(days) <> " ago"
    False ->
      case hours > 0 {
        True -> int.to_string(hours) <> " hour" <> pluralize(hours) <> " ago"
        False -> int.to_string(minutes) <> " min ago"
      }
  }
}

/// Simple pluralization helper
fn pluralize(n: Int) -> String {
  case n == 1 {
    True -> ""
    False -> "s"
  }
}
