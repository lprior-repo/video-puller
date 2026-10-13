/// Bounded policy for retrying slow or repeatedly incomplete Plex library scans.
import gleam/int
import gleam/option.{type Option, None, Some}

const minimum_cooldown_ms = 30_000

const default_cooldown_ms = 300_000

const maximum_cooldown_ms = 3_600_000

/// Convert a configured cooldown in seconds to a bounded millisecond value.
/// Invalid configuration should be passed as None and uses the five-minute default.
pub fn cooldown_ms(configured_seconds: Option(Int)) -> Int {
  case configured_seconds {
    Some(seconds) if seconds < 30 -> minimum_cooldown_ms
    Some(seconds) if seconds > 3600 -> maximum_cooldown_ms
    Some(seconds) -> seconds * 1000
    None -> default_cooldown_ms
  }
}

/// Parse the optional environment value, defaulting on missing or invalid input.
pub fn configured_cooldown_ms(value: Option(String)) -> Int {
  case value {
    Some(raw) ->
      case int.parse(raw) {
        Ok(seconds) -> cooldown_ms(Some(seconds))
        Error(_) -> default_cooldown_ms
      }
    None -> default_cooldown_ms
  }
}
