/// Retry policy for workflow failures, independent of OTP process health.
import gleam/int
import gleam/list
import gleam/string

pub type State {
  State(failures: Int, retry_at: Int, last_success_at: Int, status: String)
}

pub fn initial() -> State {
  State(0, 0, 0, "not_started")
}

pub fn permanent_item_error(message: String) -> Bool {
  let message = string.lowercase(message)
  !blocks_remaining_jobs([message])
  && list.any(
    [
      "private video",
      "video isn't available anymore",
      "removed by the uploader",
      "members-only",
      "copyright claim",
    ],
    fn(marker) { string.contains(message, marker) },
  )
}

pub fn blocks_remaining_jobs(errors: List(String)) -> Bool {
  list.any(errors, fn(message) {
    let message = string.lowercase(message)
    list.any(
      [
        "confirm you're not a bot",
        "confirm you’re not a bot",
        "http error 429",
        "too many requests",
        "network unavailable",
        "name resolution",
        "session refresh failed",
        "youtube account cookies are no longer valid",
      ],
      fn(marker) { string.contains(message, marker) },
    )
  })
}

/// Only a clean poll (or permanently unavailable individual items) restores
/// normal cadence. Partial success must not hide recoverable failures.
pub fn complete(
  previous: State,
  errors: List(String),
  now: Int,
  base_retry_ms: Int,
) -> State {
  case list.filter(errors, fn(error) { !permanent_item_error(error) }) {
    [] -> State(0, 0, now, "healthy")
    _ -> {
      let failures = int.min(previous.failures + 1, 20)
      let delay = backoff_ms(failures, base_retry_ms)
      State(
        failures,
        now + int.max(delay / 1000, 1),
        previous.last_success_at,
        "retrying",
      )
    }
  }
}

pub fn backoff_ms(failures: Int, base_retry_ms: Int) -> Int {
  double(
    int.clamp(failures - 1, 0, 8),
    int.clamp(base_retry_ms, 1000, 3_600_000),
  )
}

fn double(remaining: Int, delay: Int) -> Int {
  case remaining <= 0 || delay >= 3_600_000 {
    True -> int.min(delay, 3_600_000)
    False -> double(remaining - 1, int.min(delay * 2, 3_600_000))
  }
}
