import engine/subscription_recovery.{State}
import gleeunit/should

pub fn partial_success_with_bot_errors_retries_and_keeps_last_success_test() {
  let previous = State(0, 0, 123, "healthy")
  let failed =
    subscription_recovery.complete(
      previous,
      ["Sign in to confirm you’re not a bot"],
      1000,
      300_000,
    )
  failed.failures |> should.equal(1)
  failed.retry_at |> should.equal(1300)
  failed.last_success_at |> should.equal(123)
}

pub fn repeated_failures_back_off_with_a_finite_cap_test() {
  subscription_recovery.backoff_ms(1, 300_000) |> should.equal(300_000)
  subscription_recovery.backoff_ms(2, 300_000) |> should.equal(600_000)
  subscription_recovery.backoff_ms(20, 300_000) |> should.equal(3_600_000)
  subscription_recovery.backoff_ms(999_999, 999_999_999)
  |> should.equal(3_600_000)
}

pub fn success_resets_failures_and_records_actual_success_test() {
  subscription_recovery.complete(
    State(4, 9999, 123, "retrying"),
    [],
    2000,
    300_000,
  )
  |> should.equal(State(0, 0, 2000, "healthy"))
}

pub fn permanent_single_video_errors_do_not_trigger_retry_storms_test() {
  subscription_recovery.permanent_item_error("one private video was skipped")
  |> should.be_true()
  subscription_recovery.complete(
    subscription_recovery.initial(),
    ["one private video was skipped"],
    100,
    300_000,
  ).failures
  |> should.equal(0)
}

pub fn unavailable_is_not_proof_of_a_permanent_skip_test() {
  subscription_recovery.complete(
    State(0, 0, 50, "healthy"),
    ["Video unavailable. This content isn't available, try again later"],
    100,
    300_000,
  )
  |> should.equal(State(1, 400, 50, "retrying"))
}

pub fn authentication_errors_take_precedence_over_item_skip_words_test() {
  subscription_recovery.permanent_item_error(
    "Private video: sign in to confirm you're not a bot",
  )
  |> should.be_false()
  subscription_recovery.complete(
    State(2, 500, 50, "retrying"),
    ["Video unavailable: HTTP Error 429: Too Many Requests"],
    1000,
    300_000,
  )
  |> should.equal(State(3, 2200, 50, "retrying"))
}

pub fn shared_authentication_rate_and_network_failures_stop_the_queue_test() {
  subscription_recovery.blocks_remaining_jobs([
    "HTTP Error 429: Too Many Requests",
  ])
  |> should.be_true()
  subscription_recovery.blocks_remaining_jobs([
    "Sign in to confirm you're not a bot",
  ])
  |> should.be_true()
  subscription_recovery.blocks_remaining_jobs(["network unavailable"])
  |> should.be_true()
  subscription_recovery.blocks_remaining_jobs(["private video"])
  |> should.be_false()
  subscription_recovery.blocks_remaining_jobs(["one channel timed out"])
  |> should.be_false()
}
