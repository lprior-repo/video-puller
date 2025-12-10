/// Retry Module - Exponential Backoff with Circuit Breaker
///
/// Implements BEAM-style fault tolerance patterns:
/// - Exponential backoff for transient failures
/// - Circuit breaker to prevent cascade failures
/// - Jitter to prevent thundering herd
/// - Per-job retry tracking with max attempts
import gleam/dict.{type Dict}
import gleam/erlang/process.{type Subject}
import gleam/float
import gleam/int
import gleam/io
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/result

/// Retry configuration
pub type RetryConfig {
  RetryConfig(
    max_attempts: Int,
    base_delay_ms: Int,
    max_delay_ms: Int,
    multiplier: Float,
    jitter_percent: Int,
  )
}

/// Default retry configuration (sensible defaults for video downloads)
pub fn default_retry_config() -> RetryConfig {
  RetryConfig(
    max_attempts: 3,
    base_delay_ms: 1000,
    // 1 second base
    max_delay_ms: 300_000,
    // 5 minutes max
    multiplier: 2.0,
    jitter_percent: 25,
  )
}

/// Retry state for a single item
pub type RetryState {
  RetryState(
    item_id: String,
    attempt: Int,
    last_failure: Option(String),
    next_retry_at: Option(Int),
  )
}

/// Calculate the next retry delay with exponential backoff and jitter
pub fn calculate_delay(config: RetryConfig, attempt: Int) -> Int {
  // Exponential: base * multiplier^attempt
  let power_result =
    float.power(config.multiplier, int.to_float(attempt))
    |> result.unwrap(1.0)
  let exponential_delay =
    float.round(int.to_float(config.base_delay_ms) *. power_result)

  // Clamp to max
  let clamped_delay = int.min(exponential_delay, config.max_delay_ms)

  // Add jitter
  let jitter_range = { clamped_delay * config.jitter_percent } / 100
  let jitter = random_int(0, jitter_range * 2) - jitter_range

  int.max(0, clamped_delay + jitter)
}

/// Check if we should retry based on attempt count
pub fn should_retry(config: RetryConfig, attempt: Int) -> Bool {
  attempt < config.max_attempts
}

// ============================================================================
// Circuit Breaker - Prevents cascade failures
// ============================================================================

/// Circuit breaker states
pub type CircuitState {
  Closed
  // Normal operation, requests pass through
  Open
  // Circuit tripped, requests fail fast
  HalfOpen
  // Testing if service recovered
}

/// Circuit breaker configuration
pub type CircuitBreakerConfig {
  CircuitBreakerConfig(
    failure_threshold: Int,
    // Failures before opening
    success_threshold: Int,
    // Successes to close from half-open
    reset_timeout_ms: Int,
    // Time before trying half-open
    sample_window_ms: Int,
    // Window for counting failures
  )
}

/// Default circuit breaker config
pub fn default_circuit_config() -> CircuitBreakerConfig {
  CircuitBreakerConfig(
    failure_threshold: 5,
    success_threshold: 2,
    reset_timeout_ms: 30_000,
    // 30 seconds
    sample_window_ms: 60_000,
    // 1 minute window
  )
}

/// Circuit breaker state
pub opaque type CircuitBreakerState {
  CircuitBreakerState(
    config: CircuitBreakerConfig,
    state: CircuitState,
    failure_count: Int,
    success_count: Int,
    last_failure_at: Int,
    opened_at: Int,
    self: Option(Subject(CircuitBreakerMessage)),
  )
}

/// Circuit breaker messages
pub type CircuitBreakerMessage {
  // Record a success
  RecordSuccess
  // Record a failure
  RecordFailure(reason: String)
  // Check if request should be allowed
  CheckAllowed(reply: Subject(Bool))
  // Get current state
  GetState(reply: Subject(CircuitState))
  // Internal: attempt to half-open
  AttemptHalfOpen
  // Set self reference
  SetSelf(Subject(CircuitBreakerMessage))
  // Reset circuit
  Reset
}

/// Start a circuit breaker actor
pub fn start_circuit_breaker(
  config: CircuitBreakerConfig,
) -> Result(Subject(CircuitBreakerMessage), actor.StartError) {
  let initial_state =
    CircuitBreakerState(
      config: config,
      state: Closed,
      failure_count: 0,
      success_count: 0,
      last_failure_at: 0,
      opened_at: 0,
      self: None,
    )

  actor.start(
    actor.new(initial_state)
    |> actor.on_message(handle_circuit_message),
  )
  |> result.map(fn(started) {
    let subject = started.data
    process.send(subject, SetSelf(subject))
    subject
  })
}

/// Handle circuit breaker messages
fn handle_circuit_message(
  state: CircuitBreakerState,
  message: CircuitBreakerMessage,
) -> actor.Next(CircuitBreakerState, CircuitBreakerMessage) {
  case message {
    SetSelf(subject) -> {
      actor.continue(CircuitBreakerState(..state, self: Some(subject)))
    }

    RecordSuccess -> {
      case state.state {
        Closed -> {
          // Reset failure count on success in closed state
          actor.continue(CircuitBreakerState(..state, failure_count: 0))
        }
        HalfOpen -> {
          // Count successes in half-open
          let new_success_count = state.success_count + 1

          case new_success_count >= state.config.success_threshold {
            True -> {
              // Close the circuit
              io.println("🟢 Circuit breaker: CLOSED (recovered)")
              actor.continue(
                CircuitBreakerState(
                  ..state,
                  state: Closed,
                  failure_count: 0,
                  success_count: 0,
                ),
              )
            }
            False -> {
              actor.continue(
                CircuitBreakerState(..state, success_count: new_success_count),
              )
            }
          }
        }
        Open -> {
          // Shouldn't happen - ignore
          actor.continue(state)
        }
      }
    }

    RecordFailure(reason) -> {
      let current_time = get_timestamp()

      case state.state {
        Closed -> {
          let new_failure_count = state.failure_count + 1

          case new_failure_count >= state.config.failure_threshold {
            True -> {
              // Open the circuit
              io.println(
                "🔴 Circuit breaker: OPEN (failures="
                <> int.to_string(new_failure_count)
                <> ", reason="
                <> reason
                <> ")",
              )

              // Schedule half-open attempt
              case state.self {
                Some(self) -> {
                  schedule_half_open(self, state.config.reset_timeout_ms)
                }
                None -> Nil
              }

              actor.continue(
                CircuitBreakerState(
                  ..state,
                  state: Open,
                  failure_count: new_failure_count,
                  last_failure_at: current_time,
                  opened_at: current_time,
                ),
              )
            }
            False -> {
              actor.continue(
                CircuitBreakerState(
                  ..state,
                  failure_count: new_failure_count,
                  last_failure_at: current_time,
                ),
              )
            }
          }
        }
        HalfOpen -> {
          // Failed during test - reopen
          io.println("🔴 Circuit breaker: OPEN (test failed: " <> reason <> ")")

          // Schedule another half-open attempt
          case state.self {
            Some(self) -> {
              schedule_half_open(self, state.config.reset_timeout_ms)
            }
            None -> Nil
          }

          actor.continue(
            CircuitBreakerState(
              ..state,
              state: Open,
              success_count: 0,
              last_failure_at: current_time,
              opened_at: current_time,
            ),
          )
        }
        Open -> {
          // Already open - just update timestamp
          actor.continue(
            CircuitBreakerState(..state, last_failure_at: current_time),
          )
        }
      }
    }

    CheckAllowed(reply) -> {
      let allowed = case state.state {
        Closed -> True
        HalfOpen -> True
        // Allow limited requests in half-open
        Open -> False
      }
      process.send(reply, allowed)
      actor.continue(state)
    }

    GetState(reply) -> {
      process.send(reply, state.state)
      actor.continue(state)
    }

    AttemptHalfOpen -> {
      case state.state {
        Open -> {
          io.println("🟡 Circuit breaker: HALF-OPEN (testing)")
          actor.continue(
            CircuitBreakerState(..state, state: HalfOpen, success_count: 0),
          )
        }
        _ -> actor.continue(state)
      }
    }

    Reset -> {
      io.println("🟢 Circuit breaker: RESET")
      actor.continue(
        CircuitBreakerState(
          ..state,
          state: Closed,
          failure_count: 0,
          success_count: 0,
        ),
      )
    }
  }
}

/// Schedule a half-open attempt
fn schedule_half_open(
  circuit: Subject(CircuitBreakerMessage),
  delay_ms: Int,
) -> Nil {
  let _ = send_after(circuit, delay_ms, AttemptHalfOpen)
  Nil
}

// ============================================================================
// Retry Manager - Tracks retry state for multiple items
// ============================================================================

/// Retry manager state
pub opaque type RetryManagerState {
  RetryManagerState(
    config: RetryConfig,
    items: Dict(String, RetryState),
    circuit_breaker: Option(Subject(CircuitBreakerMessage)),
    self: Option(Subject(RetryManagerMessage)),
  )
}

/// Retry manager messages
pub type RetryManagerMessage {
  // Check if item can be retried
  CanRetry(item_id: String, reply: Subject(RetryDecision))
  // Record a failure for an item
  RecordItemFailure(item_id: String, reason: String)
  // Record success (clear retry state)
  RecordItemSuccess(item_id: String)
  // Schedule a retry for an item
  ScheduleRetry(item_id: String, callback: Subject(RetryCallback))
  // Internal: execute scheduled retry
  ExecuteRetry(item_id: String, callback: Subject(RetryCallback))
  // Get retry state for an item
  GetRetryState(item_id: String, reply: Subject(Option(RetryState)))
  // Clear all retry states
  ClearAll
  // Set self reference
  SetSelfRetry(Subject(RetryManagerMessage))
}

/// Decision about whether to retry
pub type RetryDecision {
  // Can retry, here's the delay
  RetryAfter(delay_ms: Int, attempt: Int)
  // No more retries allowed
  NoMoreRetries(attempts_made: Int)
  // Circuit breaker is open
  CircuitOpen
}

/// Callback when retry should execute
pub type RetryCallback {
  RetryNow(item_id: String, attempt: Int)
}

/// Start a retry manager
pub fn start_retry_manager(
  config: RetryConfig,
  circuit_breaker: Option(Subject(CircuitBreakerMessage)),
) -> Result(Subject(RetryManagerMessage), actor.StartError) {
  let initial_state =
    RetryManagerState(
      config: config,
      items: dict.new(),
      circuit_breaker: circuit_breaker,
      self: None,
    )

  actor.start(
    actor.new(initial_state)
    |> actor.on_message(handle_retry_message),
  )
  |> result.map(fn(started) {
    let subject = started.data
    process.send(subject, SetSelfRetry(subject))
    subject
  })
}

/// Handle retry manager messages
fn handle_retry_message(
  state: RetryManagerState,
  message: RetryManagerMessage,
) -> actor.Next(RetryManagerState, RetryManagerMessage) {
  case message {
    SetSelfRetry(subject) -> {
      actor.continue(RetryManagerState(..state, self: Some(subject)))
    }

    CanRetry(item_id, reply) -> {
      // Check circuit breaker first
      let circuit_open = case state.circuit_breaker {
        Some(cb) -> {
          let check_reply = process.new_subject()
          process.send(cb, CheckAllowed(check_reply))
          case process.receive(check_reply, 1000) {
            Ok(allowed) -> !allowed
            Error(_) -> False
          }
        }
        None -> False
      }

      case circuit_open {
        True -> {
          process.send(reply, CircuitOpen)
          actor.continue(state)
        }
        False -> {
          let attempt = case dict.get(state.items, item_id) {
            Ok(retry_state) -> retry_state.attempt
            Error(_) -> 0
          }

          case should_retry(state.config, attempt) {
            True -> {
              let delay = calculate_delay(state.config, attempt)
              process.send(reply, RetryAfter(delay, attempt + 1))
            }
            False -> {
              process.send(reply, NoMoreRetries(attempt))
            }
          }

          actor.continue(state)
        }
      }
    }

    RecordItemFailure(item_id, reason) -> {
      let current_time = get_timestamp()

      let new_state = case dict.get(state.items, item_id) {
        Ok(retry_state) -> {
          let next_attempt = retry_state.attempt + 1
          let delay_ms = calculate_delay(state.config, next_attempt)
          // Convert delay from ms to nanoseconds (system_time returns ns)
          let next_retry = current_time + { delay_ms * 1_000_000 }
          let updated =
            RetryState(
              ..retry_state,
              attempt: next_attempt,
              last_failure: Some(reason),
              next_retry_at: Some(next_retry),
            )
          dict.insert(state.items, item_id, updated)
        }
        Error(_) -> {
          let delay_ms = calculate_delay(state.config, 1)
          let next_retry = current_time + { delay_ms * 1_000_000 }
          let new_retry_state =
            RetryState(
              item_id: item_id,
              attempt: 1,
              last_failure: Some(reason),
              next_retry_at: Some(next_retry),
            )
          dict.insert(state.items, item_id, new_retry_state)
        }
      }

      // Also notify circuit breaker
      case state.circuit_breaker {
        Some(cb) -> process.send(cb, RecordFailure(reason))
        None -> Nil
      }

      actor.continue(RetryManagerState(..state, items: new_state))
    }

    RecordItemSuccess(item_id) -> {
      // Clear retry state on success
      let new_items = dict.delete(state.items, item_id)

      // Notify circuit breaker
      case state.circuit_breaker {
        Some(cb) -> process.send(cb, RecordSuccess)
        None -> Nil
      }

      actor.continue(RetryManagerState(..state, items: new_items))
    }

    ScheduleRetry(item_id, callback) -> {
      let attempt = case dict.get(state.items, item_id) {
        Ok(retry_state) -> retry_state.attempt
        Error(_) -> 0
      }

      let delay = calculate_delay(state.config, attempt)

      io.println(
        "⏱️  Scheduling retry for "
        <> item_id
        <> " in "
        <> int.to_string(delay)
        <> "ms (attempt "
        <> int.to_string(attempt + 1)
        <> ")",
      )

      // Schedule the retry using BEAM scheduler
      case state.self {
        Some(self) -> {
          let _ = send_after_retry(self, delay, ExecuteRetry(item_id, callback))
          Nil
        }
        None -> Nil
      }

      actor.continue(state)
    }

    ExecuteRetry(item_id, callback) -> {
      let attempt = case dict.get(state.items, item_id) {
        Ok(retry_state) -> retry_state.attempt + 1
        Error(_) -> 1
      }

      io.println(
        "🔄 Executing retry for "
        <> item_id
        <> " (attempt "
        <> int.to_string(attempt)
        <> ")",
      )

      process.send(callback, RetryNow(item_id, attempt))
      actor.continue(state)
    }

    GetRetryState(item_id, reply) -> {
      let result = case dict.get(state.items, item_id) {
        Ok(retry_state) -> Some(retry_state)
        Error(_) -> None
      }
      process.send(reply, result)
      actor.continue(state)
    }

    ClearAll -> {
      actor.continue(RetryManagerState(..state, items: dict.new()))
    }
  }
}

/// Generate a random integer
fn random_int(min: Int, max: Int) -> Int {
  case max > min {
    True -> min + erlang_random_uniform(max - min + 1) - 1
    False -> min
  }
}

/// Get current timestamp
@external(erlang, "os", "system_time")
fn get_timestamp() -> Int

/// Erlang's random
@external(erlang, "rand", "uniform")
fn erlang_random_uniform(n: Int) -> Int

/// Send message after delay
@external(erlang, "timer", "send_after")
fn send_after(
  target: Subject(CircuitBreakerMessage),
  delay_ms: Int,
  message: CircuitBreakerMessage,
) -> Result(Nil, Nil)

/// Send retry message after delay
@external(erlang, "timer", "send_after")
fn send_after_retry(
  target: Subject(RetryManagerMessage),
  delay_ms: Int,
  message: RetryManagerMessage,
) -> Result(Nil, Nil)
