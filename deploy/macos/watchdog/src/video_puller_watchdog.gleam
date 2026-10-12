import gleam/erlang/process
import gleam/io
import gleam/option.{type Option, None, Some}

pub type Config {
  Config(
    app_dir: String,
    state_dir: String,
    startup_grace: Int,
    failure_threshold: Int,
    restart_cooldown: Int,
    termination_grace: Int,
    agent_label: String,
    agent_plist: String,
  )
}

pub type Service {
  Service(target: String, pid: Int, identity: String, port: Int)
}

pub type Observation {
  Waiting
  Fault(String)
  Running(Service, Bool)
}

pub type State {
  State(
    instance: Option(Service),
    first_seen: Int,
    last_check: Int,
    failures: Int,
    last_restart: Option(Int),
    status: String,
  )
}

pub type Action {
  Leave
  Recover(Service)
}

pub fn empty_state() -> State {
  State(None, 0, 0, 0, None, "new")
}

/// A failed probe alone never restarts the app. Reset history on PID changes,
/// allow startup, and keep a cooldown even when the previous restart failed.
pub fn decide(
  config: Config,
  previous: State,
  observation: Observation,
  now: Int,
) -> #(State, Action) {
  let previous = case now < previous.last_check {
    True -> empty_state()
    False -> previous
  }
  let state = State(..previous, last_check: now)
  case observation {
    Waiting -> #(
      State(..state, instance: None, failures: 0, status: "waiting"),
      Leave,
    )
    Fault(_) -> #(State(..state, failures: 0, status: "error"), Leave)
    Running(service, healthy) -> {
      let state = case state.instance == Some(service) {
        True -> state
        False ->
          State(..state, instance: Some(service), first_seen: now, failures: 0)
      }
      case healthy {
        True -> #(State(..state, failures: 0, status: "healthy"), Leave)
        False if now - state.first_seen < config.startup_grace -> #(
          State(..state, failures: 0, status: "starting"),
          Leave,
        )
        False -> {
          let state =
            State(..state, failures: state.failures + 1, status: "unhealthy")
          let cooling_down = case state.last_restart {
            Some(last) -> now - last < config.restart_cooldown
            None -> False
          }
          case state.failures >= config.failure_threshold, cooling_down {
            True, False -> #(
              State(
                ..state,
                last_restart: Some(now),
                failures: 0,
                status: "restart",
              ),
              Recover(service),
            )
            True, True -> #(State(..state, status: "cooldown"), Leave)
            _, _ -> #(state, Leave)
          }
        }
      }
    }
  }
}

pub fn main() -> Nil {
  case read_config() {
    Error(reason) -> io.println("Watchdog configuration error: " <> reason)
    Ok(config) -> {
      with_lock(config.state_dir, fn() {
        let previous = load_state(config.state_dir)
        let observation = observe(config)
        let #(state, action) = decide(config, previous, observation, now())
        // Persist the restart cooldown before signalling the service. A monitor
        // crash must not create an immediate second recovery attempt.
        case save_state(config.state_dir, state) {
          Error(reason) -> log_event(config.state_dir, "error", reason)
          Ok(_) -> {
            case previous.status != state.status {
              True ->
                log_event(
                  config.state_dir,
                  state.status,
                  observation_detail(observation),
                )
              False -> Nil
            }
            case action {
              Leave -> Nil
              Recover(service) -> recover(config, service)
            }
          }
        }
      })
    }
  }
}

fn observation_detail(observation: Observation) -> String {
  case observation {
    Waiting -> "launchd owns startup; no eligible running service"
    Fault(reason) -> reason
    Running(service, _) -> service.target
  }
}

fn recover(config: Config, service: Service) -> Nil {
  // Recheck the launchd target and process birth fingerprint immediately before
  // acting. A replacement process or a newly healthy app gets no signal.
  case observe(config) {
    Running(current, False) if current == service -> {
      case send_signal(service, "TERM") {
        Error(reason) -> log_event(config.state_dir, "error", reason)
        Ok(_) -> {
          let still_alive = wait_for_exit(service, config.termination_grace)
          case still_alive {
            False -> log_event(config.state_dir, "terminated", service.target)
            True -> {
              case observe(config) {
                Running(current, _) if current == service -> {
                  case send_signal(service, "KILL") {
                    Ok(_) ->
                      log_event(config.state_dir, "forced_exit", service.target)
                    Error(reason) ->
                      log_event(config.state_dir, "error", reason)
                  }
                }
                _ -> Nil
              }
            }
          }
        }
      }
    }
    _ -> log_event(config.state_dir, "recovered_or_changed", service.target)
  }
}

fn wait_for_exit(service: Service, remaining: Int) -> Bool {
  case same_process(service) {
    False -> False
    True if remaining <= 0 -> True
    True -> {
      process.sleep(1000)
      wait_for_exit(service, remaining - 1)
    }
  }
}

@external(erlang, "watchdog_ffi", "read_config")
fn read_config() -> Result(Config, String)

@external(erlang, "watchdog_ffi", "observe")
fn observe(config: Config) -> Observation

@external(erlang, "watchdog_ffi", "load_state")
fn load_state(directory: String) -> State

@external(erlang, "watchdog_ffi", "save_state")
fn save_state(directory: String, state: State) -> Result(Nil, String)

@external(erlang, "watchdog_ffi", "with_lock")
fn with_lock(directory: String, run: fn() -> Nil) -> Nil

@external(erlang, "watchdog_ffi", "log_event")
fn log_event(directory: String, event: String, detail: String) -> Nil

@external(erlang, "watchdog_ffi", "same_process")
fn same_process(service: Service) -> Bool

@external(erlang, "watchdog_ffi", "send_signal")
fn send_signal(service: Service, signal: String) -> Result(Nil, String)

@external(erlang, "erlang", "system_time")
fn system_time(unit: TimeUnit) -> Int

type TimeUnit {
  Second
}

fn now() -> Int {
  system_time(Second)
}
