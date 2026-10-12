import gleam/option.{None, Some}
import gleeunit
import gleeunit/should
import video_puller_watchdog.{
  Config, Fault, Leave, Recover, Running, Service, State, Waiting,
}

pub fn main() {
  gleeunit.main()
}

fn config() {
  Config(
    "/app",
    "/state",
    300,
    3,
    600,
    15,
    "local.video-puller",
    "/agent.plist",
  )
}

fn service() {
  Service("gui/501/local.video-puller", 1234, "process birth fingerprint", 8080)
}

fn ready_state() {
  State(Some(service()), 1000, 1400, 0, None, "healthy")
}

pub fn healthy_app_never_restarts_test() {
  let #(state, action) =
    video_puller_watchdog.decide(
      config(),
      ready_state(),
      Running(service(), True),
      1401,
    )
  action |> should.equal(Leave)
  state.failures |> should.equal(0)
  state.status |> should.equal("healthy")
}

pub fn slow_start_gets_grace_test() {
  let #(state, action) =
    video_puller_watchdog.decide(
      config(),
      video_puller_watchdog.empty_state(),
      Running(service(), False),
      1400,
    )
  action |> should.equal(Leave)
  state.first_seen |> should.equal(1400)
  state.failures |> should.equal(0)
  state.status |> should.equal("starting")
}

pub fn first_failure_never_restarts_test() {
  let #(state, action) =
    video_puller_watchdog.decide(
      config(),
      ready_state(),
      Running(service(), False),
      1401,
    )
  action |> should.equal(Leave)
  state.failures |> should.equal(1)
}

pub fn three_consecutive_failures_request_recovery_test() {
  let #(one, _) =
    video_puller_watchdog.decide(
      config(),
      ready_state(),
      Running(service(), False),
      1401,
    )
  let #(two, second_action) =
    video_puller_watchdog.decide(config(), one, Running(service(), False), 1461)
  second_action |> should.equal(Leave)
  let #(three, action) =
    video_puller_watchdog.decide(config(), two, Running(service(), False), 1521)
  action |> should.equal(Recover(service()))
  three.last_restart |> should.equal(Some(1521))
}

pub fn recovery_resets_consecutive_failures_test() {
  let old = State(..ready_state(), failures: 2)
  let #(state, action) =
    video_puller_watchdog.decide(config(), old, Running(service(), True), 1401)
  action |> should.equal(Leave)
  state.failures |> should.equal(0)
}

pub fn new_pid_gets_full_startup_grace_test() {
  let new = Service(..service(), pid: 1235, identity: "new process")
  let old = State(..ready_state(), failures: 2)
  let #(state, action) =
    video_puller_watchdog.decide(config(), old, Running(new, False), 1401)
  action |> should.equal(Leave)
  state.status |> should.equal("starting")
  state.first_seen |> should.equal(1401)
}

pub fn reused_pid_with_new_birth_gets_grace_test() {
  let new =
    Service(..service(), identity: "reused PID with different birth time")
  let #(state, action) =
    video_puller_watchdog.decide(
      config(),
      ready_state(),
      Running(new, False),
      1401,
    )
  action |> should.equal(Leave)
  state.instance |> should.equal(Some(new))
  state.status |> should.equal("starting")
}

pub fn cooldown_survives_pid_change_test() {
  let old = State(..ready_state(), failures: 2, last_restart: Some(1350))
  let #(state, action) =
    video_puller_watchdog.decide(config(), old, Running(service(), False), 1401)
  action |> should.equal(Leave)
  state.status |> should.equal("cooldown")
  let new = Service(..service(), pid: 4567, identity: "replacement")
  let #(new_state, _) =
    video_puller_watchdog.decide(config(), state, Running(new, False), 1402)
  new_state.last_restart |> should.equal(Some(1350))
}

pub fn cooldown_expiry_allows_recovery_test() {
  let old = State(..ready_state(), failures: 2, last_restart: Some(1350))
  let #(_, action) =
    video_puller_watchdog.decide(config(), old, Running(service(), False), 1950)
  action |> should.equal(Recover(service()))
}

pub fn unloaded_service_is_left_to_launchd_test() {
  let old = State(..ready_state(), failures: 2)
  let #(state, action) =
    video_puller_watchdog.decide(config(), old, Waiting, 1401)
  action |> should.equal(Leave)
  state.instance |> should.equal(None)
  state.failures |> should.equal(0)
}

pub fn inspection_error_never_restarts_test() {
  let old = State(..ready_state(), failures: 2)
  let #(state, action) =
    video_puller_watchdog.decide(config(), old, Fault("wrong entrypoint"), 1401)
  action |> should.equal(Leave)
  state.status |> should.equal("error")
  state.failures |> should.equal(0)
}

pub fn backwards_clock_resets_history_test() {
  let old = State(..ready_state(), failures: 2, last_restart: Some(1400))
  let #(state, action) =
    video_puller_watchdog.decide(config(), old, Running(service(), False), 1200)
  action |> should.equal(Leave)
  state.status |> should.equal("starting")
  state.last_restart |> should.equal(None)
}
