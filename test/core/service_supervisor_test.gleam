/// Runtime integration coverage for the production RestForOne service tree.
import core/service_supervisor
import core/subscription_manager.{type SubscriptionMessage}
import domain/subscription_types.{SubscriptionConfig}
import engine/ytdlp
import envoy
import gleam/erlang/process
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/time/timestamp
import gleeunit
import gleeunit/should
import infra/db
import infra/migrator
import infra/subscription_repo
import simplifile

pub fn main() {
  gleeunit.main()
}

pub fn production_tree_restarts_subscription_and_real_mist_server_test() {
  let env = configure_fixture()
  let database_path = "/tmp/video_puller_service_supervisor.db"
  let data_dir = "/tmp/video_puller_service_supervisor"
  let _ = simplifile.delete(database_path)
  let _ = simplifile.delete(database_path <> "-shm")
  let _ = simplifile.delete(database_path <> "-wal")
  let _ = simplifile.delete_all([data_dir])
  let _ = simplifile.create_directory_all(data_dir)
  let connection = db.init_db(database_path) |> should.be_ok()
  migrator.run_migrations(connection) |> should.be_ok()
  let _ =
    subscription_repo.update_config(
      connection,
      SubscriptionConfig(
        enabled: False,
        poll_interval_minutes: 360,
        last_poll_at: None,
      ),
      unix_timestamp(),
    )
    |> should.be_ok()

  let services =
    service_supervisor.start(
      connection,
      test_download_config(data_dir),
      60_000,
      1,
    )
    |> should.be_ok()
  process.unlink(services.supervisor_pid)

  let first_subscription =
    process.subject_owner(services.subscription_manager) |> should.be_ok()
  let first_storage = child_by_id(services.supervisor_pid, 0)
  let first_download = child_by_id(services.supervisor_pid, 2)
  let first_guard = child_by_id(services.supervisor_pid, 1)
  let first_plex = child_by_id(services.supervisor_pid, 5)
  let first_backup = child_by_id(services.supervisor_pid, 6)
  initial_module(first_storage) |> should.equal("core@storage_guard")
  initial_module(first_plex) |> should.equal("infra@plex")
  initial_module(first_backup) |> should.equal("core@state_backup")
  let first_http = wait_for_http_child(services.supervisor_pid)
  wait_for_routes() |> should.be_true()

  // The fourth production child is the real named subscription actor. OTP
  // restarts it and every later child; the preceding download service lives.
  test_exit(first_subscription, "kill")
  let second_subscription =
    wait_for_new_subscription(services.subscription_manager, first_subscription)
  let second_http = wait_for_new_http_child(services.supervisor_pid, first_http)
  process.is_alive(first_download) |> should.be_true()
  should.be_true(first_subscription != second_subscription)
  should.be_true(first_http != second_http)
  process.is_alive(first_storage) |> should.be_true()
  wait_for_routes() |> should.be_true()

  // Killing the actual Mist supervisor child restarts only HTTP and Plex. The
  // named manager and download service remain live and the routes recover.
  test_exit(second_http, "kill")
  let third_http = wait_for_new_http_child(services.supervisor_pid, second_http)
  should.be_true(third_http != second_http)
  process.subject_owner(services.subscription_manager)
  |> should.equal(Ok(second_subscription))
  process.is_alive(first_download) |> should.be_true()
  process.is_alive(first_storage) |> should.be_true()
  wait_for_routes() |> should.be_true()

  // An upstream download-wrapper failure restarts every dependent child while
  // preserving the independent storage guard process.
  test_exit(first_download, "kill")
  let second_download =
    wait_for_new_child(services.supervisor_pid, 2, first_download)
  let third_subscription =
    wait_for_new_subscription(
      services.subscription_manager,
      second_subscription,
    )
  let fourth_http = wait_for_new_http_child(services.supervisor_pid, third_http)
  should.be_true(second_download != first_download)
  should.be_true(third_subscription != second_subscription)
  should.be_true(fourth_http != third_http)
  process.is_alive(first_storage) |> should.be_true()
  wait_for_routes() |> should.be_true()

  // The backup service is the actual work process, owned directly by OTP.
  // Its crash must not disturb the preceding web or downloader services.
  let backup_before = child_by_id(services.supervisor_pid, 6)
  // A listener may retry binding during the preceding cascade. Capture the
  // supervisor now serving healthy routes rather than an intermediate PID.
  let stable_http = wait_for_http_child(services.supervisor_pid)
  test_exit(backup_before, "kill")
  let replacement_backup =
    wait_for_new_child(services.supervisor_pid, 6, backup_before)
  initial_module(replacement_backup) |> should.equal("core@state_backup")
  process.subject_owner(services.subscription_manager)
  |> should.equal(Ok(third_subscription))
  process.is_alive(second_download) |> should.be_true()
  process.is_alive(stable_http) |> should.be_true()
  wait_for_http_child(services.supervisor_pid) |> should.equal(stable_http)

  // Losing the guardian restarts dependent actors after restoring ownership.
  test_exit(first_guard, "kill")
  let _ = wait_for_new_child(services.supervisor_pid, 1, first_guard)
  let _ =
    wait_for_new_subscription(services.subscription_manager, third_subscription)
  wait_for_routes() |> should.be_true()

  stop_supervisor(services.supervisor_pid)
  let _ = db.close(connection)
  restore_env(env)
  let _ = simplifile.delete(database_path)
  let _ = simplifile.delete(database_path <> "-shm")
  let _ = simplifile.delete(database_path <> "-wal")
  let _ = simplifile.delete_all([data_dir])
  Nil
}

@external(erlang, "service_supervisor_test_ffi", "initial_module")
fn initial_module(pid: process.Pid) -> String

type EnvValue {
  EnvValue(name: String, previous: Option(String))
}

fn configure_fixture() -> List(EnvValue) {
  [
    save_env("DATA_DIR", "/tmp/video_puller_service_supervisor"),
    save_env("DB_PATH", "/tmp/video_puller_service_supervisor.db"),
    save_env("PORT", "8098"),
    save_env("STATIC_DIR", "./priv/static"),
    save_env("OUTPUT_DIR", "/tmp/video_puller_service_supervisor/output"),
    save_env("PLEX_SECTION_ID", ""),
    save_env("STORAGE_VOLUME", ""),
    save_env("POLL_ON_START", "false"),
    save_env("POLL_TIMEOUT_MINUTES", "1"),
    save_env("MIN_DOWNLOADERS", "0"),
    save_env("MAX_DOWNLOADERS", "1"),
  ]
}

fn save_env(name: String, value: String) -> EnvValue {
  let previous = case envoy.get(name) {
    Ok(value) -> Some(value)
    Error(_) -> None
  }
  envoy.set(name, value)
  EnvValue(name, previous)
}

fn restore_env(values: List(EnvValue)) -> Nil {
  list.each(values, fn(value) {
    case value.previous {
      Some(previous) -> envoy.set(value.name, previous)
      None -> envoy.unset(value.name)
    }
  })
}

fn test_download_config(data_dir: String) -> ytdlp.DownloadConfig {
  ytdlp.DownloadConfig(
    output_directory: data_dir <> "/output",
    format: "best",
    max_filesize: "1G",
    audio_only: False,
    audio_format: ytdlp.BestAudio,
    allow_playlist: False,
    download_timeout_ms: 60_000,
    rate_limit_delay_ms: 0,
    bandwidth_limit: "",
    use_channel_folders: True,
  )
}

fn child_by_id(supervisor: process.Pid, id: Int) -> process.Pid {
  supervisor_child(supervisor, id)
  |> should.be_ok()
}

fn wait_for_http_child(supervisor: process.Pid) -> process.Pid {
  let assert Ok(pid) = supervisor_child_by_type(supervisor, "supervisor")
  pid
}

fn wait_for_new_http_child(
  supervisor: process.Pid,
  previous: process.Pid,
) -> process.Pid {
  wait_until(fn() {
    case supervisor_child_by_type(supervisor, "supervisor") {
      Ok(pid) if pid != previous -> Some(pid)
      _ -> None
    }
  })
}

fn wait_for_new_child(
  supervisor: process.Pid,
  id: Int,
  previous: process.Pid,
) -> process.Pid {
  wait_until(fn() {
    case supervisor_child(supervisor, id) {
      Ok(pid) if pid != previous -> Some(pid)
      _ -> None
    }
  })
}

fn wait_for_new_subscription(
  subject: process.Subject(SubscriptionMessage),
  previous: process.Pid,
) -> process.Pid {
  wait_until(fn() {
    case process.subject_owner(subject) {
      Ok(pid) if pid != previous -> Some(pid)
      _ -> None
    }
  })
}

fn wait_until(read: fn() -> Option(process.Pid)) -> process.Pid {
  wait_until_attempt(read, 100)
}

fn wait_until_attempt(
  read: fn() -> Option(process.Pid),
  remaining: Int,
) -> process.Pid {
  case read() {
    Some(pid) -> pid
    None if remaining > 0 -> {
      process.sleep(50)
      wait_until_attempt(read, remaining - 1)
    }
    None -> panic as "Timed out waiting for supervised process restart"
  }
}

fn wait_for_routes() -> Bool {
  test_wait_for_routes(50, 50)
}

fn unix_timestamp() -> Int {
  let #(seconds, _) =
    timestamp.system_time()
    |> timestamp.to_unix_seconds_and_nanoseconds
  seconds
}

@external(erlang, "service_supervisor_test_ffi", "supervisor_child")
fn supervisor_child(
  supervisor: process.Pid,
  id: Int,
) -> Result(process.Pid, Nil)

@external(erlang, "service_supervisor_test_ffi", "supervisor_child_by_type")
fn supervisor_child_by_type(
  supervisor: process.Pid,
  child_type: String,
) -> Result(process.Pid, Nil)

@external(erlang, "service_supervisor_test_ffi", "exit_process")
fn test_exit(pid: process.Pid, reason: String) -> Nil

@external(erlang, "service_supervisor_test_ffi", "stop_supervisor")
fn stop_supervisor(pid: process.Pid) -> Nil

@external(erlang, "service_supervisor_test_ffi", "wait_for_routes")
fn test_wait_for_routes(attempts: Int, delay_ms: Int) -> Bool
