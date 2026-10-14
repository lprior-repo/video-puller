/// Own the application services in one ordered OTP RestForOne tree.
///
/// A failure in an upstream service restarts all dependent services that start
/// after it, while leaving independent upstream children running.
import core/managed_trash
import core/manager
import core/state_backup
import core/storage_guard
import core/subscription_manager.{type SubscriptionMessage}
import domain/types.{type ManagerMessage, GetStats}
import engine/ytdl_sub
import engine/ytdlp
import envoy
import gleam/erlang/process.{type Name, type Pid, type Subject, type Timer}
import gleam/io
import gleam/option.{type Option, Some}
import gleam/otp/actor
import gleam/otp/static_supervisor
import gleam/otp/supervision
import gleam/result
import gleam/string
import gleam/time/timestamp
import infra/db.{type Db}
import infra/plex
import infra/repo
import infra/subscription_repo
import web/server

pub type Services {
  Services(
    supervisor: static_supervisor.Supervisor,
    supervisor_pid: Pid,
    subscription_manager: Subject(SubscriptionMessage),
  )
}

type DownloadHealthMessage {
  CheckDownloadHealth
}

type DownloadHealthState {
  DownloadHealthState(
    manager: Subject(ManagerMessage),
    self: Subject(DownloadHealthMessage),
    timer: Option(Timer),
  )
}

/// Start the linked application supervisor and return the stable named subject
/// used by the web context across same-VM subscription-manager restarts.
pub fn start(
  db: Db,
  download_config: ytdlp.DownloadConfig,
  poll_interval_ms: Int,
  max_concurrency: Int,
) -> Result(Services, actor.StartError) {
  let subscription_name =
    process.new_name(prefix: "video_puller_subscription_manager")
  let subscription_subject = process.named_subject(subscription_name)
  static_supervisor.new(static_supervisor.RestForOne)
  |> static_supervisor.restart_tolerance(intensity: 10, period: 60)
  |> static_supervisor.add(storage_child())
  |> static_supervisor.add(
    supervision.worker(subscription_manager.start_poll_guard)
    |> supervision.timeout(ms: 15_000),
  )
  |> static_supervisor.add(download_child(
    db,
    download_config,
    poll_interval_ms,
    max_concurrency,
  ))
  |> static_supervisor.add(subscription_child(db, subscription_name))
  |> static_supervisor.add(server.supervised(db, subscription_subject))
  |> static_supervisor.add(plex_child())
  |> static_supervisor.add(backup_child(db))
  |> static_supervisor.start
  |> result.map(fn(started) {
    Services(started.data, started.pid, subscription_subject)
  })
}

fn storage_child() -> supervision.ChildSpecification(Nil) {
  supervision.worker(start_storage_wrapper)
  |> supervision.timeout(ms: 15_000)
}

fn download_child(
  db: Db,
  download_config: ytdlp.DownloadConfig,
  poll_interval_ms: Int,
  max_concurrency: Int,
) -> supervision.ChildSpecification(Nil) {
  supervision.worker(fn() {
    actor.new_with_initialiser(15_000, fn(subject) {
      initialise_download_service(
        subject,
        db,
        download_config,
        poll_interval_ms,
        max_concurrency,
      )
    })
    |> actor.on_message(handle_download_health)
    |> actor.start
  })
  |> supervision.timeout(ms: 15_000)
}

fn subscription_child(
  db: Db,
  subscription_name: Name(SubscriptionMessage),
) -> supervision.ChildSpecification(Nil) {
  supervision.worker(fn() {
    use subject <- result.try(subscription_manager.start_named(
      db,
      subscription_name,
    ))
    case subscription_repo.get_config(db) {
      Ok(config) ->
        case config.enabled && poll_on_start() {
          True -> process.send(subject, subscription_manager.Poll)
          False -> Nil
        }
      _ -> Nil
    }
    use pid <- result.try(
      process.subject_owner(subject)
      |> result.map_error(fn(_) {
        actor.InitFailed("subscription child has no owner")
      }),
    )
    Ok(actor.Started(pid:, data: Nil))
  })
  |> supervision.timeout(ms: 15_000)
}

fn plex_child() -> supervision.ChildSpecification(Nil) {
  supervision.worker(fn() {
    let pid =
      plex.start_title_sync(
        ytdl_sub.layout_from_env().library_dir,
        managed_trash.cleanup,
      )
    Ok(actor.Started(pid:, data: Nil))
  })
  |> supervision.timeout(ms: 15_000)
}

fn start_storage_wrapper() -> actor.StartResult(Nil) {
  Ok(actor.Started(pid: storage_guard.start(), data: Nil))
}

fn backup_child(db: Db) -> supervision.ChildSpecification(Nil) {
  supervision.worker(fn() {
    Ok(actor.Started(pid: state_backup.start(db), data: Nil))
  })
  |> supervision.timeout(ms: 15_000)
}

fn initialise_download_service(
  self: Subject(DownloadHealthMessage),
  db: Db,
  download_config: ytdlp.DownloadConfig,
  poll_interval_ms: Int,
  max_concurrency: Int,
) -> Result(
  actor.Initialised(DownloadHealthState, DownloadHealthMessage, Nil),
  String,
) {
  use _ <- result.try(
    repo.reset_zombies(db, unix_timestamp())
    |> result.map_error(fn(error) {
      "job recovery failed: " <> string.inspect(error)
    }),
  )
  use manager_subject <- result.try(
    manager.start(db, download_config, poll_interval_ms, max_concurrency)
    |> result.map_error(fn(error) {
      "download manager failed to start: " <> string.inspect(error)
    }),
  )
  let timer = process.send_after(self, 30_000, CheckDownloadHealth)
  Ok(actor.initialised(DownloadHealthState(manager_subject, self, Some(timer))))
}

fn handle_download_health(
  state: DownloadHealthState,
  message: DownloadHealthMessage,
) -> actor.Next(DownloadHealthState, DownloadHealthMessage) {
  case message {
    CheckDownloadHealth -> {
      let reply = process.new_subject()
      process.send(state.manager, GetStats(reply))
      case process.receive(reply, 5000) {
        Ok(_) -> {
          let timer =
            process.send_after(state.self, 30_000, CheckDownloadHealth)
          actor.continue(DownloadHealthState(..state, timer: Some(timer)))
        }
        Error(_) -> {
          io.println(
            "Download manager health check timed out; restarting services",
          )
          actor.stop_abnormal("download manager health check timed out")
        }
      }
    }
  }
}

fn poll_on_start() -> Bool {
  case envoy.get("POLL_ON_START") {
    Ok("true") | Ok("1") | Ok("yes") -> True
    _ -> False
  }
}

fn unix_timestamp() -> Int {
  let #(seconds, _) =
    timestamp.system_time()
    |> timestamp.to_unix_seconds_and_nanoseconds
  seconds
}
