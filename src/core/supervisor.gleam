/// Root Supervisor Tree
///
/// Manages the application's main processes with fault tolerance.
/// Supervises the manager actor with a one_for_one restart strategy.
import core/manager
import core/startup
import engine/ytdlp
import gleam/erlang/process
import gleam/int
import gleam/io
import gleam/otp/actor
import gleam/otp/static_supervisor as supervisor
import gleam/otp/supervision
import infra/db.{type Db, type DbError}

/// Application state passed to children
pub type AppState {
  AppState(
    db: Db,
    config: ytdlp.DownloadConfig,
    poll_interval_ms: Int,
    max_concurrency: Int,
  )
}

/// Start the root supervisor and application
pub fn start() -> Result(actor.Started(supervisor.Supervisor), actor.StartError) {
  // Run startup sequence first
  io.println("🌳 Starting supervisor tree...")

  case startup.initialize() {
    Ok(db) -> {
      io.println("✅ Startup sequence complete")

      // Get configuration from environment
      let config = load_config()
      let poll_interval = get_env_int("POLL_INTERVAL_MS", 5000)
      let max_concurrency = get_env_int("MAX_CONCURRENCY", 3)

      // Create application state
      let app_state =
        AppState(
          db: db,
          config: config,
          poll_interval_ms: poll_interval,
          max_concurrency: max_concurrency,
        )

      // Start the supervisor with the manager as a child
      start_supervisor(app_state)
    }
    Error(err) -> {
      io.println("❌ Startup failed: " <> error_to_string(err))
      Error(actor.InitFailed("Startup sequence failed"))
    }
  }
}

/// Start the supervisor with child specifications
fn start_supervisor(
  app_state: AppState,
) -> Result(actor.Started(supervisor.Supervisor), actor.StartError) {
  supervisor.new(supervisor.OneForOne)
  |> supervisor.add(manager_child_spec(app_state))
  |> supervisor.start
}

/// Create the manager child specification
fn manager_child_spec(
  app_state: AppState,
) -> supervision.ChildSpecification(Nil) {
  supervision.worker(fn() {
    io.println(
      "🎯 Starting manager (poll="
      <> int.to_string(app_state.poll_interval_ms)
      <> "ms, max_concurrent="
      <> int.to_string(app_state.max_concurrency)
      <> ")",
    )

    case
      manager.start(
        app_state.db,
        app_state.config,
        app_state.poll_interval_ms,
        app_state.max_concurrency,
      )
    {
      Ok(subject) -> {
        io.println("✅ Manager started")
        // Get the PID from the subject
        case process.subject_owner(subject) {
          Ok(pid) -> Ok(actor.Started(pid: pid, data: Nil))
          Error(_) -> Error(actor.InitFailed("Failed to get manager PID"))
        }
      }
      Error(err) -> {
        io.println("❌ Failed to start manager")
        Error(err)
      }
    }
  })
  |> supervision.timeout(ms: 10_000)
  |> supervision.restart(supervision.Permanent)
}

/// Load download configuration from environment
fn load_config() -> ytdlp.DownloadConfig {
  let output_dir = get_env_string("OUTPUT_DIR", "./downloads")
  let format =
    get_env_string(
      "YTDLP_FORMAT",
      "bestvideo[ext=mp4]+bestaudio[ext=m4a]/best[ext=mp4]/best",
    )
  let max_filesize = get_env_string("MAX_FILESIZE", "2G")

  ytdlp.DownloadConfig(
    output_directory: output_dir,
    format: format,
    max_filesize: max_filesize,
  )
}

/// Get string from environment with default fallback
fn get_env_string(key: String, default: String) -> String {
  case system_env_get(key) {
    Ok(value) -> value
    Error(_) -> default
  }
}

/// Get integer from environment with default fallback
fn get_env_int(key: String, default: Int) -> Int {
  case system_env_get(key) {
    Ok(value) ->
      case int.parse(value) {
        Ok(n) -> n
        Error(_) -> default
      }
    Error(_) -> default
  }
}

/// Get environment variable using Erlang's os:getenv
@external(erlang, "os", "getenv")
fn system_env_get(key: String) -> Result(String, Nil)

/// Convert DbError to string for display
fn error_to_string(err: DbError) -> String {
  case err {
    db.ConnectionError(msg) -> "Connection error: " <> msg
    db.QueryError(msg) -> "Query error: " <> msg
    db.MigrationError(msg) -> "Migration error: " <> msg
  }
}
