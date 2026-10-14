/// Application startup sequence
///
/// Handles critical startup operations:
/// - Preparing the ytdl-sub engine layout for subscription pulls
/// - Running database migrations (INV-002: must run before app start)
/// - Fixing zombie jobs (INV-003: revert processing jobs to pending)
import core/channel_reconciler
import core/plex_scan_exclusion
import core/pressure_retention
import core/storage_guard
import core/subscription_manager
import engine/shell
import engine/takeout
import engine/ytdl_sub
import envoy
import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import infra/db.{type Db, type DbError}
import infra/migrator
import infra/plex
import infra/repo
import simplifile

/// Run the complete startup sequence
pub fn initialize() -> Result(Db, DbError) {
  io.println("🔎 Verifying external storage identity...")
  use _ <- result.try(
    storage_guard.check_volume() |> result.map_error(db.ConnectionError),
  )
  io.println("✅ External storage identity verified")
  plex_scan_exclusion.configure(ytdl_sub.layout_from_env().root)
  io.println("🧹 Checking persisted downloader ownership records...")
  let recovered_downloaders = shell.recover_orphan_downloads()
  case recovered_downloaders > 0 {
    True ->
      io.println(
        "🧹 Stopped "
        <> int.to_string(recovered_downloaders)
        <> " orphaned downloader process(es)",
      )
    False -> Nil
  }
  io.println("✅ Downloader ownership recovery complete")
  // A full disk can prevent SQLite opening its WAL; recover media space before
  // opening the database, without ever bypassing durable download history.
  use recovered <- result.try(
    pressure_retention.cleanup(ytdl_sub.layout_from_env())
    |> result.map_error(db.ConnectionError),
  )
  case recovered {
    [] -> Nil
    ids ->
      io.println(
        "Low-space videos removed before database startup: "
        <> int.to_string(list.length(ids)),
      )
  }
  io.println("🚀 Starting FractalVideoEater...")

  // Get database path from environment or use default
  let db_path = case envoy.get("DB_PATH") {
    Ok(path) -> path
    Error(_) -> "./data/video_eater.db"
  }

  io.println("📁 Database: " <> db_path)

  // SQLite cannot create the file inside a directory that does not exist
  ensure_database_directory(db_path)

  // Initialize database connection
  use conn <- result.try(db.init_db(db_path))

  io.println("✅ Database connected (WAL mode)")

  // Run migrations
  io.println("🔄 Running migrations...")
  use _ <- result.try(run_migrations(conn))

  io.println("✅ Migrations complete")

  // Prepare the ytdl-sub engine layout (config, channel list, folders)
  setup_ytdl_sub()

  // Merge a dropped Google Takeout subscriptions.csv into channels.txt
  let layout = ytdl_sub.layout_from_env()
  takeout.import_and_log(layout)
  use _ <- result.try(
    channel_reconciler.reconcile(conn, layout)
    |> result.map_error(db.ConnectionError),
  )
  // Cleanup may recover a low disk, but downloads must wait for the reserve.
  use _ <- result.try(
    storage_guard.check() |> result.map_error(db.ConnectionError),
  )

  // Record library files left by a poll that was interrupted earlier: the
  // engine's archive skips them, so nothing else would ever record them
  log_reconcile(subscription_manager.reconcile_library(
    conn,
    layout.library_dir,
    get_current_timestamp(),
  ))

  plex.enrich_titles(layout.library_dir)
  plex.refresh_library()

  // Fix zombie jobs
  io.println("🧟 Checking for zombie jobs...")
  use count <- result.try(fix_zombies(conn))

  case count {
    0 -> io.println("✅ No zombie jobs found")
    n -> io.println("✅ Reset " <> int.to_string(n) <> " zombie job(s)")
  }

  io.println("🎉 Startup complete!")

  Ok(conn)
}

/// Create the directory that will hold the database file
fn ensure_database_directory(db_path: String) -> Nil {
  let directory = database_directory(db_path)

  case simplifile.create_directory_all(directory) {
    Ok(_) -> Nil
    Error(_) -> io.println("⚠️  Cannot create database directory " <> directory)
  }
}

/// Directory portion of a database path, "." when the path has none
fn database_directory(path: String) -> String {
  let parts =
    path
    |> string.split("/")
    |> list.reverse
    |> list.drop(1)
    |> list.reverse

  case string.join(parts, "/") {
    "" ->
      case string.starts_with(path, "/") {
        True -> "/"
        False -> "."
      }
    directory -> directory
  }
}

/// Prepare the ytdl-sub engine directories, config and channel list
fn setup_ytdl_sub() -> Nil {
  io.println("🎬 Preparing ytdl-sub engine layout...")

  let layout = ytdl_sub.layout_from_env()

  case ytdl_sub.ensure_layout(layout) {
    Ok(_) ->
      io.println(
        "✅ ytdl-sub ready | channels: "
        <> layout.channels_file
        <> " | library: "
        <> layout.library_dir,
      )
    Error(err) -> io.println("⚠ ytdl-sub layout setup failed: " <> err)
  }

  Nil
}

/// Report the library files recorded from an earlier interrupted poll
fn log_reconcile(recorded_and_failed: #(Int, Int)) -> Nil {
  let #(recorded, failed) = recorded_and_failed
  case recorded, failed {
    0, 0 -> Nil
    n, 0 ->
      io.println(
        "📚 Recorded "
        <> int.to_string(n)
        <> " library file(s) from an earlier poll",
      )
    n, f ->
      io.println(
        "📚 Recorded "
        <> int.to_string(n)
        <> " library file(s), "
        <> int.to_string(f)
        <> " could not be written",
      )
  }
}

/// Run database migrations (INV-002)
fn run_migrations(conn: Db) -> Result(Nil, DbError) {
  migrator.run_migrations(conn)
}

/// Fix zombie jobs - jobs stuck in 'downloading' state (INV-003)
fn fix_zombies(conn: Db) -> Result(Int, DbError) {
  let timestamp = get_current_timestamp()
  repo.reset_zombies(conn, timestamp)
}

/// Get current Unix timestamp
type TimeUnit {
  Second
}

@external(erlang, "erlang", "system_time")
fn get_system_time_seconds(unit: TimeUnit) -> Int

fn get_current_timestamp() -> Int {
  get_system_time_seconds(Second)
}
