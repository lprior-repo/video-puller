/// Application startup sequence
///
/// Handles critical startup operations:
/// - Preparing the ytdl-sub engine layout for subscription pulls
/// - Running database migrations (INV-002: must run before app start)
/// - Fixing zombie jobs (INV-003: revert processing jobs to pending)
import engine/ytdl_sub
import envoy
import gleam/int
import gleam/io
import gleam/result
import infra/db.{type Db, type DbError}
import infra/migrator
import infra/repo

/// Run the complete startup sequence
pub fn initialize() -> Result(Db, DbError) {
  io.println("🚀 Starting FractalVideoEater...")

  // Get database path from environment or use default
  let db_path = case envoy.get("DB_PATH") {
    Ok(path) -> path
    Error(_) -> "./data/video_eater.db"
  }

  io.println("📁 Database: " <> db_path)

  // Initialize database connection
  use conn <- result.try(db.init_db(db_path))

  io.println("✅ Database connected (WAL mode)")

  // Run migrations
  io.println("🔄 Running migrations...")
  use _ <- result.try(run_migrations(conn))

  io.println("✅ Migrations complete")

  // Prepare the ytdl-sub engine layout (config, channel list, folders)
  setup_ytdl_sub()

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
