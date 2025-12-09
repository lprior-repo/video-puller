/// Application startup sequence
///
/// Handles critical startup operations:
/// - Running database migrations (INV-002: must run before app start)
/// - Fixing zombie jobs (INV-003: revert processing jobs to pending)
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
fn get_current_timestamp() -> Int {
  // Get system time in seconds
  system_time_seconds()
}

@external(erlang, "os", "system_time")
fn system_time_seconds() -> Int
