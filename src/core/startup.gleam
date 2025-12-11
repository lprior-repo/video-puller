/// Application startup sequence
///
/// Handles critical startup operations:
/// - Setting up YouTube cookie access (keyring unlock, dependency check)
/// - Running database migrations (INV-002: must run before app start)
/// - Fixing zombie jobs (INV-003: revert processing jobs to pending)
import engine/shell
import envoy
import gleam/int
import gleam/io
import gleam/result
import gleam/string
import infra/db.{type Db, type DbError}
import infra/migrator
import infra/repo

/// Run the complete startup sequence
pub fn initialize() -> Result(Db, DbError) {
  io.println("🚀 Starting FractalVideoEater...")

  // Setup YouTube cookie access (keyring, dependencies)
  io.println("🔐 Setting up YouTube cookie access...")
  setup_youtube_cookies()

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

/// Setup YouTube cookie access for subscription downloads
/// - Unlocks GNOME keyring if locked (needed for Chromium cookie decryption)
/// - Checks for pycryptodomex dependency
fn setup_youtube_cookies() -> Nil {
  // Check and install pycryptodomex if needed
  case check_pycryptodomex() {
    True -> io.println("   ✓ pycryptodomex available")
    False -> {
      io.println("   ⚠ pycryptodomex not found, installing...")
      case install_pycryptodomex() {
        True -> io.println("   ✓ pycryptodomex installed")
        False ->
          io.println(
            "   ⚠ Could not install pycryptodomex (cookie decryption may fail)",
          )
      }
    }
  }

  // Unlock GNOME keyring if available and locked
  case unlock_keyring() {
    KeyringUnlocked -> io.println("   ✓ Keyring unlocked")
    KeyringAlreadyUnlocked -> io.println("   ✓ Keyring already unlocked")
    KeyringNotAvailable -> io.println("   ℹ Keyring not available (non-Linux?)")
    KeyringUnlockFailed(reason) ->
      io.println("   ⚠ Keyring unlock failed: " <> reason)
  }

  Nil
}

/// Result of keyring unlock attempt
type KeyringResult {
  KeyringUnlocked
  KeyringAlreadyUnlocked
  KeyringNotAvailable
  KeyringUnlockFailed(String)
}

/// Check if pycryptodomex is installed
fn check_pycryptodomex() -> Bool {
  let script =
    "import sys; import importlib.util; sys.exit(0 if importlib.util.find_spec('Cryptodome') else 1)"
  case shell.run("python3", ["-c", script]) {
    Ok(result) -> result.exit_code == 0
    Error(_) -> False
  }
}

/// Install pycryptodomex via pip
fn install_pycryptodomex() -> Bool {
  case shell.run("pip", ["install", "--quiet", "pycryptodomex"]) {
    Ok(result) -> result.exit_code == 0
    Error(_) -> False
  }
}

/// Unlock the GNOME keyring for cookie decryption
fn unlock_keyring() -> KeyringResult {
  // Try gnome-keyring-daemon first (most reliable for automated unlock)
  case unlock_via_daemon() {
    Ok(_) -> KeyringUnlocked
    Error(_) -> {
      // Fallback to secretstorage Python approach
      case unlock_via_secretstorage() {
        Ok(result) -> result
        Error(msg) -> KeyringUnlockFailed(msg)
      }
    }
  }
}

/// Unlock keyring using gnome-keyring-daemon --unlock
/// This is the most reliable method for automated/headless unlock
fn unlock_via_daemon() -> Result(Nil, String) {
  // Use bash to pipe empty password to gnome-keyring-daemon
  // This unlocks the login keyring if it has an empty password or auto-unlocks
  let script =
    "echo '' | gnome-keyring-daemon --unlock --components=secrets 2>/dev/null"
  case shell.run("bash", ["-c", script]) {
    Ok(result) ->
      case result.exit_code {
        0 -> Ok(Nil)
        _ ->
          Error("gnome-keyring-daemon returned " <> string.trim(result.stderr))
      }
    Error(shell.ExecutionError(msg)) -> Error(msg)
    Error(shell.InvalidCommand(msg)) -> Error(msg)
  }
}

/// Fallback: unlock via Python secretstorage
fn unlock_via_secretstorage() -> Result(KeyringResult, String) {
  let script =
    "
import sys
try:
    import secretstorage
    bus = secretstorage.dbus_init()
    collection = secretstorage.get_default_collection(bus)
    if collection.is_locked():
        collection.unlock()
        print('unlocked')
    else:
        print('already_unlocked')
    sys.exit(0)
except ImportError:
    print('no_secretstorage')
    sys.exit(1)
except Exception as e:
    print(f'error:{e}')
    sys.exit(2)
"

  case shell.run("python3", ["-c", script]) {
    Ok(result) -> {
      let output = string.trim(result.stdout)
      case result.exit_code {
        0 ->
          case output {
            "unlocked" -> Ok(KeyringUnlocked)
            "already_unlocked" -> Ok(KeyringAlreadyUnlocked)
            "no_secretstorage" -> Ok(KeyringNotAvailable)
            other -> Ok(KeyringUnlockFailed(other))
          }
        1 -> Ok(KeyringNotAvailable)
        _ -> Error(output)
      }
    }
    Error(shell.ExecutionError(msg)) -> Error(msg)
    Error(shell.InvalidCommand(msg)) -> Error(msg)
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
