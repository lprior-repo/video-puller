/// Tests for the manager actor
///
/// These tests verify that the manager properly:
/// - Starts successfully with valid database connection
/// - Responds to stats requests while running
/// - Continues operating despite database errors (13m.79)
/// - Shuts down cleanly so tests do not leave actors behind
import core/manager
import domain/core_types
import domain/types.{GetStats, JobStatusUpdate, Shutdown}
import engine/ytdlp
import gleam/erlang/process
import gleam/option.{Some}
import gleeunit
import gleeunit/should
import infra/db
import infra/migrator
import infra/repo
import simplifile

pub fn main() {
  gleeunit.main()
}

// ============================================================================
// Helper functions for testing
// ============================================================================

/// Create a test download config
fn test_config() -> ytdlp.DownloadConfig {
  ytdlp.DownloadConfig(
    output_directory: "./test_downloads",
    format: "best",
    max_filesize: "2G",
    audio_only: False,
    audio_format: ytdlp.BestAudio,
    allow_playlist: False,
    download_timeout_ms: 1_800_000,
    rate_limit_delay_ms: 0,
    bandwidth_limit: "",
    use_channel_folders: False,
  )
}

/// Setup a migrated test database
fn setup_test_db(path: String) -> db.Db {
  let _ = simplifile.delete(path)
  let _ = simplifile.delete(path <> "-shm")
  let _ = simplifile.delete(path <> "-wal")

  let conn = case db.init_db(path) {
    Ok(c) -> c
    Error(_) -> panic as "Failed to init test database"
  }

  case migrator.run_migrations(conn) {
    Ok(_) -> Nil
    Error(_) -> panic as "Failed to run migrations"
  }

  conn
}

/// Stop the manager and give it time to shut the worker pool down
fn stop_manager(subject) {
  process.send(subject, Shutdown)
  process.sleep(300)
}

/// Cleanup test database files
fn cleanup_test_db(path: String) -> Nil {
  let _ = simplifile.delete(path)
  let _ = simplifile.delete(path <> "-shm")
  let _ = simplifile.delete(path <> "-wal")
  Nil
}

// ============================================================================
// Test: Manager starts successfully with valid database (basic sanity check)
// ============================================================================

pub fn manager_start_test() {
  let db_path = "/tmp/test_manager_start.db"
  let conn = setup_test_db(db_path)

  case manager.start(conn, test_config(), 5000, 3) {
    Ok(subject) -> {
      let stats_subject = process.new_subject()
      process.send(subject, GetStats(stats_subject))

      case process.receive(stats_subject, 1000) {
        Ok(_) -> Nil
        Error(_) -> should.fail()
      }

      stop_manager(subject)
    }
    Error(_) -> should.fail()
  }

  let _ = db.close(conn)
  cleanup_test_db(db_path)
}

// ============================================================================
// Test: Manager keeps operating when database work fails (13m.79)
// ============================================================================
//
// LIMITATION: closing a SQLite connection underneath the manager causes BEAM
// badarg errors inside the esqlite3 NIF rather than a graceful Result error,
// so a truly disconnected database cannot be driven from a test process.
//
// WHAT IS TESTED: the manager stays responsive after its database has been
// closed, which exercises the poll_and_dispatch error path
// (core/manager.gleam: case repo.list_jobs(...) { Error(_) -> state }) without
// crashing the actor.
// ============================================================================

pub fn manager_handles_database_failure_test() {
  let db_path = "/tmp/test_manager_db_resilience.db"
  let conn = setup_test_db(db_path)

  case manager.start(conn, test_config(), 5000, 3) {
    Ok(subject) -> {
      process.sleep(100)

      // Manager is operational before the failure
      let stats_subject = process.new_subject()
      process.send(subject, GetStats(stats_subject))
      process.receive(stats_subject, 1000) |> should.be_ok()

      // Force a database error by closing the connection behind the manager
      let _ = db.close(conn)
      process.sleep(100)

      // Manager must still answer after the database work fails
      let after_failure = process.new_subject()
      process.send(subject, GetStats(after_failure))

      case process.receive(after_failure, 1000) {
        Ok(_) -> Nil
        Error(_) -> should.fail()
      }

      stop_manager(subject)
    }
    Error(_) -> should.fail()
  }

  cleanup_test_db(db_path)
}

// ============================================================================
// Test: Manager stats are accessible and correct
// ============================================================================

pub fn manager_stats_test() {
  let db_path = "/tmp/test_manager_stats.db"
  let conn = setup_test_db(db_path)

  case manager.start(conn, test_config(), 5000, 3) {
    Ok(subject) -> {
      process.sleep(100)

      let stats_subject = process.new_subject()
      process.send(subject, GetStats(stats_subject))

      case process.receive(stats_subject, 1000) {
        Ok(stats) -> {
          // No jobs yet, so no dispatches or polls have happened
          stats.total_dispatched |> should.equal(0)
          stats.total_completed |> should.equal(0)
          stats.total_failed |> should.equal(0)
          stats.polls_executed |> should.equal(0)
        }
        Error(_) -> should.fail()
      }

      stop_manager(subject)
    }
    Error(_) -> should.fail()
  }

  let _ = db.close(conn)
  cleanup_test_db(db_path)
}

// ============================================================================
// Test: completion persists the exact reported media path
// ============================================================================

pub fn manager_records_reported_media_path_test() {
  let db_path = "/tmp/test_manager_reported_path.db"
  let conn = setup_test_db(db_path)
  let job_id = core_types.new_job_id("job-without-media-prefix")
  let media_path = "/tmp/media-title-channel-2026.mp4"

  repo.insert_job(conn, job_id, "https://example.com/video", 1000)
  |> should.be_ok()
  simplifile.write(media_path, "media") |> should.be_ok()

  case manager.start(conn, test_config(), 5000, 1) {
    Ok(subject) -> {
      process.send(
        subject,
        JobStatusUpdate(job_id, core_types.Completed, Some(media_path)),
      )
      process.sleep(100)

      case repo.get_job(conn, job_id) {
        Ok(Some(job)) -> {
          job.path |> should.equal(Some(media_path))
          job.status |> should.equal(core_types.Completed)
        }
        _ -> should.fail()
      }

      stop_manager(subject)
    }
    Error(_) -> should.fail()
  }

  let _ = simplifile.delete(media_path)
  let _ = db.close(conn)
  cleanup_test_db(db_path)
}
