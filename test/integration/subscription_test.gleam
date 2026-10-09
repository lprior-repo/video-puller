/// Integration tests for the subscription system
///
/// Covers the retained flow: the manager actor lifecycle, config persistence,
/// library records derived from engine output, and the direct-download job
/// queue.
import core/subscription_manager
import domain/core_types
import domain/subscription_types.{SubscriptionConfig}
import engine/ytdl_sub
import gleam/erlang/process
import gleam/list
import gleam/option.{type Option, None, Some}
import gleeunit
import gleeunit/should
import infra/db
import infra/migrator
import infra/repo
import infra/subscription_repo
import simplifile

pub fn main() {
  gleeunit.main()
}

// =============================================================================
// Test Helpers
// =============================================================================

fn setup_test_db(test_name: String) -> db.Db {
  let test_db = "/tmp/test_subscription_" <> test_name <> ".db"
  let _ = simplifile.delete(test_db)

  let conn = case db.init_db(test_db) {
    Ok(c) -> c
    Error(_) -> panic as "Failed to init test database"
  }

  case migrator.run_migrations(conn) {
    Ok(_) -> Nil
    Error(_) -> panic as "Failed to run migrations"
  }

  conn
}

fn cleanup_test_db(conn: db.Db, test_name: String) {
  let _ = db.close(conn)
  let test_db = "/tmp/test_subscription_" <> test_name <> ".db"
  let _ = simplifile.delete(test_db)
}

fn test_video(
  id: String,
  title: String,
  duration: Option(Int),
) -> subscription_types.DiscoveredVideo {
  subscription_types.DiscoveredVideo(
    video_id: id,
    channel_id: None,
    channel_name: Some("Test Channel"),
    title: title,
    url: "file:///library/Test Channel/" <> id <> ".mp4",
    published_at: None,
    duration_seconds: duration,
    thumbnail_url: None,
  )
}

/// Get current Unix timestamp in seconds
fn get_timestamp() -> Int {
  get_system_time(Second)
}

type TimeUnit {
  Second
}

@external(erlang, "erlang", "system_time")
fn get_system_time(unit: TimeUnit) -> Int

// =============================================================================
// Subscription Manager Tests
// =============================================================================

/// Test that the subscription manager starts and reports status
pub fn subscription_manager_start_test() {
  let conn = setup_test_db("manager_start")

  case subscription_manager.start(conn) {
    Ok(subject) -> {
      let reply = process.new_subject()
      process.send(subject, subscription_manager.GetStatus(reply))

      case process.receive(reply, 1000) {
        Ok(status) -> {
          // Default config has subscriptions disabled
          status.enabled |> should.be_false()
          status.is_polling |> should.be_false()
        }
        Error(_) -> should.fail()
      }

      process.send(subject, subscription_manager.Shutdown)
      process.sleep(50)
    }
    Error(_) -> should.fail()
  }

  cleanup_test_db(conn, "manager_start")
}

/// Test that config updates are propagated to the manager and persisted
pub fn subscription_manager_config_update_test() {
  let conn = setup_test_db("manager_config")

  case subscription_manager.start(conn) {
    Ok(subject) -> {
      let new_config =
        SubscriptionConfig(
          enabled: True,
          poll_interval_minutes: 360,
          last_poll_at: None,
        )

      process.send(subject, subscription_manager.UpdateConfig(new_config))
      process.sleep(100)

      let reply = process.new_subject()
      process.send(subject, subscription_manager.GetStatus(reply))

      case process.receive(reply, 1000) {
        Ok(status) -> status.enabled |> should.be_true()
        Error(_) -> should.fail()
      }

      case subscription_repo.get_config(conn) {
        Ok(persisted) -> persisted.enabled |> should.be_true()
        Error(_) -> should.fail()
      }

      process.send(subject, subscription_manager.Shutdown)
      process.sleep(50)
    }
    Error(_) -> should.fail()
  }

  cleanup_test_db(conn, "manager_config")
}

/// Test config persistence roundtrip
pub fn config_persistence_test() {
  let conn = setup_test_db("config_persist")
  let timestamp = get_timestamp()

  let config =
    SubscriptionConfig(
      enabled: True,
      poll_interval_minutes: 720,
      last_poll_at: Some(timestamp - 3600),
    )

  case subscription_repo.update_config(conn, config, timestamp) {
    Ok(_) -> Nil
    Error(_) -> should.fail()
  }

  case subscription_repo.get_config(conn) {
    Ok(loaded) -> {
      loaded.enabled |> should.be_true()
      loaded.poll_interval_minutes |> should.equal(720)
      loaded.last_poll_at |> should.equal(Some(timestamp - 3600))
    }
    Error(_) -> should.fail()
  }

  cleanup_test_db(conn, "config_persist")
}

// =============================================================================
// Seen Video Tracking Tests
// =============================================================================

/// Test download record tracking
pub fn seen_video_tracking_test() {
  let conn = setup_test_db("seen_tracking")

  let video = test_video("track1", "Test Video", Some(600))
  let timestamp = get_timestamp()

  case
    subscription_repo.mark_seen(
      conn,
      video,
      True,
      None,
      Some("job-123"),
      timestamp,
    )
  {
    Ok(_) -> Nil
    Error(_) -> should.fail()
  }

  case subscription_repo.is_seen(conn, "track1") {
    Ok(True) -> Nil
    _ -> should.fail()
  }

  case subscription_repo.get_seen_video(conn, "track1") {
    Ok(Some(seen)) -> {
      seen.video_id |> should.equal("track1")
      seen.downloaded |> should.be_true()
      seen.job_id |> should.equal(Some("job-123"))
    }
    _ -> should.fail()
  }

  cleanup_test_db(conn, "seen_tracking")
}

/// Test that re-recording the same video does not duplicate rows
pub fn mark_seen_idempotency_test() {
  let conn = setup_test_db("seen_idempotent")

  let video = test_video("idem1", "Repeat Video", Some(600))
  let timestamp = get_timestamp()

  case subscription_repo.mark_seen(conn, video, True, None, None, timestamp) {
    Ok(_) -> Nil
    Error(_) -> should.fail()
  }

  case
    subscription_repo.mark_seen(conn, video, True, None, None, timestamp + 60)
  {
    Ok(_) -> Nil
    Error(_) -> should.fail()
  }

  case subscription_repo.count_seen_videos(conn) {
    Ok(count) -> count |> should.equal(1)
    Error(_) -> should.fail()
  }

  cleanup_test_db(conn, "seen_idempotent")
}

// =============================================================================
// Engine Output Mapping Tests
// =============================================================================

/// Test that engine library files map to library records
pub fn engine_added_files_recorded_test() {
  let conn = setup_test_db("engine_records")
  let library_dir = "/tmp/test_subscription_library"

  let _ = simplifile.create_directory_all(library_dir <> "/Chan A")
  let _ = simplifile.create_directory_all(library_dir <> "/Chan B")

  let video_one = library_dir <> "/Chan A/s2026.e091101 - Video One.mp4"
  let sidecar_one = library_dir <> "/Chan A/s2026.e091101 - Video One.info.json"
  let video_two = library_dir <> "/Chan B/Video Two.mkv"
  let thumb_two = library_dir <> "/Chan B/Video Two.jpg"

  let _ = simplifile.write(video_one, "video")
  let _ = simplifile.write(sidecar_one, "{}")
  let _ = simplifile.write(video_two, "video")
  let _ = simplifile.write(thumb_two, "image")

  // Only the media files are reported as downloads
  let added_files = [video_one, video_two]
  let timestamp = get_timestamp()

  list.each(added_files, fn(path) {
    let video = ytdl_sub.to_discovered_video(library_dir, path)
    let _ =
      subscription_repo.mark_seen(conn, video, True, None, None, timestamp)
    Nil
  })

  case subscription_repo.count_downloaded(conn) {
    Ok(count) -> count |> should.equal(2)
    Error(_) -> should.fail()
  }

  case
    subscription_repo.get_seen_video(
      conn,
      "Chan A/s2026.e091101 - Video One.mp4",
    )
  {
    Ok(Some(seen)) -> {
      seen.channel_name |> should.equal(Some("Chan A"))
      seen.title |> should.equal("Video One")
      // 2026-09-11 as a Unix timestamp
      seen.published_at |> should.equal(Some(1_789_084_800))
    }
    _ -> should.fail()
  }

  case subscription_repo.get_seen_video(conn, "Chan B/Video Two.mkv") {
    Ok(Some(seen)) -> {
      seen.title |> should.equal("Video Two")
      seen.duration_seconds |> should.equal(None)
    }
    _ -> should.fail()
  }

  // Re-recording the same files leaves the library size unchanged
  list.each(added_files, fn(path) {
    let video = ytdl_sub.to_discovered_video(library_dir, path)
    let _ =
      subscription_repo.mark_seen(conn, video, True, None, None, timestamp + 60)
    Nil
  })

  case subscription_repo.list_seen_videos(conn, 50, 0) {
    Ok(seen) -> list.length(seen) |> should.equal(2)
    Error(_) -> should.fail()
  }

  let _ = simplifile.delete(video_one)
  let _ = simplifile.delete(sidecar_one)
  let _ = simplifile.delete(video_two)
  let _ = simplifile.delete(thumb_two)
  let _ = simplifile.delete(library_dir <> "/Chan A")
  let _ = simplifile.delete(library_dir <> "/Chan B")
  let _ = simplifile.delete(library_dir)

  cleanup_test_db(conn, "engine_records")
}

// =============================================================================
// Direct Download Job Queue Tests
// =============================================================================

/// Test that a manual download job can reference a library record
pub fn job_from_subscription_test() {
  let conn = setup_test_db("job_create")

  let video = test_video("job1", "Tutorial on BEAM", Some(1200))
  let timestamp = get_timestamp()

  let job_id = "sub-job-" <> video.video_id

  case
    repo.insert_job(conn, core_types.new_job_id(job_id), video.url, timestamp)
  {
    Ok(_) -> Nil
    Error(_) -> should.fail()
  }

  case
    subscription_repo.mark_seen(
      conn,
      video,
      True,
      None,
      Some(job_id),
      timestamp,
    )
  {
    Ok(_) -> Nil
    Error(_) -> should.fail()
  }

  case repo.get_job(conn, core_types.new_job_id(job_id)) {
    Ok(Some(job)) -> job.url |> should.equal(video.url)
    _ -> should.fail()
  }

  cleanup_test_db(conn, "job_create")
}
