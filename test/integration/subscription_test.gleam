/// Integration tests for the subscription system
///
/// Tests the complete subscription flow including:
/// - Subscription manager actor lifecycle
/// - Config updates and persistence
/// - Video filtering and job creation
/// - Status reporting
import core/subscription_manager
import domain/subscription_types.{
  type DiscoveredVideo, Chromium, DiscoveredVideo, PassedFilter,
  SkippedExcludedKeyword, SkippedNoKeywordMatch, SkippedTooOld, SkippedTooShort,
  SubscriptionConfig,
}
import engine/video_filter
import gleam/erlang/process
import gleam/option.{None, Some}
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
  duration: option.Option(Int),
  age_days: Int,
) -> DiscoveredVideo {
  let now = get_timestamp()
  let published = now - age_days * 86_400

  DiscoveredVideo(
    video_id: id,
    channel_id: Some("channel-123"),
    channel_name: Some("Test Channel"),
    title: title,
    url: "https://www.youtube.com/watch?v=" <> id,
    published_at: Some(published),
    duration_seconds: duration,
    thumbnail_url: Some("https://img.youtube.com/" <> id),
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

/// Test that subscription manager starts correctly
pub fn subscription_manager_start_test() {
  let conn = setup_test_db("manager_start")

  case subscription_manager.start(conn) {
    Ok(subject) -> {
      // Manager should start successfully
      // Send a status request to verify it's alive
      let reply = process.new_subject()
      process.send(subject, subscription_manager.GetStatus(reply))

      case process.receive(reply, 1000) {
        Ok(status) -> {
          // Default config should have subscriptions disabled
          status.enabled |> should.be_false()
          status.is_polling |> should.be_false()
        }
        Error(_) -> should.fail()
      }

      // Shutdown the manager
      process.send(subject, subscription_manager.Shutdown)
    }
    Error(_) -> should.fail()
  }

  cleanup_test_db(conn, "manager_start")
}

/// Test that config updates are propagated to the manager
pub fn subscription_manager_config_update_test() {
  let conn = setup_test_db("manager_config")

  case subscription_manager.start(conn) {
    Ok(subject) -> {
      // Create a new config
      let new_config =
        SubscriptionConfig(
          enabled: True,
          poll_interval_minutes: 30,
          browser: Chromium,
          cookies_path: None,
          max_age_days: 3,
          min_duration_seconds: 60,
          max_duration_seconds: Some(3600),
          keyword_filter: ["tutorial", "guide"],
          keyword_exclude: ["shorts", "ad"],
          last_poll_at: None,
        )

      // Send config update
      process.send(subject, subscription_manager.UpdateConfig(new_config))

      // Give it a moment to process
      process.sleep(100)

      // Check status
      let reply = process.new_subject()
      process.send(subject, subscription_manager.GetStatus(reply))

      case process.receive(reply, 1000) {
        Ok(status) -> {
          status.enabled |> should.be_true()
        }
        Error(_) -> should.fail()
      }

      process.send(subject, subscription_manager.Shutdown)
    }
    Error(_) -> should.fail()
  }

  cleanup_test_db(conn, "manager_config")
}

// =============================================================================
// Video Filter Integration Tests
// =============================================================================

/// Test complete video filtering flow
pub fn video_filter_integration_test() {
  let config =
    SubscriptionConfig(
      enabled: True,
      poll_interval_minutes: 60,
      browser: Chromium,
      cookies_path: None,
      max_age_days: 7,
      min_duration_seconds: 120,
      max_duration_seconds: Some(7200),
      keyword_filter: ["tutorial", "guide", "how to"],
      keyword_exclude: ["shorts", "ad", "sponsored"],
      last_poll_at: None,
    )

  let now = get_timestamp()

  // Video that should pass all filters
  let good_video =
    test_video("good1", "How to Build a Web App Tutorial", Some(1800), 2)

  case video_filter.should_download(good_video, config, None, now) {
    PassedFilter -> Nil
    _other -> {
      should.fail()
    }
  }

  // Video too short
  let short_video = test_video("short1", "Quick Tutorial Guide", Some(60), 1)

  case video_filter.should_download(short_video, config, None, now) {
    SkippedTooShort -> Nil
    _ -> should.fail()
  }

  // Video too old
  let old_video = test_video("old1", "Old Tutorial Guide", Some(600), 14)

  case video_filter.should_download(old_video, config, None, now) {
    SkippedTooOld -> Nil
    _ -> should.fail()
  }

  // Video with excluded keyword
  let sponsored_video =
    test_video("sponsored1", "Sponsored Tutorial Guide", Some(600), 1)

  case video_filter.should_download(sponsored_video, config, None, now) {
    SkippedExcludedKeyword(_) -> Nil
    _ -> should.fail()
  }

  // Video without any keywords
  let no_match_video =
    test_video("nomatch1", "Random Video Content", Some(600), 1)

  case video_filter.should_download(no_match_video, config, None, now) {
    SkippedNoKeywordMatch -> Nil
    _ -> should.fail()
  }
}

// =============================================================================
// Subscription Repo Integration Tests
// =============================================================================

/// Test seen video tracking and job creation
pub fn seen_video_tracking_test() {
  let conn = setup_test_db("seen_tracking")

  let video = test_video("track1", "Test Video Tutorial", Some(600), 1)
  let timestamp = get_timestamp()

  // Mark as seen
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

  // Check it's now seen
  case subscription_repo.is_seen(conn, "track1") {
    Ok(True) -> Nil
    _ -> should.fail()
  }

  // Retrieve the seen video
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

/// Test skipped video tracking
pub fn skipped_video_tracking_test() {
  let conn = setup_test_db("skip_tracking")

  let video = test_video("skip1", "Too Short Video", Some(30), 1)
  let timestamp = get_timestamp()

  // Mark as skipped
  case
    subscription_repo.mark_seen(
      conn,
      video,
      False,
      Some("Video too short"),
      None,
      timestamp,
    )
  {
    Ok(_) -> Nil
    Error(_) -> should.fail()
  }

  // Retrieve and verify
  case subscription_repo.get_seen_video(conn, "skip1") {
    Ok(Some(seen)) -> {
      seen.video_id |> should.equal("skip1")
      seen.downloaded |> should.be_false()
      seen.skipped |> should.be_true()
      seen.skip_reason |> should.equal(Some("Video too short"))
    }
    _ -> should.fail()
  }

  cleanup_test_db(conn, "skip_tracking")
}

/// Test config persistence
pub fn config_persistence_test() {
  let conn = setup_test_db("config_persist")
  let timestamp = get_timestamp()

  let config =
    SubscriptionConfig(
      enabled: True,
      poll_interval_minutes: 45,
      browser: Chromium,
      cookies_path: Some("/path/to/cookies.txt"),
      max_age_days: 5,
      min_duration_seconds: 180,
      max_duration_seconds: Some(5400),
      keyword_filter: ["gleam", "erlang", "beam"],
      keyword_exclude: ["java", "python"],
      last_poll_at: Some(timestamp - 3600),
    )

  // Save config
  case subscription_repo.update_config(conn, config, timestamp) {
    Ok(_) -> Nil
    Error(_) -> should.fail()
  }

  // Retrieve and verify
  case subscription_repo.get_config(conn) {
    Ok(loaded) -> {
      loaded.enabled |> should.be_true()
      loaded.poll_interval_minutes |> should.equal(45)
      loaded.max_age_days |> should.equal(5)
      loaded.min_duration_seconds |> should.equal(180)
      loaded.keyword_filter |> should.equal(["gleam", "erlang", "beam"])
      loaded.keyword_exclude |> should.equal(["java", "python"])
    }
    Error(_) -> should.fail()
  }

  cleanup_test_db(conn, "config_persist")
}

/// Test job creation from subscription
pub fn job_from_subscription_test() {
  let conn = setup_test_db("job_create")

  let video = test_video("job1", "Tutorial on BEAM", Some(1200), 1)
  let timestamp = get_timestamp()

  // Create a job ID
  let job_id = "sub-job-" <> video.video_id

  // Insert the job
  case
    repo.insert_job(conn, core_types.new_job_id(job_id), video.url, timestamp)
  {
    Ok(_) -> Nil
    Error(_) -> should.fail()
  }

  // Mark video as seen with job reference
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

  // Verify job exists
  case repo.get_job(conn, core_types.new_job_id(job_id)) {
    Ok(Some(job)) -> {
      job.url |> should.equal(video.url)
    }
    _ -> should.fail()
  }

  cleanup_test_db(conn, "job_create")
}

// =============================================================================
// End-to-End Flow Tests
// =============================================================================

/// Test complete subscription poll simulation
pub fn complete_poll_simulation_test() {
  let conn = setup_test_db("poll_sim")

  let config =
    SubscriptionConfig(
      enabled: True,
      poll_interval_minutes: 60,
      browser: Chromium,
      cookies_path: None,
      max_age_days: 7,
      min_duration_seconds: 120,
      max_duration_seconds: None,
      keyword_filter: [],
      keyword_exclude: ["shorts"],
      last_poll_at: None,
    )

  let timestamp = get_timestamp()

  // Simulate discovered videos
  let videos = [
    test_video("poll1", "Great Video Content", Some(600), 1),
    test_video("poll2", "Another Good Video", Some(900), 2),
    test_video("poll3", "Shorts Video Quick", Some(30), 1),
    test_video("poll4", "Old Video Content", Some(600), 14),
  ]

  // Process each video like the subscription manager would
  let _results =
    videos
    |> list.map(fn(video) {
      let filter_result =
        video_filter.should_download(video, config, None, timestamp)

      case filter_result {
        PassedFilter -> {
          let job_id = "poll-job-" <> video.video_id
          let _ =
            repo.insert_job(
              conn,
              core_types.new_job_id(job_id),
              video.url,
              timestamp,
            )
          let _ =
            subscription_repo.mark_seen(
              conn,
              video,
              True,
              None,
              Some(job_id),
              timestamp,
            )
          #(video.video_id, "queued")
        }
        other -> {
          let reason = subscription_types.filter_result_to_string(other)
          let _ =
            subscription_repo.mark_seen(
              conn,
              video,
              False,
              Some(reason),
              None,
              timestamp,
            )
          #(video.video_id, "skipped: " <> reason)
        }
      }
    })

  // Verify results
  // poll1 and poll2 should be queued (pass all filters with empty keyword_filter)
  // poll3 should be skipped (too short)
  // poll4 should be skipped (too old)

  // Check jobs were created for poll1 and poll2
  case repo.list_jobs(conn, None) {
    Ok(jobs) -> {
      list.length(jobs) |> should.equal(2)
    }
    Error(_) -> should.fail()
  }

  // Check all videos are marked as seen
  case subscription_repo.list_seen_videos(conn, 10, 0) {
    Ok(seen) -> {
      list.length(seen) |> should.equal(4)
    }
    Error(_) -> should.fail()
  }

  cleanup_test_db(conn, "poll_sim")
}

import domain/core_types
import gleam/list
