/// Subscription Integration Tests
///
/// Tests the complete subscription auto-download workflow.
import domain/subscription_types.{
  type DiscoveredVideo, type SubscriptionConfig, DiscoveredVideo, PassedFilter,
  SkippedExcludedKeyword, SkippedTooOld, SkippedTooShort,
}
import engine/video_filter
import gleam/list
import gleam/option.{None, Some}
import gleeunit
import gleeunit/should
import infra/db
import infra/migrator
import infra/subscription_repo
import simplifile

pub fn main() {
  gleeunit.main()
}

// ============================================================================
// Test Fixtures
// ============================================================================

const test_timestamp = 1_705_276_800

fn default_test_config() -> SubscriptionConfig {
  subscription_types.SubscriptionConfig(
    enabled: True,
    poll_interval_minutes: 60,
    browser: subscription_types.Firefox,
    cookies_path: None,
    max_age_days: 7,
    min_duration_seconds: 120,
    max_duration_seconds: None,
    keyword_filter: [],
    keyword_exclude: [],
    last_poll_at: None,
  )
}

fn test_video(
  video_id: String,
  title: String,
  duration: option.Option(Int),
  published_at: option.Option(Int),
) -> DiscoveredVideo {
  DiscoveredVideo(
    video_id: video_id,
    channel_id: Some("UC_test123"),
    channel_name: Some("Test Channel"),
    title: title,
    url: "https://www.youtube.com/watch?v=" <> video_id,
    published_at: published_at,
    duration_seconds: duration,
    thumbnail_url: None,
  )
}

fn setup_test_db(name: String) {
  let test_db = "/tmp/test_sub_integration_" <> name <> ".db"
  let _ = simplifile.delete(test_db)

  let conn = db.init_db(test_db) |> should.be_ok()
  migrator.run_migrations(conn) |> should.be_ok()

  #(conn, test_db)
}

fn cleanup(conn, path) {
  let _ = db.close(conn)
  let _ = simplifile.delete(path)
}

// ============================================================================
// Full Workflow Tests
// ============================================================================

/// Test the complete subscription polling workflow:
/// 1. Configure subscription settings
/// 2. Simulate feed with multiple videos
/// 3. Filter videos based on configuration
/// 4. Track seen videos
/// 5. Verify correct queuing behavior
pub fn subscription_workflow_test() {
  let #(conn, path) = setup_test_db("workflow")

  // 1. Setup config with 2-min minimum (skip shorts)
  let config = default_test_config()
  subscription_repo.update_config(conn, config, test_timestamp)
  |> should.be_ok()

  // 2. Simulate feed with various videos
  let videos = [
    // Good video - should pass
    test_video(
      "good_video",
      "Great Tutorial",
      Some(600),
      Some(test_timestamp - 86_400),
    ),
    // Short video (YouTube Short) - should be skipped
    test_video("short_vid", "Quick Tip", Some(45), Some(test_timestamp - 3600)),
    // Old video - should be skipped
    test_video(
      "old_vid",
      "Old Content",
      Some(600),
      Some(test_timestamp - { 10 * 86_400 }),
    ),
    // Another good video
    test_video(
      "good_video2",
      "Another Great Video",
      Some(1200),
      Some(test_timestamp - 43_200),
    ),
  ]

  // 3. Process each video through the filter and mark as seen
  let results =
    list.map(videos, fn(video) {
      // Check if already seen
      let is_seen =
        subscription_repo.is_seen(conn, video.video_id) |> should.be_ok()

      case is_seen {
        True -> #(video, subscription_types.SkippedAlreadySeen)
        False -> {
          // Apply filters
          let filter_result =
            video_filter.should_download(video, config, None, test_timestamp)

          // Mark as seen based on result
          case filter_result {
            PassedFilter -> {
              subscription_repo.mark_seen(
                conn,
                video,
                True,
                None,
                Some("job-" <> video.video_id),
                test_timestamp,
              )
              |> should.be_ok()
            }
            _ -> {
              subscription_repo.mark_seen(
                conn,
                video,
                False,
                Some(subscription_types.filter_result_to_string(filter_result)),
                None,
                test_timestamp,
              )
              |> should.be_ok()
            }
          }

          #(video, filter_result)
        }
      }
    })

  // 4. Verify results
  // Should have 2 passed, 1 too short, 1 too old
  let passed = list.filter(results, fn(r) { r.1 == PassedFilter })
  list.length(passed) |> should.equal(2)

  let too_short = list.filter(results, fn(r) { r.1 == SkippedTooShort })
  list.length(too_short) |> should.equal(1)

  let too_old = list.filter(results, fn(r) { r.1 == SkippedTooOld })
  list.length(too_old) |> should.equal(1)

  // 5. Verify seen videos in database
  subscription_repo.count_seen_videos(conn)
  |> should.be_ok()
  |> should.equal(4)

  subscription_repo.count_downloaded(conn)
  |> should.be_ok()
  |> should.equal(2)

  // 6. Verify specific videos
  let good_video =
    subscription_repo.get_seen_video(conn, "good_video") |> should.be_ok()
  case good_video {
    Some(v) -> {
      v.downloaded |> should.be_true()
      v.job_id |> should.equal(Some("job-good_video"))
    }
    None -> should.fail()
  }

  let short_video =
    subscription_repo.get_seen_video(conn, "short_vid") |> should.be_ok()
  case short_video {
    Some(v) -> {
      v.downloaded |> should.be_false()
      v.skipped |> should.be_true()
      v.skip_reason |> should.equal(Some("Video too short"))
    }
    None -> should.fail()
  }

  cleanup(conn, path)
}

/// Test that duplicate videos are not processed twice
pub fn duplicate_video_detection_test() {
  let #(conn, path) = setup_test_db("duplicates")

  let config = default_test_config()
  subscription_repo.update_config(conn, config, test_timestamp)
  |> should.be_ok()

  let video =
    test_video(
      "dup_vid",
      "Duplicate Test",
      Some(600),
      Some(test_timestamp - 3600),
    )

  // First poll - should be processed
  let is_seen1 = subscription_repo.is_seen(conn, "dup_vid") |> should.be_ok()
  is_seen1 |> should.be_false()

  subscription_repo.mark_seen(
    conn,
    video,
    True,
    None,
    Some("job-1"),
    test_timestamp,
  )
  |> should.be_ok()

  // Second poll - should be detected as seen
  let is_seen2 = subscription_repo.is_seen(conn, "dup_vid") |> should.be_ok()
  is_seen2 |> should.be_true()

  // Should only have one entry
  subscription_repo.count_seen_videos(conn)
  |> should.be_ok()
  |> should.equal(1)

  cleanup(conn, path)
}

/// Test keyword filtering integration
pub fn keyword_filter_integration_test() {
  let #(conn, path) = setup_test_db("keywords")

  let config =
    subscription_types.SubscriptionConfig(
      enabled: True,
      poll_interval_minutes: 60,
      browser: subscription_types.Firefox,
      cookies_path: None,
      max_age_days: 7,
      min_duration_seconds: 120,
      max_duration_seconds: None,
      keyword_filter: ["tutorial", "guide"],
      keyword_exclude: ["sponsored"],
      last_poll_at: None,
    )

  subscription_repo.update_config(conn, config, test_timestamp)
  |> should.be_ok()

  let videos = [
    // Matches keyword filter
    test_video(
      "tutorial_vid",
      "Python Tutorial for Beginners",
      Some(600),
      Some(test_timestamp - 3600),
    ),
    // Matches exclusion filter
    test_video(
      "sponsored_vid",
      "Sponsored Review",
      Some(600),
      Some(test_timestamp - 3600),
    ),
    // No keyword match
    test_video(
      "random_vid",
      "Random Vlog Day",
      Some(600),
      Some(test_timestamp - 3600),
    ),
    // Matches keyword but also excluded
    test_video(
      "sponsored_tut",
      "Sponsored Tutorial",
      Some(600),
      Some(test_timestamp - 3600),
    ),
  ]

  let results =
    list.map(videos, fn(video) {
      video_filter.should_download(video, config, None, test_timestamp)
    })

  // tutorial_vid - should pass
  list.first(results) |> should.be_ok() |> should.equal(PassedFilter)

  // sponsored_vid - should be excluded (no keyword match first)
  case results {
    [_, r2, ..] -> r2 |> should.equal(subscription_types.SkippedNoKeywordMatch)
    _ -> should.fail()
  }

  // random_vid - should fail keyword filter
  case results {
    [_, _, r3, ..] ->
      r3 |> should.equal(subscription_types.SkippedNoKeywordMatch)
    _ -> should.fail()
  }

  // sponsored_tut - should be excluded (matches "tutorial" but contains "sponsored")
  case results {
    [_, _, _, r4] -> r4 |> should.equal(SkippedExcludedKeyword("sponsored"))
    _ -> should.fail()
  }

  cleanup(conn, path)
}

/// Test configuration persistence across sessions
pub fn config_persistence_test() {
  let #(conn, path) = setup_test_db("persistence")

  // Set up custom config
  let custom_config =
    subscription_types.SubscriptionConfig(
      enabled: True,
      poll_interval_minutes: 15,
      browser: subscription_types.Chrome,
      cookies_path: None,
      max_age_days: 3,
      min_duration_seconds: 180,
      max_duration_seconds: Some(7200),
      keyword_filter: ["gaming", "gameplay"],
      keyword_exclude: ["reaction", "drama"],
      last_poll_at: Some(test_timestamp),
    )

  subscription_repo.update_config(conn, custom_config, test_timestamp)
  |> should.be_ok()

  // Update last_poll_at separately (it's not saved by update_config)
  subscription_repo.update_last_poll(conn, test_timestamp)
  |> should.be_ok()

  // Close and reopen database to simulate restart
  let _ = db.close(conn)

  let conn2 = db.init_db(path) |> should.be_ok()

  // Verify config persisted
  let loaded_config = subscription_repo.get_config(conn2) |> should.be_ok()

  loaded_config.enabled |> should.be_true()
  loaded_config.poll_interval_minutes |> should.equal(15)
  loaded_config.browser |> should.equal(subscription_types.Chrome)
  loaded_config.max_age_days |> should.equal(3)
  loaded_config.min_duration_seconds |> should.equal(180)
  loaded_config.max_duration_seconds |> should.equal(Some(7200))
  loaded_config.last_poll_at |> should.equal(Some(test_timestamp))

  let _ = db.close(conn2)
  let _ = simplifile.delete(path)
}

/// Test seen videos list ordering (most recent first)
pub fn seen_videos_ordering_test() {
  let #(conn, path) = setup_test_db("ordering")

  // Add videos at different times
  let video1 =
    test_video("first", "First Video", Some(600), Some(test_timestamp))
  let video2 =
    test_video("second", "Second Video", Some(600), Some(test_timestamp))
  let video3 =
    test_video("third", "Third Video", Some(600), Some(test_timestamp))

  // Insert in order: first, second, third (with increasing timestamps)
  subscription_repo.mark_seen(
    conn,
    video1,
    True,
    None,
    Some("j1"),
    1_700_000_001,
  )
  |> should.be_ok()
  subscription_repo.mark_seen(
    conn,
    video2,
    True,
    None,
    Some("j2"),
    1_700_000_002,
  )
  |> should.be_ok()
  subscription_repo.mark_seen(
    conn,
    video3,
    True,
    None,
    Some("j3"),
    1_700_000_003,
  )
  |> should.be_ok()

  // List should be in reverse order (most recent first)
  let videos = subscription_repo.list_seen_videos(conn, 10, 0) |> should.be_ok()

  case videos {
    [v1, v2, v3] -> {
      v1.video_id |> should.equal("third")
      v2.video_id |> should.equal("second")
      v3.video_id |> should.equal("first")
    }
    _ -> should.fail()
  }

  cleanup(conn, path)
}

/// Test the minimum duration filter correctly skips YouTube Shorts
pub fn shorts_detection_test() {
  let #(conn, path) = setup_test_db("shorts")

  let config = default_test_config()

  // Test various durations around the 2-minute boundary
  let durations = [
    #("15s", 15, SkippedTooShort),
    #("30s", 30, SkippedTooShort),
    #("59s", 59, SkippedTooShort),
    #("60s", 60, SkippedTooShort),
    #("90s", 90, SkippedTooShort),
    #("119s", 119, SkippedTooShort),
    #("120s", 120, PassedFilter),
    #("121s", 121, PassedFilter),
    #("180s", 180, PassedFilter),
    #("600s", 600, PassedFilter),
  ]

  list.each(durations, fn(test_case) {
    let #(label, duration, expected) = test_case
    let video =
      test_video(
        "test_" <> label,
        "Duration " <> label,
        Some(duration),
        Some(test_timestamp - 3600),
      )

    let result =
      video_filter.should_download(video, config, None, test_timestamp)
    result |> should.equal(expected)
  })

  cleanup(conn, path)
}
