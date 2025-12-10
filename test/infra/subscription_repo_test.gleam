/// Subscription Repository Tests
///
/// Integration tests for subscription data access layer.
import domain/subscription_types.{
  type DiscoveredVideo, ChannelSettings, DiscoveredVideo,
}
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

fn test_video(video_id: String, title: String) -> DiscoveredVideo {
  DiscoveredVideo(
    video_id: video_id,
    channel_id: Some("UC123"),
    channel_name: Some("Test Channel"),
    title: title,
    url: "https://www.youtube.com/watch?v=" <> video_id,
    published_at: Some(1_700_000_000),
    duration_seconds: Some(600),
    thumbnail_url: Some(
      "https://i.ytimg.com/vi/" <> video_id <> "/hqdefault.jpg",
    ),
  )
}

fn setup_test_db(name: String) {
  let test_db = "/tmp/test_sub_repo_" <> name <> ".db"
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
// Config Tests
// ============================================================================

pub fn get_default_config_test() {
  let #(conn, path) = setup_test_db("default_config")

  // Should return default config when no config exists
  let config = subscription_repo.get_config(conn) |> should.be_ok()

  config.enabled |> should.be_false()
  config.poll_interval_minutes |> should.equal(60)
  config.min_duration_seconds |> should.equal(120)
  config.max_age_days |> should.equal(7)

  cleanup(conn, path)
}

pub fn update_config_test() {
  let #(conn, path) = setup_test_db("update_config")

  // Create custom config
  let new_config =
    subscription_types.SubscriptionConfig(
      enabled: True,
      poll_interval_minutes: 30,
      browser: subscription_types.Chrome,
      cookies_path: None,
      max_age_days: 14,
      min_duration_seconds: 180,
      max_duration_seconds: Some(3600),
      keyword_filter: ["tutorial", "guide"],
      keyword_exclude: ["ad", "sponsored"],
      last_poll_at: None,
    )

  subscription_repo.update_config(conn, new_config, 1_700_000_000)
  |> should.be_ok()

  // Verify it was saved
  let saved_config = subscription_repo.get_config(conn) |> should.be_ok()

  saved_config.enabled |> should.be_true()
  saved_config.poll_interval_minutes |> should.equal(30)
  saved_config.max_age_days |> should.equal(14)
  saved_config.min_duration_seconds |> should.equal(180)
  saved_config.max_duration_seconds |> should.equal(Some(3600))

  cleanup(conn, path)
}

pub fn update_last_poll_test() {
  let #(conn, path) = setup_test_db("last_poll")

  subscription_repo.update_last_poll(conn, 1_700_000_000)
  |> should.be_ok()

  let config = subscription_repo.get_config(conn) |> should.be_ok()
  config.last_poll_at |> should.equal(Some(1_700_000_000))

  cleanup(conn, path)
}

// ============================================================================
// Seen Videos Tests
// ============================================================================

pub fn is_seen_false_for_new_video_test() {
  let #(conn, path) = setup_test_db("is_seen_new")

  subscription_repo.is_seen(conn, "new_video_id")
  |> should.be_ok()
  |> should.be_false()

  cleanup(conn, path)
}

pub fn mark_seen_and_is_seen_test() {
  let #(conn, path) = setup_test_db("mark_seen")

  let video = test_video("seen_video", "Test Video")

  // Mark as seen
  subscription_repo.mark_seen(
    conn,
    video,
    True,
    None,
    Some("job-123"),
    1_700_000_000,
  )
  |> should.be_ok()

  // Should now be seen
  subscription_repo.is_seen(conn, "seen_video")
  |> should.be_ok()
  |> should.be_true()

  cleanup(conn, path)
}

pub fn get_seen_video_test() {
  let #(conn, path) = setup_test_db("get_seen")

  let video = test_video("get_video", "Get Test Video")

  // Mark as seen with download
  subscription_repo.mark_seen(
    conn,
    video,
    True,
    None,
    Some("job-456"),
    1_700_000_000,
  )
  |> should.be_ok()

  // Retrieve it
  let seen =
    subscription_repo.get_seen_video(conn, "get_video") |> should.be_ok()

  case seen {
    Some(sv) -> {
      sv.video_id |> should.equal("get_video")
      sv.title |> should.equal("Get Test Video")
      sv.downloaded |> should.be_true()
      sv.skipped |> should.be_false()
      sv.job_id |> should.equal(Some("job-456"))
    }
    None -> should.fail()
  }

  cleanup(conn, path)
}

pub fn get_seen_video_not_found_test() {
  let #(conn, path) = setup_test_db("get_seen_not_found")

  subscription_repo.get_seen_video(conn, "nonexistent")
  |> should.be_ok()
  |> should.equal(None)

  cleanup(conn, path)
}

pub fn mark_seen_with_skip_reason_test() {
  let #(conn, path) = setup_test_db("skip_reason")

  let video = test_video("skipped_video", "Skipped Video")

  // Mark as seen but skipped
  subscription_repo.mark_seen(
    conn,
    video,
    False,
    Some("too short"),
    None,
    1_700_000_000,
  )
  |> should.be_ok()

  let seen =
    subscription_repo.get_seen_video(conn, "skipped_video") |> should.be_ok()

  case seen {
    Some(sv) -> {
      sv.downloaded |> should.be_false()
      sv.skipped |> should.be_true()
      sv.skip_reason |> should.equal(Some("too short"))
    }
    None -> should.fail()
  }

  cleanup(conn, path)
}

pub fn mark_downloaded_test() {
  let #(conn, path) = setup_test_db("mark_downloaded")

  let video = test_video("dl_video", "Download Test")

  // Initially mark as seen but not downloaded
  subscription_repo.mark_seen(conn, video, False, None, None, 1_700_000_000)
  |> should.be_ok()

  // Now mark as downloaded
  subscription_repo.mark_downloaded(conn, "dl_video", "job-789")
  |> should.be_ok()

  // Verify
  let seen =
    subscription_repo.get_seen_video(conn, "dl_video") |> should.be_ok()

  case seen {
    Some(sv) -> {
      sv.downloaded |> should.be_true()
      sv.job_id |> should.equal(Some("job-789"))
    }
    None -> should.fail()
  }

  cleanup(conn, path)
}

pub fn list_seen_videos_test() {
  let #(conn, path) = setup_test_db("list_seen")

  // Add several videos
  let video1 = test_video("list_vid1", "Video One")
  let video2 = test_video("list_vid2", "Video Two")
  let video3 = test_video("list_vid3", "Video Three")

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
    False,
    Some("too old"),
    None,
    1_700_000_003,
  )
  |> should.be_ok()

  // List all
  let videos = subscription_repo.list_seen_videos(conn, 10, 0) |> should.be_ok()
  list.length(videos) |> should.equal(3)

  // Test pagination
  let page1 = subscription_repo.list_seen_videos(conn, 2, 0) |> should.be_ok()
  list.length(page1) |> should.equal(2)

  let page2 = subscription_repo.list_seen_videos(conn, 2, 2) |> should.be_ok()
  list.length(page2) |> should.equal(1)

  cleanup(conn, path)
}

pub fn count_seen_videos_test() {
  let #(conn, path) = setup_test_db("count_seen")

  // Add videos
  let video1 = test_video("count_vid1", "Video One")
  let video2 = test_video("count_vid2", "Video Two")

  subscription_repo.mark_seen(
    conn,
    video1,
    True,
    None,
    Some("j1"),
    1_700_000_000,
  )
  |> should.be_ok()
  subscription_repo.mark_seen(
    conn,
    video2,
    False,
    Some("skipped"),
    None,
    1_700_000_000,
  )
  |> should.be_ok()

  // Count total
  subscription_repo.count_seen_videos(conn)
  |> should.be_ok()
  |> should.equal(2)

  // Count downloaded
  subscription_repo.count_downloaded(conn)
  |> should.be_ok()
  |> should.equal(1)

  cleanup(conn, path)
}

// ============================================================================
// Channel Settings Tests
// ============================================================================

pub fn get_channel_settings_not_found_test() {
  let #(conn, path) = setup_test_db("channel_not_found")

  subscription_repo.get_channel_settings(conn, "UC_unknown")
  |> should.be_ok()
  |> should.equal(None)

  cleanup(conn, path)
}

pub fn upsert_channel_settings_test() {
  let #(conn, path) = setup_test_db("upsert_channel")

  let settings =
    ChannelSettings(
      channel_id: "UC_test123",
      channel_name: "Test Channel",
      enabled: True,
      priority: 10,
      max_age_days: Some(3),
      min_duration_seconds: Some(60),
      max_duration_seconds: Some(1800),
      keyword_filter: ["gaming"],
      keyword_exclude: ["live"],
    )

  subscription_repo.upsert_channel_settings(conn, settings, 1_700_000_000)
  |> should.be_ok()

  // Retrieve and verify
  let retrieved =
    subscription_repo.get_channel_settings(conn, "UC_test123") |> should.be_ok()

  case retrieved {
    Some(s) -> {
      s.channel_name |> should.equal("Test Channel")
      s.enabled |> should.be_true()
      s.priority |> should.equal(10)
      s.max_age_days |> should.equal(Some(3))
      s.min_duration_seconds |> should.equal(Some(60))
      s.max_duration_seconds |> should.equal(Some(1800))
    }
    None -> should.fail()
  }

  cleanup(conn, path)
}

pub fn update_channel_settings_test() {
  let #(conn, path) = setup_test_db("update_channel")

  // Create initial settings
  let initial =
    ChannelSettings(
      channel_id: "UC_update",
      channel_name: "Update Channel",
      enabled: True,
      priority: 5,
      max_age_days: None,
      min_duration_seconds: None,
      max_duration_seconds: None,
      keyword_filter: [],
      keyword_exclude: [],
    )

  subscription_repo.upsert_channel_settings(conn, initial, 1_700_000_000)
  |> should.be_ok()

  // Update settings
  let updated =
    ChannelSettings(
      channel_id: "UC_update",
      channel_name: "Updated Channel Name",
      enabled: False,
      priority: 1,
      max_age_days: Some(1),
      min_duration_seconds: Some(300),
      max_duration_seconds: None,
      keyword_filter: ["new"],
      keyword_exclude: [],
    )

  subscription_repo.upsert_channel_settings(conn, updated, 1_700_001_000)
  |> should.be_ok()

  // Verify update
  let retrieved =
    subscription_repo.get_channel_settings(conn, "UC_update") |> should.be_ok()

  case retrieved {
    Some(s) -> {
      s.channel_name |> should.equal("Updated Channel Name")
      s.enabled |> should.be_false()
      s.priority |> should.equal(1)
      s.max_age_days |> should.equal(Some(1))
    }
    None -> should.fail()
  }

  cleanup(conn, path)
}

// ============================================================================
// Edge Cases
// ============================================================================

pub fn mark_seen_updates_existing_test() {
  let #(conn, path) = setup_test_db("update_existing")

  let video = test_video("update_vid", "Update Video")

  // First insert
  subscription_repo.mark_seen(
    conn,
    video,
    False,
    Some("skipped"),
    None,
    1_700_000_000,
  )
  |> should.be_ok()

  // Update to downloaded
  subscription_repo.mark_seen(
    conn,
    video,
    True,
    None,
    Some("job-new"),
    1_700_001_000,
  )
  |> should.be_ok()

  // Should only have one entry, updated
  subscription_repo.count_seen_videos(conn)
  |> should.be_ok()
  |> should.equal(1)

  let seen =
    subscription_repo.get_seen_video(conn, "update_vid") |> should.be_ok()

  case seen {
    Some(sv) -> {
      sv.downloaded |> should.be_true()
      sv.job_id |> should.equal(Some("job-new"))
    }
    None -> should.fail()
  }

  cleanup(conn, path)
}

pub fn empty_keyword_lists_test() {
  let #(conn, path) = setup_test_db("empty_keywords")

  let config =
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

  subscription_repo.update_config(conn, config, 1_700_000_000)
  |> should.be_ok()

  let saved = subscription_repo.get_config(conn) |> should.be_ok()
  saved.keyword_filter |> should.equal([])
  saved.keyword_exclude |> should.equal([])

  cleanup(conn, path)
}
