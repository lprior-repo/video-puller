/// Subscription Repository Tests
///
/// Integration tests for subscription data access layer.
/// Beads: video-puller-13m.19 through 13m.29
import domain/subscription_types.{
  type DiscoveredVideo, ChannelSettings, DiscoveredVideo, SubscriptionConfig,
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
// Config Tests (Beads 13m.20, 13m.21)
// ============================================================================

/// Bead: video-puller-13m.21
/// Test get_config returns sensible default when table is empty
pub fn get_default_config_test() {
  let #(conn, path) = setup_test_db("default_config")

  // Should return default config when no config exists
  let config = subscription_repo.get_config(conn) |> should.be_ok()

  config.enabled |> should.be_false()
  config.poll_interval_minutes |> should.equal(60)
  config.min_duration_seconds |> should.equal(120)
  config.max_age_days |> should.equal(7)
  config.browser |> should.equal(subscription_types.Chromium)
  config.cookies_path |> should.equal(None)
  config.keyword_filter |> should.equal([])
  config.keyword_exclude |> should.equal([])
  config.last_poll_at |> should.equal(None)

  cleanup(conn, path)
}

/// Bead: video-puller-13m.20
/// Test save_config/get_config roundtrip preserves all fields
pub fn config_roundtrip_test() {
  let #(conn, path) = setup_test_db("config_roundtrip")

  // Create custom config with all fields set
  let new_config =
    SubscriptionConfig(
      enabled: True,
      poll_interval_minutes: 30,
      browser: subscription_types.Chrome,
      cookies_path: Some("/path/to/cookies.txt"),
      max_age_days: 14,
      min_duration_seconds: 180,
      max_duration_seconds: Some(3600),
      keyword_filter: ["tutorial", "guide", "howto"],
      keyword_exclude: ["ad", "sponsored", "promo"],
      last_poll_at: Some(1_700_123_456),
    )

  subscription_repo.update_config(conn, new_config, 1_700_000_000)
  |> should.be_ok()

  // Verify all fields were saved and can be retrieved
  let saved_config = subscription_repo.get_config(conn) |> should.be_ok()

  saved_config.enabled |> should.be_true()
  saved_config.poll_interval_minutes |> should.equal(30)
  saved_config.browser |> should.equal(subscription_types.Chrome)
  saved_config.cookies_path |> should.equal(Some("/path/to/cookies.txt"))
  saved_config.max_age_days |> should.equal(14)
  saved_config.min_duration_seconds |> should.equal(180)
  saved_config.max_duration_seconds |> should.equal(Some(3600))
  saved_config.keyword_filter |> should.equal(["tutorial", "guide", "howto"])
  saved_config.keyword_exclude |> should.equal(["ad", "sponsored", "promo"])
  saved_config.last_poll_at |> should.equal(Some(1_700_123_456))

  cleanup(conn, path)
}

// ============================================================================
// Seen Videos Tests (Beads 13m.22, 13m.23, 13m.24, 13m.25, 13m.26, 13m.29)
// ============================================================================

/// Bead: video-puller-13m.22
/// Test record_seen_video creates new entry with pending status
pub fn record_seen_video_creates_entry_test() {
  let #(conn, path) = setup_test_db("record_seen")

  let video = test_video("new_video_123", "New Video Title")

  // Record as seen with pending status (not downloaded, not skipped)
  subscription_repo.mark_seen(conn, video, False, None, None, 1_700_000_000)
  |> should.be_ok()

  // Verify it was created
  let seen =
    subscription_repo.get_seen_video(conn, "new_video_123") |> should.be_ok()

  case seen {
    Some(sv) -> {
      sv.video_id |> should.equal("new_video_123")
      sv.title |> should.equal("New Video Title")
      sv.downloaded |> should.be_false()
      sv.skipped |> should.be_false()
      sv.skip_reason |> should.equal(None)
      sv.job_id |> should.equal(None)
      sv.first_seen_at |> should.equal(1_700_000_000)
    }
    None -> should.fail()
  }

  cleanup(conn, path)
}

/// Bead: video-puller-13m.23
/// Test get_seen_video returns existing entry by video_id
pub fn get_seen_video_returns_existing_test() {
  let #(conn, path) = setup_test_db("get_seen_existing")

  let video = test_video("existing_vid", "Existing Video")

  // Create the entry first
  subscription_repo.mark_seen(
    conn,
    video,
    True,
    None,
    Some("job-456"),
    1_700_000_000,
  )
  |> should.be_ok()

  // Retrieve it by video_id
  let seen =
    subscription_repo.get_seen_video(conn, "existing_vid") |> should.be_ok()

  case seen {
    Some(sv) -> {
      sv.video_id |> should.equal("existing_vid")
      sv.title |> should.equal("Existing Video")
      sv.channel_id |> should.equal(Some("UC123"))
      sv.channel_name |> should.equal(Some("Test Channel"))
      sv.downloaded |> should.be_true()
      sv.job_id |> should.equal(Some("job-456"))
    }
    None -> should.fail()
  }

  cleanup(conn, path)
}

/// Bead: video-puller-13m.24
/// Test get_seen_video returns None for unknown video_id
pub fn get_seen_video_returns_none_test() {
  let #(conn, path) = setup_test_db("get_seen_none")

  subscription_repo.get_seen_video(conn, "nonexistent_video_id")
  |> should.be_ok()
  |> should.equal(None)

  cleanup(conn, path)
}

/// Bead: video-puller-13m.25
/// Test mark_downloaded updates seen video status and job_id
pub fn mark_downloaded_updates_status_test() {
  let #(conn, path) = setup_test_db("mark_downloaded")

  let video = test_video("dl_video", "Download Test")

  // Initially mark as seen but not downloaded
  subscription_repo.mark_seen(conn, video, False, None, None, 1_700_000_000)
  |> should.be_ok()

  // Verify initial state
  let seen_before =
    subscription_repo.get_seen_video(conn, "dl_video") |> should.be_ok()

  case seen_before {
    Some(sv) -> {
      sv.downloaded |> should.be_false()
      sv.job_id |> should.equal(None)
    }
    None -> should.fail()
  }

  // Now mark as downloaded with job_id
  subscription_repo.mark_downloaded(conn, "dl_video", "job-789")
  |> should.be_ok()

  // Verify it was updated
  let seen_after =
    subscription_repo.get_seen_video(conn, "dl_video") |> should.be_ok()

  case seen_after {
    Some(sv) -> {
      sv.downloaded |> should.be_true()
      sv.job_id |> should.equal(Some("job-789"))
    }
    None -> should.fail()
  }

  cleanup(conn, path)
}

/// Bead: video-puller-13m.26
/// Test mark_skipped updates seen video with skip_reason
pub fn mark_skipped_updates_skip_reason_test() {
  let #(conn, path) = setup_test_db("mark_skipped")

  let video = test_video("skipped_video", "Skipped Video")

  // Mark as seen but skipped with a reason
  subscription_repo.mark_seen(
    conn,
    video,
    False,
    Some("Video too short"),
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
      sv.skip_reason |> should.equal(Some("Video too short"))
      sv.job_id |> should.equal(None)
    }
    None -> should.fail()
  }

  cleanup(conn, path)
}

/// Bead: video-puller-13m.29
/// Test list_seen_videos returns entries sorted by discovered_at desc
pub fn list_seen_videos_sorted_test() {
  let #(conn, path) = setup_test_db("list_seen_sorted")

  // Add several videos with different timestamps
  let video1 = test_video("vid1", "First Video")
  let video2 = test_video("vid2", "Second Video")
  let video3 = test_video("vid3", "Third Video")

  subscription_repo.mark_seen(conn, video1, False, None, None, 1_700_000_001)
  |> should.be_ok()

  subscription_repo.mark_seen(
    conn,
    video2,
    False,
    None,
    None,
    1_700_000_003,
    // Most recent
  )
  |> should.be_ok()

  subscription_repo.mark_seen(conn, video3, False, None, None, 1_700_000_002)
  |> should.be_ok()

  // List all videos
  let videos = subscription_repo.list_seen_videos(conn, 10, 0) |> should.be_ok()

  list.length(videos) |> should.equal(3)

  // Verify they're sorted by first_seen_at descending (most recent first)
  case videos {
    [first, second, third] -> {
      first.video_id |> should.equal("vid2")
      // 1_700_000_003
      first.first_seen_at |> should.equal(1_700_000_003)

      second.video_id |> should.equal("vid3")
      // 1_700_000_002
      second.first_seen_at |> should.equal(1_700_000_002)

      third.video_id |> should.equal("vid1")
      // 1_700_000_001
      third.first_seen_at |> should.equal(1_700_000_001)
    }
    _ -> should.fail()
  }

  cleanup(conn, path)
}

// ============================================================================
// Channel Settings Tests (Beads 13m.27, 13m.28)
// ============================================================================

/// Bead: video-puller-13m.27
/// Test save_channel_settings/get_channel_settings roundtrip
pub fn channel_settings_roundtrip_test() {
  let #(conn, path) = setup_test_db("channel_roundtrip")

  let settings =
    ChannelSettings(
      channel_id: "UC_roundtrip",
      channel_name: "Roundtrip Channel",
      enabled: True,
      priority: 10,
      max_age_days: Some(3),
      min_duration_seconds: Some(60),
      max_duration_seconds: Some(1800),
      keyword_filter: ["gaming", "tutorial"],
      keyword_exclude: ["live", "stream"],
    )

  // Save channel settings
  subscription_repo.upsert_channel_settings(conn, settings, 1_700_000_000)
  |> should.be_ok()

  // Retrieve and verify all fields
  let retrieved =
    subscription_repo.get_channel_settings(conn, "UC_roundtrip")
    |> should.be_ok()

  case retrieved {
    Some(s) -> {
      s.channel_id |> should.equal("UC_roundtrip")
      s.channel_name |> should.equal("Roundtrip Channel")
      s.enabled |> should.be_true()
      s.priority |> should.equal(10)
      s.max_age_days |> should.equal(Some(3))
      s.min_duration_seconds |> should.equal(Some(60))
      s.max_duration_seconds |> should.equal(Some(1800))
      s.keyword_filter |> should.equal(["gaming", "tutorial"])
      s.keyword_exclude |> should.equal(["live", "stream"])
    }
    None -> should.fail()
  }

  cleanup(conn, path)
}

/// Bead: video-puller-13m.28
/// Test get_channel_settings returns None for unknown channel
pub fn get_channel_settings_returns_none_test() {
  let #(conn, path) = setup_test_db("channel_none")

  subscription_repo.get_channel_settings(conn, "UC_unknown_channel")
  |> should.be_ok()
  |> should.equal(None)

  cleanup(conn, path)
}
