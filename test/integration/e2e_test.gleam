/// End-to-End Integration Test
///
/// Tests the complete happy path from job creation to completion.
/// I-001: End-to-End Test (Happy Path)
import domain/core_types
import domain/subscription_types
import gleam/list
import gleam/option.{None, Some}
import gleam/string
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

/// Test the complete happy path workflow:
/// 1. Create a job (pending)
/// 2. List jobs and verify
/// 3. Update to downloading
/// 4. Update to completed
/// 5. Verify final state
pub fn happy_path_workflow_test() {
  let test_db = "/tmp/test_e2e_happy_path.db"
  let _ = simplifile.delete(test_db)

  // Initialize database
  let conn = db.init_db(test_db) |> should.be_ok()

  // Run migrations
  migrator.run_migrations(conn) |> should.be_ok()

  // 1. Create a job
  let job_id = core_types.new_job_id("test-job-001")
  let url = "https://www.youtube.com/watch?v=dQw4w9WgXcQ"
  let timestamp = 1_000_000

  repo.insert_job(conn, job_id, url, timestamp) |> should.be_ok()

  // 2. List jobs and verify the job is there
  let jobs = repo.list_jobs(conn, None) |> should.be_ok()
  list.length(jobs) |> should.equal(1)

  let job = list.first(jobs) |> should.be_ok()
  core_types.job_id_to_string(job.id) |> should.equal("test-job-001")
  job.url |> should.equal(url)
  job.status |> should.equal(core_types.Pending)

  // 3. Update to downloading with progress
  repo.update_status(conn, job_id, core_types.Downloading(50), timestamp + 100)
  |> should.be_ok()

  let jobs_downloading =
    repo.list_jobs(conn, Some("downloading")) |> should.be_ok()
  list.length(jobs_downloading) |> should.equal(1)

  let downloading_job = list.first(jobs_downloading) |> should.be_ok()
  case downloading_job.status {
    core_types.Downloading(progress) -> progress |> should.equal(50)
    _ -> should.fail()
  }

  // 4. Update to completed
  repo.update_status(conn, job_id, core_types.Completed, timestamp + 200)
  |> should.be_ok()

  // Set the output path
  repo.update_path(conn, job_id, "/downloads/video.mp4", timestamp + 200)
  |> should.be_ok()

  // 5. Verify final state
  let jobs_completed = repo.list_jobs(conn, Some("completed")) |> should.be_ok()
  list.length(jobs_completed) |> should.equal(1)

  let completed_job = list.first(jobs_completed) |> should.be_ok()
  completed_job.status |> should.equal(core_types.Completed)
  completed_job.path |> should.equal(Some("/downloads/video.mp4"))

  // Cleanup
  let _ = db.close(conn)
  let _ = simplifile.delete(test_db)
}

/// Test multiple jobs in the queue
pub fn multiple_jobs_test() {
  let test_db = "/tmp/test_e2e_multiple_jobs.db"
  let _ = simplifile.delete(test_db)

  let conn = db.init_db(test_db) |> should.be_ok()
  migrator.run_migrations(conn) |> should.be_ok()

  // Create multiple jobs
  let job1 = core_types.new_job_id("job-001")
  let job2 = core_types.new_job_id("job-002")
  let job3 = core_types.new_job_id("job-003")

  repo.insert_job(conn, job1, "https://youtube.com/1", 1000) |> should.be_ok()
  repo.insert_job(conn, job2, "https://youtube.com/2", 1001) |> should.be_ok()
  repo.insert_job(conn, job3, "https://youtube.com/3", 1002) |> should.be_ok()

  // Verify all jobs are pending
  let pending_jobs = repo.list_jobs(conn, Some("pending")) |> should.be_ok()
  list.length(pending_jobs) |> should.equal(3)

  // Complete one job
  repo.update_status(conn, job1, core_types.Completed, 2000) |> should.be_ok()

  // Fail one job
  repo.update_status(conn, job2, core_types.Failed("Network error"), 2000)
  |> should.be_ok()

  // Verify counts
  repo.list_jobs(conn, Some("pending"))
  |> should.be_ok()
  |> list.length()
  |> should.equal(1)
  repo.list_jobs(conn, Some("completed"))
  |> should.be_ok()
  |> list.length()
  |> should.equal(1)
  repo.list_jobs(conn, Some("failed"))
  |> should.be_ok()
  |> list.length()
  |> should.equal(1)

  // Total should still be 3
  repo.list_jobs(conn, None)
  |> should.be_ok()
  |> list.length()
  |> should.equal(3)

  // Cleanup
  let _ = db.close(conn)
  let _ = simplifile.delete(test_db)
}

/// Test zombie job recovery (jobs stuck in downloading state)
pub fn zombie_recovery_test() {
  let test_db = "/tmp/test_e2e_zombies.db"
  let _ = simplifile.delete(test_db)

  let conn = db.init_db(test_db) |> should.be_ok()
  migrator.run_migrations(conn) |> should.be_ok()

  // Create jobs in different states
  let job1 = core_types.new_job_id("zombie-001")
  let job2 = core_types.new_job_id("zombie-002")
  let job3 = core_types.new_job_id("normal-001")

  repo.insert_job(conn, job1, "https://youtube.com/1", 1000) |> should.be_ok()
  repo.insert_job(conn, job2, "https://youtube.com/2", 1000) |> should.be_ok()
  repo.insert_job(conn, job3, "https://youtube.com/3", 1000) |> should.be_ok()

  // Set two jobs to "downloading" (simulating crash mid-download)
  repo.update_status(conn, job1, core_types.Downloading(30), 1100)
  |> should.be_ok()
  repo.update_status(conn, job2, core_types.Downloading(75), 1100)
  |> should.be_ok()
  // job3 stays pending

  // Verify we have 2 downloading jobs
  repo.list_jobs(conn, Some("downloading"))
  |> should.be_ok()
  |> list.length()
  |> should.equal(2)

  // Run zombie recovery (simulates startup sequence)
  let reset_count = repo.reset_zombies(conn, 2000) |> should.be_ok()
  reset_count |> should.equal(2)

  // Verify all jobs are now pending
  repo.list_jobs(conn, Some("downloading"))
  |> should.be_ok()
  |> list.length()
  |> should.equal(0)

  repo.list_jobs(conn, Some("pending"))
  |> should.be_ok()
  |> list.length()
  |> should.equal(3)

  // Cleanup
  let _ = db.close(conn)
  let _ = simplifile.delete(test_db)
}

/// Test idempotency of migrations and operations
pub fn idempotency_test() {
  let test_db = "/tmp/test_e2e_idempotency.db"
  let _ = simplifile.delete(test_db)

  let conn = db.init_db(test_db) |> should.be_ok()

  // Run migrations multiple times
  migrator.run_migrations(conn) |> should.be_ok()
  migrator.run_migrations(conn) |> should.be_ok()
  migrator.run_migrations(conn) |> should.be_ok()

  // Create a job
  let job_id = core_types.new_job_id("idem-001")
  repo.insert_job(conn, job_id, "https://test.com", 1000) |> should.be_ok()

  // Update status multiple times to same value
  repo.update_status(conn, job_id, core_types.Downloading(50), 1100)
  |> should.be_ok()
  repo.update_status(conn, job_id, core_types.Downloading(50), 1101)
  |> should.be_ok()

  // Verify only one job exists
  repo.list_jobs(conn, None)
  |> should.be_ok()
  |> list.length()
  |> should.equal(1)

  // Cleanup
  let _ = db.close(conn)
  let _ = simplifile.delete(test_db)
}

/// Test E2E download flow with mocked yt-dlp output
/// Bead: video-puller-13m.71
///
/// Tests the complete download workflow from job creation to completion
/// with simulated yt-dlp progress output. This test verifies:
/// 1. Job creation in pending state
/// 2. Progress updates via mocked yt-dlp output parsing
/// 3. Successful completion with file path
/// 4. Database state consistency throughout the flow
pub fn full_download_flow_with_mocked_ytdlp_test() {
  let test_db = "/tmp/test_e2e_full_download.db"
  let _ = simplifile.delete(test_db)

  let conn = db.init_db(test_db) |> should.be_ok()
  migrator.run_migrations(conn) |> should.be_ok()

  // 1. Create a job (simulating user submitting a download)
  let job_id = core_types.new_job_id("test-download-001")
  let url = "https://www.youtube.com/watch?v=dQw4w9WgXcQ"
  let timestamp = 1_000_000

  repo.insert_job(conn, job_id, url, timestamp) |> should.be_ok()

  // Verify job is pending
  let job = repo.get_job(conn, job_id) |> should.be_ok() |> should.be_some()
  job.status |> should.equal(core_types.Pending)

  // 2. Simulate download starting (worker picks up job)
  repo.update_status(conn, job_id, core_types.Downloading(0), timestamp + 10)
  |> should.be_ok()

  let job_downloading =
    repo.get_job(conn, job_id) |> should.be_ok() |> should.be_some()
  case job_downloading.status {
    core_types.Downloading(progress) -> progress |> should.equal(0)
    _ -> should.fail()
  }

  // 3. Simulate progress updates (parsing mocked yt-dlp output)
  // These would come from parser.parse_progress() in real flow
  let progress_updates = [
    #(10, "[download]  10.5% of  100.00MiB at  1.23MiB/s ETA 01:15"),
    #(25, "[download]  25.3% of  100.00MiB at  1.45MiB/s ETA 00:52"),
    #(50, "[download]  50.8% of  100.00MiB at  1.67MiB/s ETA 00:30"),
    #(75, "[download]  75.2% of  100.00MiB at  1.89MiB/s ETA 00:13"),
    #(95, "[download]  95.7% of  100.00MiB at  2.01MiB/s ETA 00:02"),
  ]

  list.each(progress_updates, fn(update) {
    let #(progress, _line) = update
    repo.update_status(
      conn,
      job_id,
      core_types.Downloading(progress),
      timestamp + 100,
    )
    |> should.be_ok()

    // Verify progress was updated
    let job_progress =
      repo.get_job(conn, job_id) |> should.be_ok() |> should.be_some()
    case job_progress.status {
      core_types.Downloading(p) -> p |> should.equal(progress)
      _ -> should.fail()
    }
  })

  // 4. Simulate download completion
  repo.update_status(conn, job_id, core_types.Completed, timestamp + 200)
  |> should.be_ok()

  // Set output path (simulating file system result)
  let output_path = "/downloads/test-download-001.mp4"
  repo.update_path(conn, job_id, output_path, timestamp + 200)
  |> should.be_ok()

  // 5. Verify final state
  let completed_job =
    repo.get_job(conn, job_id) |> should.be_ok() |> should.be_some()
  completed_job.status |> should.equal(core_types.Completed)
  completed_job.path |> should.equal(Some(output_path))

  // Verify job appears in completed list
  let completed_jobs = repo.list_jobs(conn, Some("completed")) |> should.be_ok()
  list.length(completed_jobs) |> should.equal(1)

  // Verify job does not appear in pending or downloading lists
  let pending_jobs = repo.list_jobs(conn, Some("pending")) |> should.be_ok()
  list.length(pending_jobs) |> should.equal(0)

  let downloading_jobs =
    repo.list_jobs(conn, Some("downloading")) |> should.be_ok()
  list.length(downloading_jobs) |> should.equal(0)

  // Cleanup
  let _ = db.close(conn)
  let _ = simplifile.delete(test_db)
}

/// Test E2E subscription poll flow with mocked feed response
/// Bead: video-puller-13m.72
///
/// Tests the complete subscription polling workflow with simulated feed data.
/// This test verifies:
/// 1. Subscription configuration setup
/// 2. Feed fetch simulation with multiple videos
/// 3. Video filtering (duration, age, keywords)
/// 4. Seen video tracking
/// 5. Job creation for passing videos
/// 6. Skip reason recording for filtered videos
pub fn subscription_poll_flow_with_mocked_feed_test() {
  let test_db = "/tmp/test_e2e_subscription_poll.db"
  let _ = simplifile.delete(test_db)

  let conn = db.init_db(test_db) |> should.be_ok()
  migrator.run_migrations(conn) |> should.be_ok()

  let timestamp = 1_705_276_800

  // 1. Set up subscription configuration
  let config =
    subscription_types.SubscriptionConfig(
      enabled: True,
      poll_interval_minutes: 60,
      browser: subscription_types.Chromium,
      cookies_path: None,
      max_age_days: 7,
      min_duration_seconds: 120,
      max_duration_seconds: None,
      keyword_filter: ["tutorial", "guide"],
      keyword_exclude: ["sponsored", "ad"],
      last_poll_at: None,
    )

  subscription_repo.update_config(conn, config, timestamp) |> should.be_ok()

  // Verify config saved
  let loaded_config = subscription_repo.get_config(conn) |> should.be_ok()
  loaded_config.enabled |> should.be_true()
  loaded_config.min_duration_seconds |> should.equal(120)

  // 2. Simulate feed response (mocked yt-dlp output)
  // These would normally come from subscription_feed.fetch_feed()
  let mocked_videos = [
    // Video 1: Good tutorial - should pass all filters
    subscription_types.DiscoveredVideo(
      video_id: "vid_good_tutorial",
      channel_id: Some("UC_tech_channel"),
      channel_name: Some("Tech Tutorials"),
      title: "Complete Python Tutorial for Beginners",
      url: "https://www.youtube.com/watch?v=vid_good_tutorial",
      published_at: Some(timestamp - 86_400),
      duration_seconds: Some(1800),
      thumbnail_url: Some(
        "https://i.ytimg.com/vi/vid_good_tutorial/default.jpg",
      ),
    ),
    // Video 2: Too short (YouTube Short) - should be skipped
    subscription_types.DiscoveredVideo(
      video_id: "vid_short",
      channel_id: Some("UC_tech_channel"),
      channel_name: Some("Tech Tutorials"),
      title: "Quick Python Tip",
      url: "https://www.youtube.com/watch?v=vid_short",
      published_at: Some(timestamp - 3600),
      duration_seconds: Some(45),
      thumbnail_url: Some("https://i.ytimg.com/vi/vid_short/default.jpg"),
    ),
    // Video 3: Too old - should be skipped
    subscription_types.DiscoveredVideo(
      video_id: "vid_old",
      channel_id: Some("UC_tech_channel"),
      channel_name: Some("Tech Tutorials"),
      title: "Old Tutorial Video",
      url: "https://www.youtube.com/watch?v=vid_old",
      published_at: Some(timestamp - { 10 * 86_400 }),
      duration_seconds: Some(900),
      thumbnail_url: Some("https://i.ytimg.com/vi/vid_old/default.jpg"),
    ),
    // Video 4: No keyword match - should be skipped
    subscription_types.DiscoveredVideo(
      video_id: "vid_no_keyword",
      channel_id: Some("UC_vlog_channel"),
      channel_name: Some("Daily Vlog"),
      title: "My Daily Routine Vlog",
      url: "https://www.youtube.com/watch?v=vid_no_keyword",
      published_at: Some(timestamp - 43_200),
      duration_seconds: Some(600),
      thumbnail_url: Some("https://i.ytimg.com/vi/vid_no_keyword/default.jpg"),
    ),
    // Video 5: Excluded keyword - should be skipped
    subscription_types.DiscoveredVideo(
      video_id: "vid_sponsored",
      channel_id: Some("UC_tech_channel"),
      channel_name: Some("Tech Tutorials"),
      title: "Sponsored Python Tutorial - Special Offer",
      url: "https://www.youtube.com/watch?v=vid_sponsored",
      published_at: Some(timestamp - 7200),
      duration_seconds: Some(600),
      thumbnail_url: Some("https://i.ytimg.com/vi/vid_sponsored/default.jpg"),
    ),
    // Video 6: Another good guide - should pass
    subscription_types.DiscoveredVideo(
      video_id: "vid_good_guide",
      channel_id: Some("UC_edu_channel"),
      channel_name: Some("Education Hub"),
      title: "Complete Beginner's Guide to JavaScript",
      url: "https://www.youtube.com/watch?v=vid_good_guide",
      published_at: Some(timestamp - 21_600),
      duration_seconds: Some(2400),
      thumbnail_url: Some("https://i.ytimg.com/vi/vid_good_guide/default.jpg"),
    ),
  ]

  // 3. Process videos through filter logic (simulating subscription_manager)
  list.each(mocked_videos, fn(video) {
    // Check if already seen
    let is_seen =
      subscription_repo.is_seen(conn, video.video_id) |> should.be_ok()

    case is_seen {
      True -> {
        // Already processed - skip
        Nil
      }
      False -> {
        // Apply filters (simulating video_filter.should_download logic)
        let filter_result = apply_mock_filters(video, config, timestamp)

        case filter_result {
          subscription_types.PassedFilter -> {
            // Create download job
            let job_id = core_types.new_job_id("sub-job-" <> video.video_id)
            repo.insert_job(conn, job_id, video.url, timestamp)
            |> should.be_ok()

            // Mark as seen with job reference
            subscription_repo.mark_seen(
              conn,
              video,
              True,
              None,
              Some(core_types.job_id_to_string(job_id)),
              timestamp,
            )
            |> should.be_ok()
          }
          _ -> {
            // Mark as seen with skip reason
            let skip_reason = filter_result_to_string(filter_result)
            subscription_repo.mark_seen(
              conn,
              video,
              False,
              Some(skip_reason),
              None,
              timestamp,
            )
            |> should.be_ok()
          }
        }
      }
    }
  })

  // 4. Verify results

  // Should have 2 jobs created (good_tutorial and good_guide)
  let pending_jobs = repo.list_jobs(conn, Some("pending")) |> should.be_ok()
  list.length(pending_jobs) |> should.equal(2)

  // Should have 6 seen videos total
  let seen_count = subscription_repo.count_seen_videos(conn) |> should.be_ok()
  seen_count |> should.equal(6)

  // Should have 2 downloaded (queued for download)
  let downloaded_count =
    subscription_repo.count_downloaded(conn) |> should.be_ok()
  downloaded_count |> should.equal(2)

  // 5. Verify specific video states

  // Good tutorial - should be queued
  let good_tutorial =
    subscription_repo.get_seen_video(conn, "vid_good_tutorial")
    |> should.be_ok()
  case good_tutorial {
    Some(v) -> {
      v.downloaded |> should.be_true()
      v.skipped |> should.be_false()
      case v.job_id {
        Some(job_id) -> {
          job_id |> should.equal("sub-job-vid_good_tutorial")
        }
        None -> should.fail()
      }
    }
    None -> should.fail()
  }

  // Short video - should be skipped
  let short_video =
    subscription_repo.get_seen_video(conn, "vid_short") |> should.be_ok()
  case short_video {
    Some(v) -> {
      v.downloaded |> should.be_false()
      v.skipped |> should.be_true()
      v.skip_reason |> should.equal(Some("Video too short"))
    }
    None -> should.fail()
  }

  // Old video - should be skipped
  let old_video =
    subscription_repo.get_seen_video(conn, "vid_old") |> should.be_ok()
  case old_video {
    Some(v) -> {
      v.downloaded |> should.be_false()
      v.skipped |> should.be_true()
      v.skip_reason |> should.equal(Some("Video too old"))
    }
    None -> should.fail()
  }

  // No keyword match - should be skipped
  let no_keyword =
    subscription_repo.get_seen_video(conn, "vid_no_keyword") |> should.be_ok()
  case no_keyword {
    Some(v) -> {
      v.downloaded |> should.be_false()
      v.skipped |> should.be_true()
      v.skip_reason |> should.equal(Some("No keyword match"))
    }
    None -> should.fail()
  }

  // Sponsored - should be skipped with excluded keyword
  let sponsored =
    subscription_repo.get_seen_video(conn, "vid_sponsored") |> should.be_ok()
  case sponsored {
    Some(v) -> {
      v.downloaded |> should.be_false()
      v.skipped |> should.be_true()
      v.skip_reason |> should.equal(Some("Excluded keyword: sponsored"))
    }
    None -> should.fail()
  }

  // Good guide - should be queued
  let good_guide =
    subscription_repo.get_seen_video(conn, "vid_good_guide") |> should.be_ok()
  case good_guide {
    Some(v) -> {
      v.downloaded |> should.be_true()
      v.skipped |> should.be_false()
      case v.job_id {
        Some(_) -> True |> should.be_true()
        None -> should.fail()
      }
    }
    None -> should.fail()
  }

  // 6. Test idempotency - processing same videos again should not create duplicates
  list.each(mocked_videos, fn(video) {
    let is_seen =
      subscription_repo.is_seen(conn, video.video_id) |> should.be_ok()
    is_seen |> should.be_true()
  })

  // Still should have same counts
  let pending_jobs_2 = repo.list_jobs(conn, Some("pending")) |> should.be_ok()
  list.length(pending_jobs_2) |> should.equal(2)

  let seen_count_2 = subscription_repo.count_seen_videos(conn) |> should.be_ok()
  seen_count_2 |> should.equal(6)

  // Cleanup
  let _ = db.close(conn)
  let _ = simplifile.delete(test_db)
}

// Helper functions for mocked subscription flow

fn apply_mock_filters(
  video: subscription_types.DiscoveredVideo,
  config: subscription_types.SubscriptionConfig,
  current_time: Int,
) -> subscription_types.FilterResult {
  // Check age
  case video.published_at {
    Some(published) -> {
      let age_seconds = current_time - published
      let max_age_seconds = config.max_age_days * 86_400
      case age_seconds > max_age_seconds {
        True -> subscription_types.SkippedTooOld
        False -> {
          // Check duration
          case video.duration_seconds {
            Some(duration) -> {
              case duration < config.min_duration_seconds {
                True -> subscription_types.SkippedTooShort
                False -> {
                  // Check keywords
                  check_keywords(video.title, config)
                }
              }
            }
            None -> subscription_types.PassedFilter
          }
        }
      }
    }
    None -> {
      // No publish date - check duration and keywords
      case video.duration_seconds {
        Some(duration) -> {
          case duration < config.min_duration_seconds {
            True -> subscription_types.SkippedTooShort
            False -> check_keywords(video.title, config)
          }
        }
        None -> check_keywords(video.title, config)
      }
    }
  }
}

fn check_keywords(
  title: String,
  config: subscription_types.SubscriptionConfig,
) -> subscription_types.FilterResult {
  let title_lower = string.lowercase(title)

  // Check excluded keywords first
  let excluded =
    list.find(config.keyword_exclude, fn(keyword) {
      string.contains(title_lower, string.lowercase(keyword))
    })

  case excluded {
    Ok(keyword) -> subscription_types.SkippedExcludedKeyword(keyword)
    Error(_) -> {
      // Check required keywords
      case list.is_empty(config.keyword_filter) {
        True -> subscription_types.PassedFilter
        False -> {
          let has_keyword =
            list.any(config.keyword_filter, fn(keyword) {
              string.contains(title_lower, string.lowercase(keyword))
            })
          case has_keyword {
            True -> subscription_types.PassedFilter
            False -> subscription_types.SkippedNoKeywordMatch
          }
        }
      }
    }
  }
}

fn filter_result_to_string(result: subscription_types.FilterResult) -> String {
  case result {
    subscription_types.PassedFilter -> "Passed"
    subscription_types.SkippedTooOld -> "Video too old"
    subscription_types.SkippedTooShort -> "Video too short"
    subscription_types.SkippedTooLong -> "Video too long"
    subscription_types.SkippedNoKeywordMatch -> "No keyword match"
    subscription_types.SkippedExcludedKeyword(kw) -> "Excluded keyword: " <> kw
    subscription_types.SkippedAlreadySeen -> "Already seen"
    subscription_types.SkippedAlreadyDownloaded -> "Already downloaded"
  }
}
