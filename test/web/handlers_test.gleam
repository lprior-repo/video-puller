import domain/core_types
import domain/subscription_types.{DiscoveredVideo, SubscriptionStatus}
import engine/subscription_progress.{Snapshot}
import engine/ytdl_sub
import envoy
import gleam/erlang/process
import gleam/http
import gleam/http/response
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import gleeunit
import gleeunit/should
import infra/db
import infra/migrator
import infra/repo
import infra/subscription_repo
import lustre/element.{to_document_string}
import simplifile
import web/handlers
import web/middleware
import web/templates
import wisp/simulate

pub fn main() {
  gleeunit.main()
}

// =============================================================================
// Helper functions for testing
// =============================================================================

/// Create a test database and context for handler tests
fn setup_test_db(test_name: String) -> #(db.Db, middleware.Context) {
  let test_db = "/tmp/test_handlers_" <> test_name <> ".db"
  let _ = simplifile.delete(test_db)

  let conn = case db.init_db(test_db) {
    Ok(c) -> c
    Error(_) -> panic as "Failed to init test database"
  }

  case migrator.run_migrations(conn) {
    Ok(_) -> Nil
    Error(_) -> panic as "Failed to run migrations"
  }

  let ctx =
    middleware.new_context(
      conn,
      "/tmp/static",
      "/tmp/test_downloads_" <> test_name,
    )

  #(conn, ctx)
}

/// Clean up test database
fn cleanup_test_db(conn: db.Db, test_name: String) {
  let _ = db.close(conn)
  let test_db = "/tmp/test_handlers_" <> test_name <> ".db"
  let _ = simplifile.delete(test_db)
}

// =============================================================================
// Test get_parent_directory function
// =============================================================================

pub fn get_parent_directory_simple_test() {
  handlers.get_parent_directory("/home/user/videos/file.mp4")
  |> should.equal("/home/user/videos")
}

pub fn get_parent_directory_root_test() {
  handlers.get_parent_directory("/file.mp4")
  |> should.equal("")
}

pub fn get_parent_directory_nested_test() {
  handlers.get_parent_directory(
    "/home/user/downloads/videos/subfolder/video.mp4",
  )
  |> should.equal("/home/user/downloads/videos/subfolder")
}

pub fn get_parent_directory_no_extension_test() {
  handlers.get_parent_directory("/home/user/videos/myfile")
  |> should.equal("/home/user/videos")
}

pub fn get_parent_directory_empty_test() {
  handlers.get_parent_directory("")
  |> should.equal(".")
}

pub fn get_parent_directory_single_component_test() {
  handlers.get_parent_directory("file.mp4")
  |> should.equal(".")
}

pub fn get_parent_directory_trailing_slash_test() {
  handlers.get_parent_directory("/home/user/videos/")
  |> should.equal("/home/user/videos")
}

/// The settings page reflects the manager's configured output and installed yt-dlp.
pub fn settings_uses_effective_output_and_yt_dlp_version_test() {
  let #(conn, ctx) = setup_test_db("settings_effective_output")
  let response = handlers.settings(simulate.request(http.Get, "/settings"), ctx)
  let body = simulate.read_body(response)

  response.status |> should.equal(200)
  body |> string.contains(ctx.output_directory) |> should.be_true()
  body |> string.contains("yt-dlp Version") |> should.be_true()
  body |> string.contains("2024.11.18") |> should.be_false()

  cleanup_test_db(conn, "settings_effective_output")
}

pub fn get_parent_directory_multiple_extensions_test() {
  handlers.get_parent_directory("/home/user/videos/archive.tar.gz")
  |> should.equal("/home/user/videos")
}

pub fn plex_library_url_uses_native_media_route_when_machine_id_present_test() {
  handlers.plex_library_url_for(
    "http://127.0.0.1:32400/",
    Some("machine123"),
    Some("4"),
  )
  |> should.equal(Some(
    "http://127.0.0.1:32400/web/index.html#!/media/machine123/com.plexapp.plugins.library?source=4",
  ))
}

pub fn plex_library_url_falls_back_to_web_without_machine_id_test() {
  handlers.plex_library_url_for("http://127.0.0.1:32400", None, Some("4"))
  |> should.equal(Some("http://127.0.0.1:32400/web"))
}

// =============================================================================
// Test validate_video_url function
// =============================================================================

pub fn validate_video_url_https_test() {
  case handlers.validate_video_url("https://youtube.com/watch?v=123") {
    Ok(url) -> url |> should.equal("https://youtube.com/watch?v=123")
    Error(_) -> should.fail()
  }
}

pub fn validate_video_url_http_test() {
  case handlers.validate_video_url("http://example.com/video") {
    Ok(url) -> url |> should.equal("http://example.com/video")
    Error(_) -> should.fail()
  }
}

pub fn validate_video_url_no_protocol_test() {
  handlers.validate_video_url("youtube.com/watch?v=123")
  |> should.be_error()
}

pub fn validate_video_url_empty_test() {
  handlers.validate_video_url("")
  |> should.be_error()
}

pub fn validate_video_url_whitespace_test() {
  handlers.validate_video_url("   ")
  |> should.be_error()
}

pub fn validate_video_url_trim_whitespace_test() {
  case handlers.validate_video_url("  https://youtube.com/watch?v=123  ") {
    Ok(url) -> url |> should.equal("https://youtube.com/watch?v=123")
    Error(_) -> should.fail()
  }
}

pub fn validate_video_url_invalid_protocol_test() {
  handlers.validate_video_url("ftp://example.com/video")
  |> should.be_error()
}

pub fn validate_video_url_empty_error_message_test() {
  case handlers.validate_video_url("") {
    Error(msg) -> msg |> should.equal("URL cannot be empty")
    Ok(_) -> should.fail()
  }
}

pub fn validate_video_url_no_protocol_error_message_test() {
  case handlers.validate_video_url("example.com") {
    Error(msg) -> msg |> should.equal("URL must start with http:// or https://")
    Ok(_) -> should.fail()
  }
}

// =============================================================================
// Handler Tests: create_job
// =============================================================================

/// Test: create_job validates URL format before inserting
pub fn create_job_validates_url_format_test() {
  let #(conn, ctx) = setup_test_db("create_job_validates")

  // Create a POST request with an invalid URL (no protocol)
  let request =
    simulate.request(http.Post, "/jobs")
    |> simulate.form_body([#("url", "youtube.com/watch?v=123")])

  let response = handlers.create_job(request, ctx)

  // Should return 400 Bad Request
  response.status |> should.equal(400)

  // Response should contain error message about URL format
  let body = simulate.read_body(response)
  body |> string.contains("Invalid URL") |> should.be_true()
  body |> string.contains("URL must start with http://") |> should.be_true()

  cleanup_test_db(conn, "create_job_validates")
}

/// Test: create_job rejects empty URL with error page
pub fn create_job_rejects_empty_url_test() {
  let #(conn, ctx) = setup_test_db("create_job_empty_url")

  // Create a POST request with empty URL
  let request =
    simulate.request(http.Post, "/jobs")
    |> simulate.form_body([#("url", "")])

  let response = handlers.create_job(request, ctx)

  // Should return 400 Bad Request
  response.status |> should.equal(400)

  // Response should contain error message about empty URL
  let body = simulate.read_body(response)
  body |> string.contains("Invalid URL") |> should.be_true()
  body |> string.contains("URL cannot be empty") |> should.be_true()

  cleanup_test_db(conn, "create_job_empty_url")
}

/// Test: create_job rejects missing URL field with error page
pub fn create_job_rejects_missing_url_test() {
  let #(conn, ctx) = setup_test_db("create_job_missing_url")

  // Create a POST request without URL field
  let request =
    simulate.request(http.Post, "/jobs")
    |> simulate.form_body([])

  let response = handlers.create_job(request, ctx)

  // Should return 400 Bad Request
  response.status |> should.equal(400)

  // Response should contain error message about missing URL
  let body = simulate.read_body(response)
  body |> string.contains("Missing URL") |> should.be_true()
  body |> string.contains("URL is required") |> should.be_true()

  cleanup_test_db(conn, "create_job_missing_url")
}

/// Test: create_job creates job and redirects to dashboard
pub fn create_job_creates_and_redirects_test() {
  let #(conn, ctx) = setup_test_db("create_job_success")

  // Create a POST request with valid URL
  let request =
    simulate.request(http.Post, "/jobs")
    |> simulate.form_body([
      #("url", "https://youtube.com/watch?v=dQw4w9WgXcQ"),
    ])

  let response = handlers.create_job(request, ctx)

  // Should return 303 See Other (redirect)
  response.status |> should.equal(303)

  // Should redirect to dashboard
  response
  |> response.get_header("location")
  |> should.equal(Ok("/"))

  // Verify job was inserted into database
  let jobs = case repo.list_jobs(conn, None) {
    Ok(j) -> j
    Error(_) -> panic as "Failed to list jobs"
  }

  list.length(jobs) |> should.equal(1)

  let job = case list.first(jobs) {
    Ok(j) -> j
    Error(_) -> panic as "No job found"
  }

  job.url |> should.equal("https://youtube.com/watch?v=dQw4w9WgXcQ")
  job.status |> should.equal(core_types.Pending)

  cleanup_test_db(conn, "create_job_success")
}

/// Test: create_job trims whitespace from URL
pub fn create_job_trims_whitespace_test() {
  let #(conn, ctx) = setup_test_db("create_job_trim")

  let request =
    simulate.request(http.Post, "/jobs")
    |> simulate.form_body([#("url", "  https://example.com/video  ")])

  let response = handlers.create_job(request, ctx)

  response.status |> should.equal(303)

  // Verify URL was trimmed
  let jobs = case repo.list_jobs(conn, None) {
    Ok(j) -> j
    Error(_) -> panic as "Failed to list jobs"
  }

  let job = case list.first(jobs) {
    Ok(j) -> j
    Error(_) -> panic as "No job found"
  }

  job.url |> should.equal("https://example.com/video")

  cleanup_test_db(conn, "create_job_trim")
}

pub fn youtube_requests_use_one_idempotent_subscription_coordinator_test() {
  let root = "/tmp/test_handlers_youtube_coordinator"
  let _ = simplifile.delete_all([root])
  let _ = simplifile.create_directory_all(root <> "/ytdl-sub")
  let channels_file = root <> "/ytdl-sub/channels.txt"
  let _ = simplifile.write(channels_file, "https://www.youtube.com/@Fireship\n")
  let previous = envoy.get("DATA_DIR")
  envoy.set("DATA_DIR", root)
  let #(conn, ctx) = setup_test_db("youtube_coordinator")
  let manager = process.new_subject()
  let ctx = middleware.Context(..ctx, subscription_manager: Some(manager))
  let request =
    simulate.request(http.Post, "/jobs")
    |> simulate.form_body([
      #("url", "https://www.youtube.com/watch?v=dQw4w9WgXcQ"),
    ])
  handlers.create_job(request, ctx)
  |> response.get_header("location")
  |> should.equal(Ok("/subscriptions"))
  process.receive(manager, 100) |> should.be_ok()
  let before =
    simplifile.read(root <> "/ytdl-sub/desired-channels.txt") |> should.be_ok()
  handlers.create_job(request, ctx).status |> should.equal(303)
  simplifile.read(root <> "/ytdl-sub/desired-channels.txt")
  |> should.equal(Ok(before))
  process.receive(manager, 10) |> should.be_error()
  repo.list_jobs(conn, None) |> should.equal(Ok([]))
  ytdl_sub.read_channels(root <> "/ytdl-sub/desired-channels.txt")
  |> should.be_ok()
  |> list.length
  |> should.equal(2)
  case previous {
    Ok(value) -> envoy.set("DATA_DIR", value)
    Error(_) -> envoy.unset("DATA_DIR")
  }
  let _ = cleanup_test_db(conn, "youtube_coordinator")
  let _ = simplifile.delete_all([root])
  Nil
}

// =============================================================================
// Handler Tests: dashboard
// =============================================================================

/// Test: dashboard returns all jobs sorted by created_at
pub fn dashboard_returns_all_jobs_sorted_test() {
  let #(conn, ctx) = setup_test_db("dashboard_sorted")

  // Insert multiple jobs with different timestamps
  let job1 = core_types.new_job_id("test-job-1")
  let job2 = core_types.new_job_id("test-job-2")
  let job3 = core_types.new_job_id("test-job-3")

  case repo.insert_job(conn, job1, "https://example.com/1", 1000) {
    Ok(_) -> Nil
    Error(_) -> panic as "Failed to insert job1"
  }
  case repo.insert_job(conn, job2, "https://example.com/2", 2000) {
    Ok(_) -> Nil
    Error(_) -> panic as "Failed to insert job2"
  }
  case repo.insert_job(conn, job3, "https://example.com/3", 3000) {
    Ok(_) -> Nil
    Error(_) -> panic as "Failed to insert job3"
  }

  let request = simulate.request(http.Get, "/")
  let response = handlers.dashboard(request, ctx)

  // Should return 200 OK
  response.status |> should.equal(200)

  // Should have HTML content-type
  response
  |> response.get_header("content-type")
  |> should.equal(Ok("text/html; charset=utf-8"))

  // Response should contain all job URLs
  let body = simulate.read_body(response)
  body |> string.contains("example.com/1") |> should.be_true()
  body |> string.contains("example.com/2") |> should.be_true()
  body |> string.contains("example.com/3") |> should.be_true()

  cleanup_test_db(conn, "dashboard_sorted")
}

/// Test: dashboard works with empty database
pub fn dashboard_empty_database_test() {
  let #(conn, ctx) = setup_test_db("dashboard_empty")

  let request = simulate.request(http.Get, "/")
  let response = handlers.dashboard(request, ctx)

  response.status |> should.equal(200)

  // Should still render the page successfully
  response
  |> response.get_header("content-type")
  |> should.equal(Ok("text/html; charset=utf-8"))

  cleanup_test_db(conn, "dashboard_empty")
}

// =============================================================================
// Handler Tests: get_job (job detail)
// =============================================================================

/// Test: job detail returns 404 for unknown job_id
/// Note: Current implementation just redirects to dashboard, but this tests the expected behavior
pub fn get_job_redirects_to_dashboard_test() {
  let #(conn, ctx) = setup_test_db("get_job_redirect")

  let request = simulate.request(http.Get, "/jobs/unknown-job-id")
  let response = handlers.get_job(request, ctx, "unknown-job-id")

  // Current implementation redirects to dashboard (303)
  response.status |> should.equal(303)

  response
  |> response.get_header("location")
  |> should.equal(Ok("/"))

  cleanup_test_db(conn, "get_job_redirect")
}

/// Test: get_job redirects for any job_id (current implementation)
pub fn get_job_redirects_for_valid_job_test() {
  let #(conn, ctx) = setup_test_db("get_job_valid")

  // Insert a job
  let job_id = core_types.new_job_id("test-job-123")
  case repo.insert_job(conn, job_id, "https://example.com", 1000) {
    Ok(_) -> Nil
    Error(_) -> panic as "Failed to insert job"
  }

  let request = simulate.request(http.Get, "/jobs/test-job-123")
  let response = handlers.get_job(request, ctx, "test-job-123")

  // Should redirect to dashboard
  response.status |> should.equal(303)

  cleanup_test_db(conn, "get_job_valid")
}

// =============================================================================
// Handler Tests: subscriptions (subscription_config GET)
// =============================================================================

/// Test: subscription_config GET renders form with current config
pub fn subscriptions_renders_config_form_test() {
  let #(conn, ctx) = setup_test_db("subscriptions_get")

  let request = simulate.request(http.Get, "/subscriptions")
  let response = handlers.subscriptions(request, ctx)

  // Should return 200 OK
  response.status |> should.equal(200)

  // Should have HTML content-type
  response
  |> response.get_header("content-type")
  |> should.equal(Ok("text/html; charset=utf-8"))

  // Response should contain form elements for subscription config
  let body = simulate.read_body(response)
  // Check for key form elements
  body |> string.contains("enabled") |> should.be_true()
  body |> string.contains("poll_interval") |> should.be_true()
  body |> string.contains("ytdl-sub") |> should.be_true()
  body |> string.contains("Subscriptions") |> should.be_true()
  body |> string.contains("href=\"/subscriptions\"") |> should.be_true()
  body |> string.contains("class=\"nav-item active\"") |> should.be_true()
  body |> string.contains("name=\"days\"") |> should.be_true()
  body |> string.contains("name=\"workers\"") |> should.be_true()
  body |> string.contains("id=\"subscription-live\"") |> should.be_true()
  body |> string.contains("setInterval(refreshLive, 4000)") |> should.be_true()
  body |> string.contains("fetch('/subscriptions/feed'") |> should.be_true()
  body |> string.contains("let liveInFlight = false") |> should.be_true()
  body
  |> string.contains("action=\"/subscriptions/channels\"")
  |> should.be_true()
  body |> string.contains("name=\"channel_text\"") |> should.be_true()
  body
  |> string.contains("action=\"/subscriptions/channels/add\"")
  |> should.be_true()
  body |> string.contains("name=\"channel_url\"") |> should.be_true()

  cleanup_test_db(conn, "subscriptions_get")
}

/// A committed subscription appears promptly with its original video title.
pub fn subscriptions_feed_uses_video_title_test() {
  let #(conn, ctx) = setup_test_db("subscriptions_feed_title")
  let media_path = "/tmp/test_subscription_feed_meta.mp4"
  let sidecar_path = media_path <> ".info.json"
  let _ =
    simplifile.write(
      sidecar_path,
      "{\"title\":\"The actual video title\",\"channel\":\"Sample Channel\",\"duration\":83,\"thumbnail\":\"https://example.com/thumb.jpg\",\"webpage_url\":\"https://www.youtube.com/watch?v=dQw4w9WgXcQ\",\"id\":\"dQw4w9WgXcQ\"}",
    )
  let video =
    DiscoveredVideo(
      video_id: "dQw4w9WgXcQ",
      channel_id: None,
      channel_name: Some("Sample Channel"),
      title: "episode filename title",
      url: "file://" <> media_path,
      published_at: None,
      duration_seconds: None,
      thumbnail_url: None,
    )
  let _ = subscription_repo.mark_seen(conn, video, True, None, None, 1000)

  let response =
    handlers.subscriptions_feed(
      simulate.request(http.Get, "/subscriptions/feed"),
      ctx,
    )
  let body = simulate.read_body(response)

  response.status |> should.equal(200)
  body |> string.contains("The actual video title") |> should.be_true()
  body |> string.contains("Sample Channel") |> should.be_true()
  body
  |> string.contains("https://www.youtube.com/watch?v=dQw4w9WgXcQ")
  |> should.be_true()
  body |> string.contains("1:23") |> should.be_true()
  body |> string.contains("https://example.com/thumb.jpg") |> should.be_true()
  body
  |> string.contains("file:///tmp/test_subscription_feed_meta.mp4")
  |> should.be_false()

  let _ = simplifile.delete(sidecar_path)
  cleanup_test_db(conn, "subscriptions_feed_title")
}

pub fn subscriptions_progress_shows_phase_totals_and_active_downloads_test() {
  let progress = [
    Snapshot(
      key: "recent-alpha",
      channel: "Alpha Channel",
      phase: "recent",
      status: "completed",
      message: "Video saved",
      percent: Some(100),
      started_at: 100,
      updated_at: 110,
      phase_total: 2,
    ),
    Snapshot(
      key: "recent-beta",
      channel: "Beta Channel",
      phase: "recent",
      status: "running",
      message: "Downloading latest upload",
      percent: Some(42),
      started_at: 105,
      updated_at: 111,
      phase_total: 2,
    ),
    Snapshot(
      key: "backfill-alpha",
      channel: "Alpha Channel",
      phase: "backfill",
      status: "running",
      message: "Downloading archive upload",
      percent: None,
      started_at: 112,
      updated_at: 112,
      phase_total: 3,
    ),
  ]
  let status =
    SubscriptionStatus(
      enabled: True,
      last_poll_at: None,
      next_poll_at: None,
      last_result: None,
      is_polling: True,
    )
  let html =
    templates.subscription_live_fragment(status, progress, 63)
    |> to_document_string

  html |> string.contains("1 / 2 finished") |> should.be_true()
  html |> string.contains("0 / 3 finished") |> should.be_true()
  html |> string.contains("Beta Channel") |> should.be_true()
  html |> string.contains("Downloading latest upload · 42%") |> should.be_true()
  html |> string.contains("Alpha Channel") |> should.be_true()
  html
  |> string.contains("63 completed videos in your library")
  |> should.be_true()
}

/// Test: subscriptions page shows default config when none exists
pub fn subscriptions_shows_default_config_test() {
  let #(conn, ctx) = setup_test_db("subscriptions_default")

  let request = simulate.request(http.Get, "/subscriptions")
  let response = handlers.subscriptions(request, ctx)

  response.status |> should.equal(200)

  // Should render successfully even with default config
  let body = simulate.read_body(response)
  body |> string.contains("Subscriptions") |> should.be_true()

  cleanup_test_db(conn, "subscriptions_default")
}

// =============================================================================
// Health endpoint tests
// =============================================================================

/// Test health endpoint returns 200 with database connected
pub fn health_endpoint_success_test() {
  let #(conn, ctx) = setup_test_db("health_success")

  let request = simulate.request(http.Get, "/health")
  let response = handlers.health(request, ctx)

  // Should return 200 status
  response.status |> should.equal(200)

  // Should have JSON content type
  response
  |> response.get_header("content-type")
  |> should.equal(Ok("application/json"))

  // Response body should contain status and database fields
  let body = simulate.read_body(response)
  body |> string.contains("\"status\"") |> should.be_true()
  body |> string.contains("\"ok\"") |> should.be_true()
  body |> string.contains("\"database\"") |> should.be_true()
  body |> string.contains("\"connected\"") |> should.be_true()

  cleanup_test_db(conn, "health_success")
}

pub fn health_detects_unresponsive_scheduler_with_connected_database_test() {
  let #(conn, ctx) = setup_test_db("health_hung_scheduler")
  let hung = process.new_subject()
  let ctx = middleware.Context(..ctx, subscription_manager: Some(hung))
  let response = handlers.health(simulate.request(http.Get, "/health"), ctx)
  response.status |> should.equal(503)
  let body = simulate.read_body(response)
  body |> string.contains("\"database\":\"connected\"") |> should.be_true()
  body |> string.contains("\"scheduler\":\"unresponsive\"") |> should.be_true()
  cleanup_test_db(conn, "health_hung_scheduler")
}
// Note: Testing disconnected database is not feasible with SQLite in-process
// because using a closed connection causes a BEAM-level badarg that isn't
// caught by Result types. In production, connection failures would typically
// occur during connection establishment, not after closure.
