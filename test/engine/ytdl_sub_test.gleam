/// Tests for the ytdl-sub engine wrapper
///
/// Pins the contract with the installed engine: the generated subscriptions
/// file shape, the channel label, transaction-log parsing and the library
/// path mapping.
import engine/ytdl_sub
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import gleeunit
import gleeunit/should
import simplifile

pub fn main() {
  gleeunit.main()
}

// =============================================================================
// Channel labels
// =============================================================================

pub fn subscription_label_uses_handle_test() {
  ytdl_sub.subscription_label("https://www.youtube.com/@Fireship")
  |> should.equal("Fireship")
}

pub fn subscription_label_ignores_tab_and_query_test() {
  ytdl_sub.subscription_label("https://www.youtube.com/@Fireship/videos")
  |> should.equal("Fireship")

  ytdl_sub.subscription_label("https://www.youtube.com/@Fireship/streams?x=1")
  |> should.equal("Fireship")
}

pub fn subscription_label_handles_channel_urls_test() {
  ytdl_sub.subscription_label("https://www.youtube.com/channel/UCabc123/")
  |> should.equal("UCabc123")
}

// =============================================================================
// Generated subscriptions file
// =============================================================================

pub fn write_subscriptions_uses_supported_presets_test() {
  let root = "/tmp/test_ytdl_sub_config"
  let layout = ytdl_sub.layout(root)

  let _ = simplifile.create_directory_all(root <> "/ytdl-sub")

  ytdl_sub.write_subscriptions(layout, [
    "https://www.youtube.com/@Fireship",
    "https://www.youtube.com/@Computerphile",
  ])
  |> should.be_ok()

  let content = simplifile.read(layout.subscriptions_file) |> should.be_ok()

  // Channel directory for downloads
  content
  |> string.contains("tv_show_directory: \"" <> layout.library_dir <> "\"")
  |> should.be_true()

  // Show preset with no date window: the pull takes the channel's full history
  // and ytdl-sub's download archive keeps later polls incremental
  content |> string.contains("Plex TV Show by Date:") |> should.be_true()

  content
  |> string.contains("only_recent_date_range")
  |> should.be_false()

  // A bare `preset:` block inside the show group is read as a subscription
  // URL by the engine and injects a phantom error into every poll
  content |> string.contains("  preset:") |> should.be_false()

  // One YouTube URL entry per configured channel
  content
  |> string.contains("\"Fireship\": \"https://www.youtube.com/@Fireship\"")
  |> should.be_true()

  content
  |> string.contains(
    "\"Computerphile\": \"https://www.youtube.com/@Computerphile\"",
  )
  |> should.be_true()

  let _ = simplifile.delete(layout.subscriptions_file)
  let _ = simplifile.delete(root <> "/ytdl-sub")
  let _ = simplifile.delete(root)
}

// =============================================================================
// Transaction log parsing
// =============================================================================

/// Fixture copied from a real `ytdl-sub sub` run (2026.8.26), including the
/// per-file detail block that contains text, blank lines and URLs.
const real_transaction_log =
  "[ytdl-sub] Validating subscriptions...
[ytdl-sub] Transaction log for Fireship:
Files created:
----------------------------------------
/var/library/Fireship
  .ytdl-sub-Fireship-download-archive.json
/var/library/Fireship/Season 2026
  s2026.e033101 - Video One.mp4
    Video Tags:
      contentRating: TV-14
      date: 2026-03-31
      synopsis:
        https://www.youtube.com/watch?v=o7NYXvYohYk

        A description with links - https://mux.com/fireship

        - https://www.stepsecurity.io/blog/axios-compromised
      title: 2026-03-31 - Video One
      year: 2026
/var/library/Channel Two
  s2026.e033102 - Video Two.mkv
  s2026.e033102 - Video Two-thumb.jpg
[ytdl-sub] Download Summary:
Fireship     +1 0 0     1 ✔
Total: 1     +1 0 0     1 Success
"

pub fn parse_pull_output_reads_created_media_test() {
  let summary = ytdl_sub.parse_pull_output(real_transaction_log)

  summary.downloaded |> should.equal(2)
  summary.added_files
  |> should.equal([
    "/var/library/Fireship/Season 2026/s2026.e033101 - Video One.mp4",
    "/var/library/Channel Two/s2026.e033102 - Video Two.mkv",
  ])
  summary.errors |> should.equal([])
}

/// A file line outside a "Files created:" section is not a download
pub fn parse_pull_output_ignores_files_outside_section_test() {
  let output =
    "Files created:
----------------------------------------
/var/library/Channel One
  Video One.mp4
[ytdl-sub] Download Summary:
/var/library/Channel One
  Video Two.mp4
"

  let summary = ytdl_sub.parse_pull_output(output)

  summary.added_files
  |> should.equal(["/var/library/Channel One/Video One.mp4"])
}

pub fn parse_pull_output_reports_errors_test() {
  let output =
    "[ytdl-sub] Validating subscriptions...
[ytdl-sub:yt-dlp] ERROR: [youtube] abc: Requested format is not available
[ytdl-sub:yt-dlp] ERROR: [youtube] def: Video unavailable
"

  let summary = ytdl_sub.parse_pull_output(output)

  summary.downloaded |> should.equal(0)
  list.length(summary.errors) |> should.equal(2)
}

pub fn parse_pull_output_empty_test() {
  let summary = ytdl_sub.parse_pull_output("")

  summary.downloaded |> should.equal(0)
  summary.added_files |> should.equal([])
}

// =============================================================================
// Library path mapping
// =============================================================================

pub fn to_discovered_video_maps_episode_file_test() {
  let library = "/library"
  let video =
    ytdl_sub.to_discovered_video(
      library,
      library <> "/Channel One/Season 2026/s2026.e091101 - Video One.mp4",
    )

  video.channel_name |> should.equal(Some("Channel One"))
  video.title |> should.equal("Video One")
  // 2026-09-11 as a Unix timestamp
  video.published_at |> should.equal(Some(1_789_084_800))
  video.url
  |> should.equal(
    "file:///library/Channel One/Season 2026/s2026.e091101 - Video One.mp4",
  )
}

pub fn to_discovered_video_without_episode_prefix_test() {
  let library = "/library"
  let video =
    ytdl_sub.to_discovered_video(
      library,
      library <> "/Channel One/Some Video.mp4",
    )

  video.channel_name |> should.equal(Some("Channel One"))
  video.title |> should.equal("Some Video")
  video.published_at |> should.equal(None)
}

pub fn parse_episode_date_rejects_bad_season_test() {
  ytdl_sub.parse_episode_date("s2026.e131101 - Video")
  |> should.equal(None)

  ytdl_sub.parse_episode_date("s2026.e091101 - Video")
  |> should.equal(Some(1_789_084_800))

  ytdl_sub.parse_episode_date("short")
  |> should.equal(None)
}

// =============================================================================
// Generated subscriptions file: throttle-protection overrides
// =============================================================================

pub fn write_subscriptions_disables_resolution_assert_test() {
  let root = "/tmp/test_ytdl_sub_assert"
  let layout = ytdl_sub.layout(root)

  let _ = simplifile.create_directory_all(root <> "/ytdl-sub")

  ytdl_sub.write_subscriptions(layout, ["https://www.youtube.com/@Fireship"])
  |> should.be_ok()

  let content = simplifile.read(layout.subscriptions_file) |> should.be_ok()

  // The engine's resolution assert aborts a subscription whenever it touches
  // a genuinely low-res upload (below 361p), which is not throttling
  content
  |> string.contains("enable_resolution_assert: False")
  |> should.be_true()

  let _ = simplifile.delete(layout.subscriptions_file)
  let _ = simplifile.delete(root <> "/ytdl-sub")
  let _ = simplifile.delete(root)
}

// =============================================================================
// Run outcome interpretation
// =============================================================================

/// Fixture: one subscription downloaded, one failed, engine exit code 1
const partial_failure_output =
  "[ytdl-sub] Transaction log for Good Channel:
Files created:
----------------------------------------
/var/library/Good Channel/Season 2026
  s2026.e033101 - Video One.mp4
[ytdl-sub] Download Summary:
Good Channel +1 0 0     1 ✔
Bad Channel  0 0 0     0 ERROR: [youtube] abc: Video unavailable
Total: 2      +1 0 0     1 Error
"

pub fn interpret_run_result_accepts_success_test() {
  let output =
    "Files created:
----------------------------------------
/var/library/Channel One
  Video One.mp4
"

  let summary = ytdl_sub.interpret_run_result(0, output) |> should.be_ok()

  summary.downloaded |> should.equal(1)
  summary.errors |> should.equal([])
}

/// A non-zero exit keeps the downloads that did land, with failures reported
pub fn interpret_run_result_keeps_downloads_on_partial_failure_test() {
  let summary =
    ytdl_sub.interpret_run_result(1, partial_failure_output) |> should.be_ok()

  summary.downloaded |> should.equal(1)
  summary.added_files
  |> should.equal([
    "/var/library/Good Channel/Season 2026/s2026.e033101 - Video One.mp4",
  ])
  list.length(summary.errors) |> should.equal(1)
}

pub fn interpret_run_result_fails_when_nothing_downloaded_test() {
  let message =
    ytdl_sub.interpret_run_result(
      1,
      "[ytdl-sub:yt-dlp] ERROR: [youtube] abc: Video unavailable\n",
    )
    |> should.be_error()

  string.contains(message, "exit code 1") |> should.be_true()
}
