import domain/subscription_types.{SeenVideo, SubscriptionStatus, default_config}
import engine/subscription_progress.{Snapshot}
import engine/subscription_settings.{Settings}
import gleam/option.{None, Some}
import gleam/string
import gleeunit
import gleeunit/should
import lustre/element.{to_document_string}
import lustre/element/html
import web/templates.{FeedCard}

pub fn main() {
  gleeunit.main()
}

pub fn renders_plex_primary_link_and_real_video_metadata_test() {
  let video =
    SeenVideo(
      video_id: "dQw4w9WgXcQ",
      channel_id: Some("channel-id"),
      channel_name: Some("Sample Channel"),
      title: "The actual video title",
      url: "https://www.youtube.com/watch?v=dQw4w9WgXcQ",
      published_at: Some(1_792_000_000),
      duration_seconds: Some(83),
      thumbnail_url: Some("https://example.com/thumb.jpg"),
      first_seen_at: 1_792_000_000,
      downloaded: True,
      skipped: False,
      skip_reason: None,
      job_id: None,
    )
  let card =
    FeedCard(
      video: video,
      plex_url: Some("http://plex.local/web/index.html#!/media/video-key"),
      description: Some("A real description from the cached sidecar."),
      view_count: Some(12_345),
    )
  let body = templates.subscription_feed_list([card]) |> to_document_string

  body |> string.contains("class=\"feed-grid list-none\"") |> should.be_true()
  body |> string.contains("feed-thumbnail-frame") |> should.be_true()
  body |> string.contains("1:23") |> should.be_true()
  body |> string.contains("The actual video title") |> should.be_true()
  body |> string.contains("Sample Channel") |> should.be_true()
  body |> string.contains("12345 views") |> should.be_true()
  body
  |> string.contains("http://plex.local/web/index.html#!/media/video-key")
  |> should.be_true()
  body |> string.contains("YouTube ↗") |> should.be_true()
  body
  |> string.contains("A real description from the cached sidecar.")
  |> should.be_true()
}

pub fn missing_optional_metadata_is_omitted_and_source_is_fallback_test() {
  let card =
    FeedCard(
      video: SeenVideo(
        video_id: "dQw4w9WgXcQ",
        channel_id: None,
        channel_name: Some("Sample Channel"),
        title: "Video without cached extras",
        url: "https://www.youtube.com/watch?v=dQw4w9WgXcQ",
        published_at: None,
        duration_seconds: None,
        thumbnail_url: None,
        first_seen_at: 1_792_000_000,
        downloaded: True,
        skipped: False,
        skip_reason: None,
        job_id: None,
      ),
      plex_url: None,
      description: None,
      view_count: None,
    )
  let body = templates.subscription_feed_list([card]) |> to_document_string

  body |> string.contains("Video without cached extras") |> should.be_true()
  body
  |> string.contains("https://www.youtube.com/watch?v=dQw4w9WgXcQ")
  |> should.be_true()
  body |> string.contains("views") |> should.be_false()
  body |> string.contains("Description") |> should.be_false()
  body |> string.contains("No thumbnail") |> should.be_true()
}

pub fn live_progress_starts_collapsed_beside_compact_status_test() {
  let status =
    SubscriptionStatus(
      enabled: True,
      last_poll_at: Some(1_792_000_000),
      next_poll_at: None,
      last_result: None,
      is_polling: True,
    )
  let progress = [
    Snapshot(
      key: "recent-channel",
      channel: "Sample Channel",
      phase: "recent",
      status: "running",
      message: "Downloading recent uploads",
      percent: Some(34),
      phase_total: 8,
      started_at: 1_792_000_000,
      updated_at: 1_792_000_010,
    ),
  ]
  let body =
    templates.subscription_live_fragment(status, progress, 112)
    |> to_document_string

  body
  |> string.contains("class=\"subscription-status-panel\"")
  |> should.be_true()
  body |> string.contains("data-default-open=\"false\"") |> should.be_true()
  body
  |> string.contains("data-live-details=\"poll-progress\" open")
  |> should.be_false()
  body |> string.contains("1 active · 0 queued") |> should.be_true()
}

pub fn saved_progress_is_marked_interrupted_when_poll_is_idle_test() {
  let status =
    SubscriptionStatus(
      enabled: False,
      last_poll_at: Some(1_792_000_000),
      next_poll_at: None,
      last_result: None,
      is_polling: False,
    )
  let progress = [
    Snapshot(
      key: "recent-running",
      channel: "Running channel",
      phase: "recent",
      status: "running",
      message: "Downloading",
      percent: Some(34),
      phase_total: 8,
      started_at: 1_792_000_000,
      updated_at: 1_792_000_010,
    ),
    Snapshot(
      key: "recent-queued",
      channel: "Queued channel",
      phase: "recent",
      status: "queued",
      message: "Waiting",
      percent: None,
      phase_total: 8,
      started_at: 1_792_000_000,
      updated_at: 1_792_000_010,
    ),
  ]
  let body =
    templates.subscription_live_fragment(status, progress, 112)
    |> to_document_string

  body
  |> string.contains("2 interrupted · poll is not running")
  |> should.be_true()
  body |> string.contains("0 active · 2 queued") |> should.be_false()
  body |> string.contains("2 failed/stale") |> should.be_true()
}

pub fn live_fragment_is_compact_and_feed_loads_separately_test() {
  let video =
    SeenVideo(
      video_id: "dQw4w9WgXcQ",
      channel_id: None,
      channel_name: Some("Sample Channel"),
      title: "Video title",
      url: "https://www.youtube.com/watch?v=dQw4w9WgXcQ",
      published_at: Some(1_792_000_000),
      duration_seconds: Some(83),
      thumbnail_url: Some("https://example.com/thumb.jpg"),
      first_seen_at: 1_792_000_000,
      downloaded: True,
      skipped: False,
      skip_reason: None,
      job_id: None,
    )
  let status =
    SubscriptionStatus(
      enabled: False,
      last_poll_at: None,
      next_poll_at: None,
      last_result: None,
      is_polling: False,
    )
  let live =
    templates.subscription_live_fragment(status, [], 1) |> to_document_string
  let feed =
    templates.subscription_feed_fragment(
      [FeedCard(video, None, Some("Long description"), None)],
      1,
    )
    |> to_document_string

  live |> string.contains("data-completed-count=\"1\"") |> should.be_true()
  live |> string.contains("Video title") |> should.be_false()
  live |> string.contains("Long description") |> should.be_false()
  feed |> string.contains("Video title") |> should.be_true()
  feed |> string.contains("Long description") |> should.be_true()
  feed |> string.contains("data-completed-count=\"1\"") |> should.be_true()
}

pub fn subscriptions_page_includes_watched_cleanup_note_and_download_nav_test() {
  let status =
    SubscriptionStatus(
      enabled: False,
      last_poll_at: None,
      next_poll_at: None,
      last_result: None,
      is_polling: False,
    )
  let page =
    templates.subscriptions_page(
      default_config(),
      status,
      "/library",
      Settings(workers: 8, days: 30, skip_restricted: True),
      "2026.10.1",
      None,
      [],
      112,
    )
    |> to_document_string
  let nav = templates.layout("Download", html.div([], []))

  page
  |> string.contains(
    "Watched-video cleanup runs from the configured Plex library",
  )
  |> should.be_true()
  page |> string.contains("href=\"#subscription-settings\"") |> should.be_true()
  page |> string.contains("id=\"subscription-settings\"") |> should.be_true()
  page
  |> string.contains("id=\"subscription-controls\" open")
  |> should.be_false()
  page |> string.contains("112 completed videos") |> should.be_true()
  nav |> string.contains("href=\"/download\">Download") |> should.be_true()
}
