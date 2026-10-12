import domain/subscription_types.{SeenVideo}
import engine/ytdl_sub
import envoy
import gleam/list
import gleam/option.{None, Some}
import gleeunit/should
import simplifile
import web/library_feed

pub fn cards_use_local_metadata_and_individual_plex_index_test() {
  let root = "/tmp/test_library_feed_cache"
  let _ = simplifile.delete_all([root])
  let folder = root <> "/library/Channel/Season 2026"
  let _ = simplifile.create_directory_all(folder)
  let _ = simplifile.create_directory_all(root <> "/cache")
  let path = folder <> "/video.mp4"
  let _ = simplifile.write(path, "fixture")
  let _ =
    simplifile.write(
      folder <> "/video.info.json",
      "{\"id\":\"abc123DEF_-\",\"description\":\"Original description\",\"view_count\":12345}",
    )
  let _ =
    simplifile.write(
      root <> "/cache/plex-index.json",
      "{\"" <> path <> "\":\"123\"}",
    )
  let previous_root = envoy.get("DATA_DIR")
  let previous_machine = envoy.get("PLEX_MACHINE_ID")
  envoy.set("DATA_DIR", root)
  envoy.set("PLEX_MACHINE_ID", "fixture-server")
  let video =
    SeenVideo(
      video_id: "abc123DEF_-",
      channel_id: None,
      channel_name: Some("Channel"),
      title: "Original title",
      url: "https://www.youtube.com/watch?v=abc123DEF_-",
      published_at: Some(100),
      duration_seconds: Some(120),
      thumbnail_url: None,
      first_seen_at: 200,
      downloaded: True,
      skipped: False,
      skip_reason: None,
      job_id: None,
    )
  let cards = library_feed.cards(ytdl_sub.layout(root), [video])
  let card = list.first(cards) |> should.be_ok()
  card.description |> should.equal(Some("Original description"))
  card.view_count |> should.equal(Some(12_345))
  card.plex_url
  |> should.equal(Some(
    "http://127.0.0.1:32400/web/index.html#!/server/fixture-server/details?key=%2Flibrary%2Fmetadata%2F123",
  ))
  case previous_root {
    Ok(value) -> envoy.set("DATA_DIR", value)
    Error(_) -> envoy.unset("DATA_DIR")
  }
  case previous_machine {
    Ok(value) -> envoy.set("PLEX_MACHINE_ID", value)
    Error(_) -> envoy.unset("PLEX_MACHINE_ID")
  }
  let _ = simplifile.delete_all([root])
  Nil
}
