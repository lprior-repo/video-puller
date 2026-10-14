import envoy
import gleam/option.{None, Some}
import gleeunit
import gleeunit/should
import infra/plex
import infra/plex_scan_policy

pub fn main() {
  gleeunit.main()
}

pub fn ytdl_sub_info_sidecar_uses_media_stem_test() {
  plex.info_sidecar_path(
    "/library/Channel/Season 2026/s2026.e091101 - Video Title.mp4",
  )
  |> should.equal(
    "/library/Channel/Season 2026/s2026.e091101 - Video Title.info.json",
  )
}

pub fn info_sidecar_uses_last_known_media_extension_test() {
  plex.info_sidecar_path("/library/Show/Title.mkv")
  |> should.equal("/library/Show/Title.info.json")
}

pub fn completed_download_missing_from_plex_requests_rescan_test() {
  plex.needs_scan(["old.mp4", "new.mp4"], ["old.mp4"]) |> should.be_true()
  plex.needs_scan(["new.mp4", "old.mp4"], ["old.mp4", "new.mp4", "retired.mp4"])
  |> should.be_false()
  plex.needs_scan([], []) |> should.be_false()
}

pub fn scan_policy_defaults_and_bounds_configured_cooldown_test() {
  plex_scan_policy.configured_cooldown_ms(None) |> should.equal(300_000)
  plex_scan_policy.configured_cooldown_ms(Some("invalid"))
  |> should.equal(300_000)
  plex_scan_policy.configured_cooldown_ms(Some("20"))
  |> should.equal(30_000)
  plex_scan_policy.configured_cooldown_ms(Some("600"))
  |> should.equal(600_000)
  plex_scan_policy.configured_cooldown_ms(Some("9999"))
  |> should.equal(3_600_000)
}

pub fn parse_video_xml_extracts_rating_key_path_and_title_test() {
  let xml =
    "<MediaContainer size=\"1\"><Video ratingKey=\"913\" title=\"Old Title\"><Media><Part id=\"1\" file=\"/library/Channel/Season 2026/episode.mp4\" /></Media></Video></MediaContainer>"

  plex.parse_videos_xml(xml)
  |> should.equal([
    #("913", "/library/Channel/Season 2026/episode.mp4", "Old Title"),
  ])
}

pub fn parse_video_xml_preserves_unicode_title_and_file_path_test() {
  let xml =
    "<MediaContainer size=\"1\"><Video ratingKey=\"914\" title=\"L’école — ＡＢＣ\"><Media><Part id=\"2\" file=\"/library/Channel/Season 2026/café-🍎.mp4\" /></Media></Video></MediaContainer>"

  plex.parse_videos_xml(xml)
  |> should.equal([
    #("914", "/library/Channel/Season 2026/café-🍎.mp4", "L’école — ＡＢＣ"),
  ])
}

pub fn parse_watched_paths_only_includes_positive_view_count_test() {
  let xml =
    "<MediaContainer size=\"3\"><Video ratingKey=\"1\" viewCount=\"1\"><Media><Part file=\"/library/Show/Season 1/watched.mp4\" /></Media></Video><Video ratingKey=\"2\" viewCount=\"0\"><Media><Part file=\"/library/Show/Season 1/unwatched.mp4\" /></Media></Video><Video ratingKey=\"3\"><Media><Part file=\"/library/Show/Season 1/unknown.mp4\" /></Media></Video></MediaContainer>"

  plex.parse_watched_paths_xml(xml)
  |> should.equal(["/library/Show/Season 1/watched.mp4"])
}

pub fn parse_watched_paths_deduplicates_multiple_plex_rows_test() {
  let xml =
    "<MediaContainer><Video ratingKey=\"1\" viewCount=\"2\"><Media><Part file=\"/library/Show/Season 1/watched.mp4\" /></Media></Video><Video ratingKey=\"2\" viewCount=\"3\"><Media><Part file=\"/library/Show/Season 1/watched.mp4\" /></Media></Video></MediaContainer>"

  plex.parse_watched_paths_xml(xml)
  |> should.equal(["/library/Show/Season 1/watched.mp4"])
}

pub fn parse_active_session_paths_includes_every_part_file_test() {
  let xml =
    "<MediaContainer size=\"2\"><Video ratingKey=\"1\"><Media><Part file=\"/library/Show/active.mp4\" /></Media></Video><Track ratingKey=\"2\"><Media><Part file=\"/library/Show/active-audio.m4a\" /><Part file=\"/library/Show/café-🍎.m4a\" /></Media></Track></MediaContainer>"

  plex.parse_active_paths_xml(xml)
  |> should.equal(
    Ok([
      "/library/Show/active-audio.m4a",
      "/library/Show/active.mp4",
      "/library/Show/café-🍎.m4a",
    ]),
  )
}

pub fn malformed_or_non_container_sessions_xml_fails_closed_test() {
  plex.parse_active_paths_xml("<MediaContainer>") |> should.be_error()
  plex.parse_active_paths_xml("<html><Part file=\"/tmp/file.mp4\" /></html>")
  |> should.be_error()
}

pub fn watched_candidates_exclude_all_active_session_paths_test() {
  plex.exclude_active_paths(
    ["/library/Show/watched.mp4", "/library/Show/active.mp4"],
    ["/library/Show/active.mp4", "/library/Show/other.mkv"],
  )
  |> should.equal(["/library/Show/watched.mp4"])
}

pub fn disabled_plex_configuration_returns_none_through_public_api_test() {
  let previous = envoy.get("PLEX_SECTION_ID")
  let _ = envoy.set("PLEX_SECTION_ID", "")
  let result = plex.watched_video_paths_result()
  case previous {
    Ok(value) -> envoy.set("PLEX_SECTION_ID", value)
    Error(_) -> envoy.unset("PLEX_SECTION_ID")
  }

  result |> should.equal(Ok(None))
}

pub fn managed_section_parser_requires_exact_key_and_known_scan_state_test() {
  let xml =
    "<MediaContainer size=\"1\"><Directory key=\"4\" type=\"show\" refreshing=\"0\"><Location path=\"/Volumes/YouTube SSD/YouTube/library\"/></Directory></MediaContainer>"
  plex.parse_managed_section_xml(xml, "4")
  |> should.equal(
    Ok(#("show", False, ["/Volumes/YouTube SSD/YouTube/library"])),
  )
  plex.parse_managed_section_xml(
    "<MediaContainer><Directory key=\"4\" type=\"show\"><Location path=\"/x\"/></Directory></MediaContainer>",
    "4",
  )
  |> should.be_error
  plex.parse_managed_section_xml(xml, "5") |> should.be_error
}

pub fn section_paths_parser_requires_each_video_part_path_test() {
  let xml =
    "<MediaContainer size=\"1\" totalSize=\"1\" offset=\"0\"><Video ratingKey=\"1\"><Media><Part file=\"/Volumes/YouTube SSD/YouTube/library/Artist/Season 2026/One.mp4\"/></Media></Video></MediaContainer>"
  plex.parse_section_paths_xml(xml)
  |> should.equal(
    Ok([
      "/Volumes/YouTube SSD/YouTube/library/Artist/Season 2026/One.mp4",
    ]),
  )
  plex.parse_section_paths_xml(
    "<MediaContainer size=\"1\"><Video ratingKey=\"1\"><Media/></Video></MediaContainer>",
  )
  |> should.be_error
  plex.parse_section_paths_xml("not XML") |> should.be_error
  plex.parse_section_paths_xml(
    "<MediaContainer size=\"1\" totalSize=\"2\" offset=\"0\"><Video ratingKey=\"1\"><Media><Part file=\"/Volumes/YouTube SSD/YouTube/library/Artist/one.mp4\"/></Media></Video></MediaContainer>",
  )
  |> should.be_error
  plex.parse_section_paths_xml(
    "<MediaContainer size=\"1\" totalSize=\"1\" offset=\"0\"><Video ratingKey=\"1\"><Media><Part file=\"/Volumes/YouTube SSD/YouTube/library/Artist/one.mp4\"/><Part/></Media></Video></MediaContainer>",
  )
  |> should.be_error
}
