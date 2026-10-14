import gleeunit
import gleeunit/should
import infra/plex

pub fn main() {
  gleeunit.main()
}

pub fn absent_plex_watch_fields_mean_unwatched_test() {
  plex.parse_partial_watched_paths_xml(
    "<MediaContainer size=\"2\" totalSize=\"2\" offset=\"0\"><Video><Media><Part file=\"/unwatched.mp4\"/></Media></Video><Video viewOffset=\"1000\"><Media><Part file=\"/partial.mp4\"/></Media></Video></MediaContainer>",
  )
  |> should.equal(Ok(["/partial.mp4"]))
}

pub fn partial_episode_paths_include_every_part_and_dedupe_test() {
  let xml =
    "<MediaContainer size=\"3\" totalSize=\"3\" offset=\"0\"><Video ratingKey=\"1\" viewCount=\"0\" viewOffset=\"1250\"><Media><Part file=\"/library/Show/partial-a.mp4\"/><Part file=\"/library/Show/partial-b.mp4\"/></Media></Video><Video ratingKey=\"2\" viewCount=\"0\" viewOffset=\"0\"><Media><Part file=\"/library/Show/unwatched.mp4\"/></Media></Video><Video ratingKey=\"3\" viewCount=\"1\" viewOffset=\"1250\"><Media><Part file=\"/library/Show/watched.mp4\"/></Media></Video></MediaContainer>"

  plex.parse_partial_watched_paths_xml(xml)
  |> should.equal(
    Ok([
      "/library/Show/partial-a.mp4",
      "/library/Show/partial-b.mp4",
    ]),
  )
}

pub fn partial_episode_parser_rejects_bad_container_and_malformed_rows_test() {
  plex.parse_partial_watched_paths_xml("<html><Video/></html>")
  |> should.be_error
  plex.parse_partial_watched_paths_xml("<MediaContainer>") |> should.be_error
  plex.parse_partial_watched_paths_xml(
    "<MediaContainer size=\"1\" totalSize=\"2\" offset=\"0\"><Video viewCount=\"0\" viewOffset=\"1\"><Media><Part file=\"/library/partial.mp4\"/></Media></Video></MediaContainer>",
  )
  |> should.be_error
  plex.parse_partial_watched_paths_xml(
    "<MediaContainer size=\"1\" totalSize=\"1\" offset=\"0\"><Video viewCount=\"0\" viewOffset=\"1\"><Media><Part/></Media></Video></MediaContainer>",
  )
  |> should.be_error
  plex.parse_partial_watched_paths_xml(
    "<MediaContainer size=\"1\" totalSize=\"1\" offset=\"0\"><Video viewCount=\"sometimes\" viewOffset=\"1\"><Media><Part file=\"/library/partial.mp4\"/></Media></Video></MediaContainer>",
  )
  |> should.be_error
}

pub fn active_session_parser_protects_all_media_part_paths_test() {
  let xml =
    "<MediaContainer size=\"2\"><Video ratingKey=\"1\"><Media><Part file=\"/library/Show/active.mp4\"/></Media></Video><Track ratingKey=\"2\"><Media><Part file=\"/library/Show/active-a.m4a\"/><Part file=\"/library/Show/active-b.m4a\"/></Media></Track></MediaContainer>"

  plex.parse_active_paths_xml(xml)
  |> should.equal(
    Ok([
      "/library/Show/active-a.m4a",
      "/library/Show/active-b.m4a",
      "/library/Show/active.mp4",
    ]),
  )
}

pub fn active_session_missing_file_or_parts_blocks_cleanup_test() {
  plex.parse_active_paths_xml(
    "<MediaContainer size=\"1\"><Video><Media><Part/></Media></Video></MediaContainer>",
  )
  |> should.be_error()
  plex.parse_active_paths_xml(
    "<MediaContainer size=\"1\"><Video/></MediaContainer>",
  )
  |> should.be_error()
}
