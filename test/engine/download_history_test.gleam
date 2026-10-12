import engine/download_history
import gleam/list
import gleam/string
import gleeunit
import gleeunit/should
import simplifile

pub fn main() {
  gleeunit.main()
}

pub fn durable_remember_is_idempotent_and_snapshot_is_private_test() {
  let root = "/tmp/test_download_history_durable"
  let _ = simplifile.delete_all([root])
  let id = "a1b2c3d4e5F"
  download_history.remember(root, id) |> should.be_ok
  download_history.remember(root, id) |> should.be_ok
  download_history.known_ids(root) |> should.be_ok |> should.equal([id])

  let snapshot = root <> "/ytdl-sub/jobs/private.txt"
  download_history.write_snapshot(root, snapshot) |> should.be_ok
  simplifile.read(snapshot)
  |> should.be_ok
  |> should.equal("youtube " <> id <> "\n")
  simplifile.write(snapshot, "youtube z9y8x7w6v5U\n") |> should.be_ok
  download_history.known_ids(root) |> should.be_ok |> should.equal([id])
}

pub fn refresh_imports_valid_completed_media_and_ytdl_sub_archives_test() {
  let root = "/tmp/test_download_history_refresh"
  let _ = simplifile.delete_all([root])
  let show = root <> "/library/Live Show/Season 2026"
  let retired = root <> "/retired-library/Old Show/Season 2025"
  simplifile.create_directory_all(show) |> should.be_ok
  simplifile.create_directory_all(retired) |> should.be_ok
  simplifile.create_directory_all(root <> "/cache/video-metadata")
  |> should.be_ok

  let media = show <> "/s2026.e1012 - Live.mp4"
  simplifile.write(media, "complete") |> should.be_ok
  simplifile.write(
    show <> "/s2026.e1012 - Live.info.json",
    "{\"id\":\"abcde12345Z\",\"title\":\"Live\"}",
  )
  |> should.be_ok
  simplifile.write(
    root <> "/library/Live Show/.ytdl-sub-Live-download-archive.json",
    "{\"zzzzz12345Z\":{\"upload_date\":\"2026-10-12\",\"extractor\":\"youtube\",\"file_names\":[\"x\"]}}",
  )
  |> should.be_ok
  simplifile.write(
    root <> "/retired-library/Old Show/.ytdl-sub-Old-download-archive.json",
    "{\"yyyyy12345Y\":{\"upload_date\":\"2025-10-12\",\"extractor\":\"YouTube\",\"file_names\":[]}}",
  )
  |> should.be_ok
  simplifile.write(
    root <> "/library/Live Show/.ytdl-sub-Other-download-archive.json",
    "{\"xxxxx12345X\":{\"extractor\":\"vimeo\",\"file_names\":[]}}",
  )
  |> should.be_ok

  download_history.refresh(root) |> should.be_ok
  download_history.known_ids(root)
  |> should.be_ok
  |> list.sort(by: string.compare)
  |> should.equal(["abcde12345Z", "yyyyy12345Y", "zzzzz12345Z"])
}

pub fn invalid_ids_are_rejected_test() {
  let root = "/tmp/test_download_history_bad"
  let _ = simplifile.delete_all([root])
  download_history.remember(root, "not-a-video-id") |> should.be_error
}
