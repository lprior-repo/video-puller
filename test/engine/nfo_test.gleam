/// Tests for yt-dlp info.json -> .nfo sidecar generation
import engine/nfo
import gleam/string
import gleeunit
import gleeunit/should
import simplifile

pub fn main() {
  gleeunit.main()
}

/// Ratings render with one decimal; the old io_lib-based formatter returned an
/// iolist that raised badarg and killed the downloader actor
pub fn generate_nfo_from_info_json_writes_rating_test() {
  let dir = "/tmp/test_vp_nfo"
  let path = dir <> "/video.info.json"
  let _ = simplifile.create_directory_all(dir)
  let _ =
    simplifile.write(
      path,
      "{\"id\":\"abc\",\"title\":\"Video\",\"duration\":60,"
        <> "\"view_count\":1000,\"like_count\":100}",
    )

  let nfo_path = nfo.generate_nfo_from_info_json(path) |> should.be_ok()
  let content = simplifile.read(nfo_path) |> should.be_ok()

  content |> string.contains("<value>5.0</value>") |> should.be_true()
  content |> string.contains("<votes>100</votes>") |> should.be_true()
  content |> string.contains("<views>1000</views>") |> should.be_true()
}

/// Missing counts are not fatal: the rating falls back to zero
pub fn generate_nfo_from_info_json_without_counts_test() {
  let dir = "/tmp/test_vp_nfo"
  let path = dir <> "/plain.info.json"
  let _ = simplifile.create_directory_all(dir)
  let _ = simplifile.write(path, "{\"id\":\"abc\",\"title\":\"Plain\"}")

  let nfo_path = nfo.generate_nfo_from_info_json(path) |> should.be_ok()
  let content = simplifile.read(nfo_path) |> should.be_ok()

  content |> string.contains("<value>0.0</value>") |> should.be_true()
}
