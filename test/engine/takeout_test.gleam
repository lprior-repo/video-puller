/// Tests for the Google Takeout subscription import
import engine/takeout
import engine/ytdl_sub
import gleam/string
import gleeunit
import gleeunit/should
import simplifile

pub fn main() {
  gleeunit.main()
}

/// Current Takeout layout, including a quoted title containing a comma
const takeout_csv =
  "Channel Id,Channel Url,Channel Title
UCabc123,http://www.youtube.com/channel/UCabc123,Channel One
UCdef456,https://www.youtube.com/@Two,\"Two, with comma\"
"

pub fn parse_takeout_urls_test() {
  takeout.parse_subscription_urls(takeout_csv)
  |> should.equal([
    "http://www.youtube.com/channel/UCabc123",
    "https://www.youtube.com/@Two",
  ])
}

pub fn parse_takeout_handles_legacy_order_and_blanks_test() {
  let content =
    "\"UCaaa\",\"Channel A\",\"https://www.youtube.com/channel/UCaaa\"\n\n\"UCbbb\",\"Channel B\",\"https://www.youtube.com/@B\"\r\n"

  takeout.parse_subscription_urls(content)
  |> should.equal([
    "https://www.youtube.com/channel/UCaaa",
    "https://www.youtube.com/@B",
  ])
}

pub fn parse_takeout_skips_rows_without_url_test() {
  takeout.parse_subscription_urls(
    "Channel Id,Channel Url,Channel Title\nno,url,here\n",
  )
  |> should.equal([])
}

pub fn merge_channels_appends_only_missing_test() {
  let existing = "# my channels\nhttps://www.youtube.com/@One\n"

  let #(content, added) =
    takeout.merge_channels(existing, [
      "https://www.youtube.com/@One/",
      "https://www.youtube.com/@Two",
      "https://www.youtube.com/@Two",
    ])

  added |> should.equal(1)
  content
  |> should.equal(
    "# my channels\nhttps://www.youtube.com/@One\nhttps://www.youtube.com/@Two\n",
  )
}

pub fn merge_channels_noop_without_new_urls_test() {
  let existing = "https://www.youtube.com/@One\n"

  let #(content, added) =
    takeout.merge_channels(existing, ["https://www.youtube.com/@One"])

  added |> should.equal(0)
  content |> should.equal(existing)
}

pub fn import_if_present_merges_and_marks_csv_test() {
  let root = "/tmp/test_takeout_import"
  let layout = ytdl_sub.layout(root)

  let _ = simplifile.create_directory_all(root <> "/ytdl-sub")
  let _ =
    simplifile.write(layout.channels_file, "https://www.youtube.com/@One\n")
  let _ = simplifile.write(layout.takeout_file, takeout_csv)

  let summary =
    takeout.import_if_present(layout) |> should.be_ok() |> should.be_some()

  summary.added |> should.equal(2)
  summary.found |> should.equal(2)

  simplifile.read(layout.channels_file)
  |> should.be_ok()
  |> should.equal(
    "https://www.youtube.com/@One\nhttp://www.youtube.com/channel/UCabc123\nhttps://www.youtube.com/@Two\n",
  )

  simplifile.is_file(layout.takeout_file <> ".imported")
  |> should.be_ok()
  |> should.be_true()

  // The export is renamed, so a second run has nothing to import
  takeout.import_if_present(layout)
  |> should.be_ok()
  |> should.be_none()

  let _ = simplifile.delete(layout.channels_file)
  let _ = simplifile.delete(layout.takeout_file <> ".imported")
  let _ = simplifile.delete(root <> "/ytdl-sub")
  let _ = simplifile.delete(root)
}

pub fn import_without_urls_fails_and_keeps_csv_test() {
  let root = "/tmp/test_takeout_no_urls"
  let layout = ytdl_sub.layout(root)

  let _ = simplifile.create_directory_all(root <> "/ytdl-sub")
  let _ = simplifile.write(layout.channels_file, "")
  let _ =
    simplifile.write(layout.takeout_file, "Channel Id,Channel Url\nnone,none\n")

  let message = takeout.import_if_present(layout) |> should.be_error()

  string.contains(message, "no channel URLs") |> should.be_true()

  // Kept in place so the export can be corrected
  simplifile.is_file(layout.takeout_file)
  |> should.be_ok()
  |> should.be_true()

  let _ = simplifile.delete(layout.channels_file)
  let _ = simplifile.delete(layout.takeout_file)
  let _ = simplifile.delete(root <> "/ytdl-sub")
  let _ = simplifile.delete(root)
}
