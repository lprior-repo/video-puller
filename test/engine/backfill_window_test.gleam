/// Regression tests for the selected-window backfill configuration.
import engine/ytdl_sub
import gleam/option.{Some}
import gleam/string
import gleeunit
import gleeunit/should
import simplifile

pub fn main() {
  gleeunit.main()
}

pub fn full_pass_keeps_selected_window_and_traverses_archives_test() {
  let root = "/tmp/test_ytdl_sub_backfill_window"
  let _ = simplifile.delete_all([root])
  let base = ytdl_sub.layout(root)
  let layout = ytdl_sub.Layout(..base, workers: 1, download_days: 30)
  let _ = simplifile.create_directory_all(root <> "/ytdl-sub")
  let channels = [
    ytdl_sub.Channel(Some("Channel A"), "https://www.youtube.com/@channel-a"),
  ]

  ytdl_sub.write_subscriptions_with_budget(layout, channels, 0)
  |> should.be_ok()

  let full = simplifile.read(layout.subscriptions_file) |> should.be_ok()
  full |> string.contains("after: \"today-30days\"") |> should.be_true()
  full
  |> string.contains("  ytdl_options:\n    break_on_existing: False\n")
  |> should.be_true()
  full |> string.contains("playlist_end") |> should.be_false()

  let recent =
    simplifile.read(layout.recent_subscriptions_file) |> should.be_ok()
  recent |> string.contains("after: \"today-30days\"") |> should.be_true()
  recent
  |> string.contains("  ytdl_options:\n    playlist_end: 5\n")
  |> should.be_true()
  recent |> string.contains("break_on_existing: False") |> should.be_false()

  let _ = simplifile.delete_all([root])
}
