import engine/ytdl_sub
import gleam/list
import gleam/option.{Some}
import gleam/string
import gleeunit/should
import simplifile

pub fn both_passes_and_each_worker_receive_request_pacing_test() {
  let root = "/tmp/test_youtube_request_pacing"
  let layout = ytdl_sub.layout(root)
  simplifile.create_directory_all(root <> "/ytdl-sub") |> should.be_ok()
  ytdl_sub.write_subscriptions_with_budget(
    layout,
    [
      ytdl_sub.Channel(
        Some("Test"),
        "https://www.youtube.com/watch?v=abcdefghijk",
      ),
    ],
    0,
  )
  |> should.be_ok()
  let passes = [layout.subscriptions_file, layout.recent_subscriptions_file]
  list.each(passes, fn(path) {
    let manifest = simplifile.read(path <> ".jobs") |> should.be_ok()
    let worker_file =
      manifest
      |> string.split("\t")
      |> list.drop(1)
      |> list.first()
      |> should.be_ok()
    list.each([path, worker_file], fn(file) {
      let content = simplifile.read(file) |> should.be_ok()
      content
      |> string.contains("    sleep_interval_requests: 1.0\n")
      |> should.be_true()
      content
      |> string.contains("    sleep_interval: 5.0\n")
      |> should.be_true()
      content
      |> string.contains("    max_sleep_interval: 10.0\n")
      |> should.be_true()
    })
  })
  simplifile.delete_all([root]) |> should.be_ok()
}
