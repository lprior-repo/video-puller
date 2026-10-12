import engine/shell
import engine/subscription_progress
import gleam/erlang/process
import gleam/list
import gleam/option.{None, Some}
import gleeunit
import gleeunit/should
import simplifile

pub fn main() {
  gleeunit.main()
}

pub fn shell_timeout_runner_delivers_output_lines_to_callback_test() {
  let subject = process.new_subject()
  let result =
    shell.run_with_timeout_and_output(
      "printf",
      ["first\nsecond\n"],
      5000,
      fn(line) { process.send(subject, line) },
    )
    |> should.be_ok()

  result.stdout |> should.equal("first\nsecond")
  process.receive(subject, 1000) |> should.equal(Ok("first"))
  process.receive(subject, 1000) |> should.equal(Ok("second"))
}

pub fn progress_updates_track_percent_and_terminal_status_test() {
  let root = "/tmp/test_subscription_progress"
  let _ = simplifile.delete(root <> "/ytdl-sub/progress/recent-channel-a.json")
  subscription_progress.prepare_phase(
    root,
    "recent",
    [#("channel-a", "Fireship")],
    1,
  )
  subscription_progress.start(root, "recent", "channel-a", "Fireship", 1)
  subscription_progress.observe(
    root,
    "recent",
    "channel-a",
    "[download] 25.0% of 1.00GiB at 4.00MiB/s",
  )

  let assert [running] = subscription_progress.read_snapshots(root)
  running.status |> should.equal("running")
  running.channel |> should.equal("Fireship")
  running.phase |> should.equal("recent")
  running.percent |> should.equal(Some(25))

  subscription_progress.finish(root, "recent", "channel-a", Ok(Nil))
  let assert [completed] = subscription_progress.read_snapshots(root)
  completed.status |> should.equal("completed")
  completed.phase_total |> should.equal(1)
}

pub fn recent_phase_clears_old_rows_and_backfill_keeps_recent_test() {
  let root = "/tmp/test_subscription_progress_phases"
  subscription_progress.prepare_phase(
    root,
    "recent",
    [#("channel-a", "Fireship")],
    1,
  )
  subscription_progress.finish(root, "recent", "channel-a", Ok(Nil))
  subscription_progress.prepare_phase(
    root,
    "backfill",
    [#("channel-a", "Fireship")],
    1,
  )

  let snapshots = subscription_progress.read_snapshots(root)
  list.length(snapshots) |> should.equal(2)
  list.any(snapshots, fn(row) { row.phase == "recent" }) |> should.be_true()
  list.any(snapshots, fn(row) { row.phase == "backfill" }) |> should.be_true()
}

pub fn progress_resets_percent_at_next_video_and_closes_unfinished_workers_test() {
  let root = "/tmp/test_subscription_progress_close"
  subscription_progress.prepare_phase(
    root,
    "recent",
    [#("active", "Fireship"), #("queued", "Computerphile")],
    2,
  )
  subscription_progress.start(root, "recent", "active", "Fireship", 2)
  subscription_progress.observe(
    root,
    "recent",
    "active",
    "[download] 100.0% of 1.00MiB",
  )
  let assert Ok(active) =
    list.find(subscription_progress.read_snapshots(root), fn(row) {
      row.key == "recent-active"
    })
  active.percent |> should.equal(Some(100))

  subscription_progress.observe(
    root,
    "recent",
    "active",
    "[download] Destination: next-video.mp4",
  )
  let assert Ok(active) =
    list.find(subscription_progress.read_snapshots(root), fn(row) {
      row.key == "recent-active"
    })
  active.percent |> should.equal(None)

  subscription_progress.close_phase(
    root,
    "recent",
    "Worker stopped before completion (deadline exceeded)",
  )
  let snapshots = subscription_progress.read_snapshots(root)
  let assert Ok(active) =
    list.find(snapshots, fn(row) { row.key == "recent-active" })
  let assert Ok(queued) =
    list.find(snapshots, fn(row) { row.key == "recent-queued" })
  active.status |> should.equal("failed")
  active.message
  |> should.equal("Worker stopped before completion (deadline exceeded)")
  queued.status |> should.equal("failed")
}
