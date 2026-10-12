import core/channel_reconciler
import gleam/result
import gleeunit
import gleeunit/should
import simplifile

pub fn main() {
  gleeunit.main()
}

pub fn managed_folders_retire_then_repeat_idempotently_test() {
  let root = "/tmp/test_channel_reconciler_retire_repeat"
  clean(root)
  make_managed_show(root, "Retired", "video-data")

  channel_reconciler.reconcile_folders(root, ["Keep"])
  |> should.be_ok()
  is_directory(root <> "/library/Retired") |> should.be_false()
  is_directory(root <> "/retired-library/Retired") |> should.be_true()
  read(root <> "/retired-library/Retired/episode.mp4")
  |> should.equal("video-data")

  channel_reconciler.reconcile_folders(root, ["Keep"])
  |> should.be_ok()
  is_directory(root <> "/retired-library/Retired") |> should.be_true()
  read(root <> "/retired-library/Retired/episode.mp4")
  |> should.equal("video-data")
  clean(root)
}

pub fn readded_folder_restores_archive_and_video_unchanged_test() {
  let root = "/tmp/test_channel_reconciler_restore"
  clean(root)
  make_managed_show(root, "Readded", "preserved-video")
  let archive = archive_path(root <> "/library/Readded", "Readded")
  let _ = simplifile.write(archive, "archive-state")

  channel_reconciler.reconcile_folders(root, ["Other"])
  |> should.be_ok()
  channel_reconciler.reconcile_folders(root, ["Other", "Readded"])
  |> should.be_ok()

  is_directory(root <> "/retired-library/Readded") |> should.be_false()
  read(root <> "/library/Readded/episode.mp4")
  |> should.equal("preserved-video")
  read(archive_path(root <> "/library/Readded", "Readded"))
  |> should.equal("archive-state")
  clean(root)
}

pub fn unmanaged_directory_is_not_retired_test() {
  let root = "/tmp/test_channel_reconciler_unmanaged"
  clean(root)
  let unmanaged = root <> "/library/Unmanaged"
  let _ = simplifile.create_directory_all(unmanaged)
  let _ = simplifile.write(unmanaged <> "/notes.txt", "leave me")
  make_managed_show(root, "Managed", "managed-video")

  channel_reconciler.reconcile_folders(root, ["Keep"])
  |> should.be_ok()

  is_directory(unmanaged) |> should.be_true()
  read(unmanaged <> "/notes.txt") |> should.equal("leave me")
  is_directory(root <> "/retired-library/Managed") |> should.be_true()
  clean(root)
}

pub fn empty_desired_list_refusal_leaves_files_unchanged_test() {
  let root = "/tmp/test_channel_reconciler_empty"
  clean(root)
  make_managed_show(root, "Managed", "do-not-move")

  channel_reconciler.reconcile_folders(root, [])
  |> should.equal(Error("refusing to reconcile an empty channel list"))

  is_directory(root <> "/library/Managed") |> should.be_true()
  is_directory(root <> "/retired-library") |> should.be_false()
  read(root <> "/library/Managed/episode.mp4")
  |> should.equal("do-not-move")
  clean(root)
}

pub fn destination_conflicts_never_overwrite_either_show_test() {
  let retirement_root = "/tmp/test_channel_reconciler_retire_conflict"
  clean(retirement_root)
  make_managed_show(retirement_root, "Collision", "live-data")
  let retired = retirement_root <> "/retired-library/Collision"
  let _ = simplifile.create_directory_all(retired)
  let _ = simplifile.write(retired <> "/episode.mp4", "retired-data")

  channel_reconciler.reconcile_folders(retirement_root, ["Keep"])
  |> should.equal(Error(
    "refusing to overwrite existing show "
    <> retirement_root
    <> "/retired-library/Collision",
  ))
  read(retirement_root <> "/library/Collision/episode.mp4")
  |> should.equal("live-data")
  read(retired <> "/episode.mp4") |> should.equal("retired-data")

  let restore_root = "/tmp/test_channel_reconciler_restore_conflict"
  clean(restore_root)
  make_managed_show(restore_root, "Collision", "retired-data")
  channel_reconciler.reconcile_folders(restore_root, ["Keep"])
  |> should.be_ok()
  make_managed_show(restore_root, "Collision", "live-data")

  channel_reconciler.reconcile_folders(restore_root, ["Collision"])
  |> should.equal(Error(
    "refusing to overwrite existing show "
    <> restore_root
    <> "/library/Collision",
  ))
  read(restore_root <> "/retired-library/Collision/episode.mp4")
  |> should.equal("retired-data")
  read(restore_root <> "/library/Collision/episode.mp4")
  |> should.equal("live-data")

  clean(retirement_root)
  clean(restore_root)
}

fn make_managed_show(root: String, label: String, video: String) -> Nil {
  let path = root <> "/library/" <> label
  let _ = simplifile.create_directory_all(path)
  let _ = simplifile.write(path <> "/episode.mp4", video)
  let _ = simplifile.write(archive_path(path, label), "archive")
  Nil
}

fn archive_path(path: String, label: String) -> String {
  path <> "/.ytdl-sub-" <> label <> "-download-archive.json"
}

fn is_directory(path: String) -> Bool {
  simplifile.is_directory(path) |> result.unwrap(False)
}

fn read(path: String) -> String {
  case simplifile.read(path) {
    Ok(contents) -> contents
    Error(_) -> panic as "Fixture file was not preserved"
  }
}

fn clean(path: String) -> Nil {
  let _ = simplifile.delete(path)
  Nil
}
