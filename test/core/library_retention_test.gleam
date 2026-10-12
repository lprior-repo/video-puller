import core/library_retention
import engine/ytdl_sub
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import gleeunit
import gleeunit/should
import simplifile

pub fn main() {
  gleeunit.main()
}

pub fn retention_removes_only_old_managed_regular_episodes_and_sidecars_test() {
  let root = "/tmp/test_library_retention"
  let layout = ytdl_sub.Layout(..ytdl_sub.layout(root), download_days: 30)
  let _ = simplifile.create_directory_all(layout.library_dir)
  let _ = simplifile.create_directory_all(layout.channels_file |> parent_path)
  let _ =
    simplifile.write(
      layout.channels_file,
      "Managed Show = https://www.youtube.com/@ManagedShow\n"
        <> "No Archive = https://www.youtube.com/@NoArchive\n"
        <> "Pinned One Off = https://www.youtube.com/watch?v=pin123\n",
    )

  let managed = layout.library_dir <> "/Managed Show/Season 1970"
  let no_archive = layout.library_dir <> "/No Archive/Season 1970"
  let pinned = layout.library_dir <> "/Pinned One Off/Season 1970"
  let unmanaged = layout.library_dir <> "/Unmanaged/Season 1970"
  let _ = simplifile.create_directory_all(managed)
  let _ = simplifile.create_directory_all(no_archive)
  let _ = simplifile.create_directory_all(pinned)
  let _ = simplifile.create_directory_all(unmanaged)
  let _ =
    simplifile.write(
      layout.library_dir
        <> "/Managed Show/.ytdl-sub-Managed Show-download-archive.json",
      "archive-state",
    )
  let _ =
    simplifile.write(
      layout.library_dir <> "/No Archive/s1970.e010100 - No Archive.mp4",
      "no marker should prevent traversal",
    )
  let _ =
    simplifile.write(
      layout.library_dir
        <> "/Pinned One Off/.ytdl-sub-Pinned One Off-download-archive.json",
      "pinned archive",
    )
  let _ =
    simplifile.write(
      layout.library_dir
        <> "/Unmanaged/.ytdl-sub-Unmanaged-download-archive.json",
      "other archive",
    )

  let old = managed <> "/s1970.e010100 - Old.mp4"
  let boundary = managed <> "/s1970.e011100 - Boundary.mp4"
  let new = managed <> "/s1970.e011200 - New.mp4"
  let unknown = managed <> "/unknown-date.mp4"
  let _ = simplifile.write(old, "old")
  let _ =
    simplifile.write(
      managed <> "/s1970.e010100 - Old.info.json",
      "{\"id\":\"old-video\",\"upload_date\":\"19700101\"}",
    )
  let _ = simplifile.write(managed <> "/s1970.e010100 - Old-thumb.jpg", "thumb")
  let _ =
    simplifile.write(managed <> "/s1970.e010100 - Old.en-orig.srt", "caption")
  let _ = simplifile.write(managed <> "/s1970.e010100 - Old.nfo", "nfo")
  let _ = simplifile.write(managed <> "/poster.jpg", "show poster")
  let _ =
    simplifile.write(managed <> "/s1970.e010100 - Old.backup.txt", "unrelated")
  let _ = simplifile.write(boundary, "boundary")
  let _ = simplifile.write(new, "new")
  let _ = simplifile.write(unknown, "unknown")

  let _ =
    simplifile.write(
      pinned <> "/s1970.e010100 - Pinned.mp4",
      "one-off must be kept",
    )
  let _ =
    simplifile.write(
      unmanaged <> "/s1970.e010100 - Unmanaged.mp4",
      "unsubscribed must be kept",
    )

  let summary =
    library_retention.prune(layout, 40 * 86_400 + 43_200)
    |> should.be_ok()
  summary.deleted_ids |> should.equal(["old-video"])
  summary.eligible_labels |> should.equal(["Managed Show"])
  summary.cutoff_timestamp |> should.equal(Some(10 * 86_400))

  should.be_false(simplifile.is_file(old) |> result.unwrap(True))
  should.be_false(
    simplifile.is_file(managed <> "/s1970.e010100 - Old.info.json")
    |> result.unwrap(True),
  )
  should.be_false(
    simplifile.is_file(managed <> "/s1970.e010100 - Old-thumb.jpg")
    |> result.unwrap(True),
  )
  should.be_false(
    simplifile.is_file(managed <> "/s1970.e010100 - Old.en-orig.srt")
    |> result.unwrap(True),
  )
  should.be_false(
    simplifile.is_file(managed <> "/s1970.e010100 - Old.nfo")
    |> result.unwrap(True),
  )
  should.be_true(simplifile.is_file(boundary) |> result.unwrap(False))
  should.be_true(simplifile.is_file(new) |> result.unwrap(False))
  should.be_true(simplifile.is_file(unknown) |> result.unwrap(False))
  should.be_true(
    simplifile.is_file(managed <> "/poster.jpg") |> result.unwrap(False),
  )
  should.be_true(
    simplifile.is_file(managed <> "/s1970.e010100 - Old.backup.txt")
    |> result.unwrap(False),
  )
  should.be_true(
    simplifile.is_file(
      layout.library_dir
      <> "/Managed Show/.ytdl-sub-Managed Show-download-archive.json",
    )
    |> result.unwrap(False),
  )
  should.be_true(
    simplifile.is_file(pinned <> "/s1970.e010100 - Pinned.mp4")
    |> result.unwrap(False),
  )
  should.be_true(
    simplifile.is_file(unmanaged <> "/s1970.e010100 - Unmanaged.mp4")
    |> result.unwrap(False),
  )

  let summary =
    library_retention.prune(layout, 40 * 86_400 + 43_200)
    |> should.be_ok()
  summary.deleted_ids |> should.equal([])
  let _ = simplifile.delete(root)
  Nil
}

pub fn zero_day_window_disables_retention_test() {
  let root = "/tmp/test_library_retention_disabled"
  let layout = ytdl_sub.Layout(..ytdl_sub.layout(root), download_days: 0)
  let _ = simplifile.create_directory_all(parent_path(layout.channels_file))
  let old = layout.library_dir <> "/Managed/Season 1970/s1970.e010100 - Old.mp4"
  let _ = simplifile.create_directory_all(parent_path(old))
  let _ =
    simplifile.write(
      layout.channels_file,
      "Managed = https://www.youtube.com/@Managed\n",
    )
  let _ =
    simplifile.write(
      layout.library_dir <> "/Managed/.ytdl-sub-Managed-download-archive.json",
      "archive",
    )
  let _ = simplifile.write(old, "keep")

  library_retention.prune(layout, 40 * 86_400)
  |> should.equal(Ok(library_retention.PruneSummary([], [], None)))
  simplifile.is_file(old) |> should.equal(Ok(True))
  let _ = simplifile.delete(root)
  Nil
}

pub fn durable_history_is_written_before_expired_media_is_removed_test() {
  let root = "/tmp/test_library_retention_history"
  let _ = simplifile.delete_all([root])
  let layout = ytdl_sub.Layout(..ytdl_sub.layout(root), download_days: 30)
  let _ = simplifile.create_directory_all(parent_path(layout.channels_file))
  let _ =
    simplifile.write(
      layout.channels_file,
      "Managed = https://www.youtube.com/@Managed\n",
    )
  let show = layout.library_dir <> "/Managed"
  let season = show <> "/Season 1970"
  let media = season <> "/s1970.e010100 - Expired.mp4"
  let _ = simplifile.create_directory_all(season)
  let _ =
    simplifile.write(show <> "/.ytdl-sub-Managed-download-archive.json", "{}")
  let _ = simplifile.write(media, "complete")
  let _ =
    simplifile.write(
      season <> "/s1970.e010100 - Expired.info.json",
      "{\"id\":\"a1b2c3d4e5F\",\"upload_date\":\"19700101\"}",
    )

  library_retention.prune(layout, 40 * 86_400) |> should.be_ok()
  simplifile.is_file(media) |> should.equal(Ok(False))
  simplifile.read(layout.root <> "/ytdl-sub/download-history/archive.txt")
  |> should.equal(Ok("youtube a1b2c3d4e5F\n"))
}

pub fn explicit_video_is_preserved_when_its_file_lives_in_regular_channel_test() {
  let root = "/tmp/test_library_retention_pinned_in_regular"
  let _ = simplifile.delete_all([root])
  let layout = ytdl_sub.Layout(..ytdl_sub.layout(root), download_days: 30)
  let _ = simplifile.create_directory_all(parent_path(layout.channels_file))
  let _ =
    simplifile.write(
      layout.channels_file,
      "Regular = https://www.youtube.com/@Regular\n"
        <> "Pinned = https://youtu.be/a1b2c3d4e5F\n",
    )
  let show = layout.library_dir <> "/Regular"
  let season = show <> "/Season 1970"
  let pinned = season <> "/s1970.e010100 - Pinned.mp4"
  let old = season <> "/s1970.e010200 - Other.mp4"
  let _ = simplifile.create_directory_all(season)
  let _ =
    simplifile.write(show <> "/.ytdl-sub-Regular-download-archive.json", "{}")
  let _ = simplifile.write(pinned, "pinned file")
  let _ =
    simplifile.write(
      season <> "/s1970.e010100 - Pinned.info.json",
      "{\"id\":\"a1b2c3d4e5F\",\"upload_date\":\"19700101\"}",
    )
  let _ = simplifile.write(old, "old file")
  let _ =
    simplifile.write(
      season <> "/s1970.e010200 - Other.info.json",
      "{\"id\":\"z9y8x7w6v5U\",\"upload_date\":\"19700102\"}",
    )

  let summary = library_retention.prune(layout, 40 * 86_400) |> should.be_ok()
  summary.deleted_ids |> should.equal(["z9y8x7w6v5U"])
  simplifile.is_file(pinned) |> should.equal(Ok(True))
  simplifile.is_file(old) |> should.equal(Ok(False))
}

pub fn collision_uses_full_subscription_set_before_excluding_one_offs_test() {
  let root = "/tmp/test_library_retention_collision"
  let layout = ytdl_sub.Layout(..ytdl_sub.layout(root), download_days: 30)
  let _ = simplifile.create_directory_all(parent_path(layout.channels_file))
  let _ =
    simplifile.write(
      layout.channels_file,
      "Foo = https://www.youtube.com/watch?v=pinned123\n"
        <> "Foo = https://www.youtube.com/@handle\n",
    )
  let pinned =
    layout.library_dir <> "/Foo/Season 1970/s1970.e010100 - Pinned.mp4"
  let regular =
    layout.library_dir
    <> "/Foo (handle)/Season 1970/s1970.e010100 - Regular.mp4"
  let _ = simplifile.create_directory_all(parent_path(pinned))
  let _ = simplifile.create_directory_all(parent_path(regular))
  let _ =
    simplifile.write(
      layout.library_dir <> "/Foo/.ytdl-sub-Foo-download-archive.json",
      "pinned archive",
    )
  let _ =
    simplifile.write(
      layout.library_dir
        <> "/Foo (handle)/.ytdl-sub-Foo (handle)-download-archive.json",
      "regular archive",
    )
  let _ = simplifile.write(pinned, "keep")
  let _ = simplifile.write(regular, "remove")
  let _ =
    simplifile.write(
      layout.library_dir
        <> "/Foo (handle)/Season 1970/s1970.e010100 - Regular.info.json",
      "{\"id\":\"regular-video\",\"upload_date\":\"19700101\"}",
    )

  let summary =
    library_retention.prune(layout, 40 * 86_400)
    |> should.be_ok()
  summary.eligible_labels |> should.equal(["Foo (handle)"])
  summary.deleted_ids |> should.equal(["regular-video"])
  simplifile.is_file(pinned) |> should.equal(Ok(True))
  simplifile.is_file(regular) |> should.equal(Ok(False))
  let _ = simplifile.delete(root)
  Nil
}

pub fn empty_channel_file_fails_safe_without_deleting_anything_test() {
  let root = "/tmp/test_library_retention_empty"
  let layout = ytdl_sub.Layout(..ytdl_sub.layout(root), download_days: 30)
  let _ = simplifile.create_directory_all(parent_path(layout.channels_file))
  let _ =
    simplifile.create_directory_all(
      layout.library_dir <> "/Managed/Season 1970",
    )
  let old = layout.library_dir <> "/Managed/Season 1970/s1970.e010100 - Old.mp4"
  let _ = simplifile.write(layout.channels_file, "")
  let _ =
    simplifile.write(
      layout.library_dir <> "/Managed/.ytdl-sub-Managed-download-archive.json",
      "archive",
    )
  let _ = simplifile.write(old, "keep")

  library_retention.prune(layout, 40 * 86_400)
  |> should.equal(Ok(library_retention.PruneSummary([], [], Some(10 * 86_400))))
  simplifile.is_file(old) |> should.equal(Ok(True))
  let _ = simplifile.delete(root)
  Nil
}

fn parent_path(path: String) -> String {
  path
  |> string.split("/")
  |> list.reverse
  |> list.drop(1)
  |> list.reverse
  |> string.join("/")
}
