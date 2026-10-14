import core/pressure_retention
import engine/download_history
import engine/ytdl_sub
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import gleeunit
import gleeunit/should
import simplifile

pub fn main() {
  gleeunit.main()
}

pub fn pressure_retention_removes_oldest_then_stops_at_space_target_test() {
  let #(layout, season) = fixture("pressure_oldest")
  let oldest =
    add_video(
      layout,
      season,
      "s1970.e010100 - Oldest",
      "oldest00001",
      Some("19700101"),
    )
  let middle =
    add_video(
      layout,
      season,
      "s1970.e010200 - Middle",
      "middle00001",
      Some("19700102"),
    )
  let newest =
    add_video(
      layout,
      season,
      "s1970.e010300 - Newest",
      "newest00001",
      Some("19700103"),
    )

  let removed =
    pressure_retention.prune(
      layout,
      [],
      fn() { Ok(simplifile.is_file(oldest) == Ok(False)) },
      fn(_path) { Ok(True) },
    )
    |> should.be_ok()

  removed |> should.equal(["oldest00001"])
  is_file(oldest) |> should.be_false()
  is_file(middle) |> should.be_true()
  is_file(newest) |> should.be_true()
  history(layout) |> should.equal(["oldest00001"])
  clean(layout.root)
}

pub fn equal_dates_use_path_order_for_the_first_deletion_test() {
  let #(layout, season) = fixture("pressure_tie")
  let first =
    add_video(
      layout,
      season,
      "s1970.e010100 - A",
      "tievideo001",
      Some("19700101"),
    )
  let second =
    add_video(
      layout,
      season,
      "s1970.e010100 - B",
      "tievideo002",
      Some("19700101"),
    )

  pressure_retention.prune(
    layout,
    [],
    fn() { Ok(simplifile.is_file(first) == Ok(False)) },
    fn(_path) { Ok(True) },
  )
  |> should.equal(Ok(["tievideo001"]))
  is_file(first) |> should.be_false()
  is_file(second) |> should.be_true()
  clean(layout.root)
}

pub fn pinned_unknown_unmanaged_symlink_and_protected_paths_are_kept_test() {
  let #(layout, season) = fixture("pressure_protected")
  let pinned =
    add_video(
      layout,
      season,
      "s1970.e010100 - Pinned",
      "pinned00001",
      Some("19700101"),
    )
  let protected =
    add_video(
      layout,
      season,
      "s1970.e010200 - Protected",
      "protect0001",
      Some("19700102"),
    )
  let eligible =
    add_video(
      layout,
      season,
      "s1970.e010300 - Eligible",
      "eligibl0001",
      Some("19700103"),
    )
  let unknown = add_video(layout, season, "undated", "unknown0001", None)
  let _ =
    simplifile.write(
      layout.channels_file,
      "Managed = https://www.youtube.com/@Managed\n"
        <> "Pinned One Off = https://www.youtube.com/watch?v=pinned00001\n",
    )

  let unmanaged_show = layout.library_dir <> "/Unmanaged"
  let unmanaged_season = unmanaged_show <> "/Season 1970"
  let _ = simplifile.create_directory_all(unmanaged_season)
  let _ =
    simplifile.write(
      unmanaged_show <> "/.ytdl-sub-Unmanaged-download-archive.json",
      "{}",
    )
  let unmanaged =
    add_video(
      layout,
      unmanaged_season,
      "s1970.e010100 - Unmanaged",
      "unmanage001",
      Some("19700101"),
    )

  let target = layout.root <> "/outside-target.mp4"
  let _ = simplifile.write(target, "outside target")
  let symlink = season <> "/s1970.e010500 - Symlink.mp4"
  simplifile.create_symlink(to: "../../../outside-target.mp4", from: symlink)
  |> should.be_ok()

  let removed =
    pressure_retention.prune(layout, [protected], fn() { Ok(False) }, fn(_path) {
      Ok(True)
    })
    |> should.be_ok()

  removed |> should.equal(["eligibl0001"])
  is_file(pinned) |> should.be_true()
  is_file(protected) |> should.be_true()
  is_file(eligible) |> should.be_false()
  is_file(unknown) |> should.be_true()
  is_file(unmanaged) |> should.be_true()
  is_file(target) |> should.be_true()
  simplifile.is_symlink(symlink) |> should.equal(Ok(True))
  clean(layout.root)
}

pub fn repeated_prune_keeps_history_and_snapshot_blocks_redownload_test() {
  let #(layout, season) = fixture("pressure_history")
  let media =
    add_video(
      layout,
      season,
      "s1970.e010100 - Completed",
      "history0001",
      Some("19700101"),
    )

  pressure_retention.prune(layout, [], fn() { Ok(False) }, fn(_path) {
    Ok(True)
  })
  |> should.equal(Ok(["history0001"]))
  is_file(media) |> should.be_false()

  // A second cleanup sees no media candidate and leaves the durable ledger
  // intact; the next ytdl-sub snapshot still skips this completed video.
  pressure_retention.prune(layout, [], fn() { Ok(False) }, fn(_path) {
    Ok(True)
  })
  |> should.equal(Ok([]))
  history(layout) |> should.equal(["history0001"])
  let snapshot = layout.root <> "/snapshot.txt"
  download_history.write_snapshot(layout.root, snapshot) |> should.be_ok()
  simplifile.read(snapshot)
  |> should.equal(Ok("youtube history0001\n"))
  clean(layout.root)
}

pub fn late_can_delete_recheck_preserves_protected_candidate_test() {
  let #(layout, season) = fixture("pressure_late_protection")
  let protected =
    add_video(
      layout,
      season,
      "s1970.e010100 - Now Active",
      "active00001",
      Some("19700101"),
    )
  let next =
    add_video(
      layout,
      season,
      "s1970.e010200 - Next",
      "nextvideo01",
      Some("19700102"),
    )

  pressure_retention.prune(layout, [], fn() { Ok(False) }, fn(path) {
    Ok(path != protected)
  })
  |> should.equal(Ok(["nextvideo01"]))
  is_file(protected) |> should.be_true()
  is_file(next) |> should.be_false()
  history(layout) |> should.equal(["nextvideo01"])
  clean(layout.root)
}

pub fn failed_space_check_blocks_all_deletions_test() {
  let #(layout, season) = fixture("pressure_space_error")
  let media =
    add_video(
      layout,
      season,
      "s1970.e010100 - Keep",
      "spaceerror1",
      Some("19700101"),
    )

  pressure_retention.prune(
    layout,
    [],
    fn() { Error("space check failed") },
    fn(_path) { Ok(True) },
  )
  |> should.equal(Error("space check failed"))
  is_file(media) |> should.be_true()
  history(layout) |> should.equal([])
  clean(layout.root)
}

pub fn failed_volume_recheck_blocks_all_deletions_test() {
  let #(layout, season) = fixture("pressure_volume_error")
  let media =
    add_video(
      layout,
      season,
      "s1970.e010100 - Keep",
      "volumeerr01",
      Some("19700101"),
    )

  pressure_retention.prune(layout, [], fn() { Ok(False) }, fn(_path) {
    Error("volume identity changed")
  })
  |> should.equal(Error("volume identity changed"))
  is_file(media) |> should.be_true()
  history(layout) |> should.equal([])
  clean(layout.root)
}

pub fn unreadable_history_preserves_media_before_any_deletion_test() {
  let #(layout, season) = fixture("pressure_history_error")
  let media = add_video(layout, season, "Keep", "historyerr1", Some("19700101"))
  // A directory at the ledger filepath forces a real history write failure.
  simplifile.create_directory_all(download_history.archive_path(layout.root))
  |> should.be_ok()
  pressure_retention.prune(layout, [], fn() { Ok(False) }, fn(_) { Ok(True) })
  |> should.be_error()
  is_file(media) |> should.be_true()
  is_file(season <> "/Keep.info.json") |> should.be_true()
  clean(layout.root)
}

fn fixture(name: String) {
  let root = "/tmp/" <> name
  let _ = simplifile.delete_all([root])
  let layout = ytdl_sub.Layout(..ytdl_sub.layout(root), download_days: 30)
  let season = layout.library_dir <> "/Managed/Season 1970"
  let _ = simplifile.create_directory_all(season)
  let _ = simplifile.create_directory_all(parent_path(layout.channels_file))
  let _ =
    simplifile.write(
      layout.channels_file,
      "Managed = https://www.youtube.com/@Managed\n",
    )
  let _ =
    simplifile.write(
      layout.library_dir <> "/Managed/.ytdl-sub-Managed-download-archive.json",
      "{}",
    )
  #(layout, season)
}

fn add_video(
  _layout: ytdl_sub.Layout,
  season: String,
  stem: String,
  id: String,
  upload_date: Option(String),
) -> String {
  let path = season <> "/" <> stem <> ".mp4"
  let sidecar = season <> "/" <> stem <> ".info.json"
  let date_field = case upload_date {
    Some(date) -> ",\"upload_date\":\"" <> date <> "\""
    None -> ""
  }
  let _ = simplifile.write(path, "completed media")
  let _ =
    simplifile.write(
      sidecar,
      "{\"id\":\""
        <> id
        <> "\",\"webpage_url\":\"https://www.youtube.com/watch?v="
        <> id
        <> "\""
        <> date_field
        <> "}",
    )
  path
}

fn history(layout: ytdl_sub.Layout) -> List(String) {
  download_history.known_ids(layout.root) |> should.be_ok()
}

fn is_file(path: String) -> Bool {
  simplifile.is_file(path) == Ok(True)
}

fn clean(root: String) {
  let _ = simplifile.delete_all([root])
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
