import core/managed_trash
import gleeunit
import gleeunit/should
import simplifile

pub fn main() {
  gleeunit.main()
}

pub fn cleanup_gate_requires_exact_idle_show_section_and_complete_index_test() {
  let library = "/Volumes/YouTube SSD/YouTube/library"
  let current = [library <> "/Artist/Season 2026/episode.mp4"]
  let indexed = [library <> "/Artist/Season 2025/removed.mp4", ..current]
  managed_trash.inventory_is_safe(
    library,
    "show",
    False,
    [library],
    indexed,
    current,
  )
  |> should.equal(True)
  managed_trash.inventory_is_safe(
    library,
    "movie",
    False,
    [library],
    indexed,
    current,
  )
  |> should.equal(False)
  managed_trash.inventory_is_safe(
    library,
    "show",
    True,
    [library],
    indexed,
    current,
  )
  |> should.equal(False)
  managed_trash.inventory_is_safe(
    library,
    "show",
    False,
    [library, "/Volumes/1 TB Plex/TV"],
    indexed,
    current,
  )
  |> should.equal(False)
  managed_trash.inventory_is_safe(
    library,
    "show",
    False,
    [library],
    ["/Volumes/1 TB Plex/Movies/item.mp4", ..indexed],
    current,
  )
  |> should.equal(False)
  managed_trash.inventory_is_safe(
    library,
    "show",
    False,
    [library],
    indexed,
    [],
  )
  |> should.equal(False)
  managed_trash.inventory_is_safe(
    library,
    "show",
    False,
    [library],
    [library <> "/Artist/Season 2025/removed.mp4"],
    current,
  )
  |> should.equal(False)
}

pub fn missing_path_requires_enoent_and_readable_parent_test() {
  let root = "/tmp/test_managed_plex_trash"
  let _ = simplifile.delete_all([root])
  let season = root <> "/library/Artist/Season 2025"
  simplifile.create_directory_all(season) |> should.be_ok
  let present = season <> "/present.mp4"
  let missing = season <> "/removed.mp4"
  simplifile.write(present, "media") |> should.be_ok
  managed_trash.missing_paths(root <> "/library", [present, missing], [present])
  |> should.equal(Ok([missing]))
  managed_trash.missing_paths(
    root <> "/library",
    [root <> "/library/no-parent/file.mp4"],
    [],
  )
  |> should.equal(Ok([root <> "/library/no-parent/file.mp4"]))
  managed_trash.missing_paths(
    root <> "/library",
    [root <> "/library/Artist/Season 2024/removed.mp4"],
    [],
  )
  |> should.equal(Ok([root <> "/library/Artist/Season 2024/removed.mp4"]))
  managed_trash.missing_paths(
    root <> "/library",
    [root <> "/library/../Movies/unsafe.mp4"],
    [],
  )
  |> should.be_error
  let _ = simplifile.delete_all([root])
}

pub fn broken_symlink_is_not_classified_as_missing_test() {
  let root = "/tmp/test_managed_plex_trash_symlink"
  let _ = simplifile.delete_all([root])
  let season = root <> "/library/Artist/Season 2025"
  simplifile.create_directory_all(season) |> should.be_ok
  let broken = season <> "/removed.mp4"
  simplifile.create_symlink("no-such-target.mp4", broken) |> should.be_ok
  managed_trash.missing_paths(root <> "/library", [broken], [])
  |> should.be_error
  let _ = simplifile.delete_all([root])
}
