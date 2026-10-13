import core/plex_scan_exclusion
import gleeunit/should
import simplifile

pub fn parent_rule_excludes_only_dedicated_data_tree_test() {
  plex_scan_exclusion.exclusion_rule(
    "/Volumes/Media/",
    "/Volumes/Media/YouTube/",
  )
  |> should.equal(Ok("YouTube/*"))
  plex_scan_exclusion.exclusion_rule(
    "/Volumes/Media",
    "/Volumes/Media/Video/YouTube",
  )
  |> should.equal(Ok("Video/YouTube/*"))
}

pub fn parent_rule_refuses_broad_or_ambiguous_paths_test() {
  plex_scan_exclusion.exclusion_rule("/Volumes/Media", "/Volumes/Media")
  |> should.be_error()
  plex_scan_exclusion.exclusion_rule(
    "/Volumes/Media",
    "/Volumes/Media-other/YouTube",
  )
  |> should.be_error()
  plex_scan_exclusion.exclusion_rule(
    "/Volumes/Media",
    "/Volumes/Media/../Other",
  )
  |> should.be_error()
  plex_scan_exclusion.exclusion_rule("/Volumes/Media", "/Volumes/Media/*")
  |> should.be_error()
  plex_scan_exclusion.exclusion_rule("/", "/Users/lewis") |> should.be_error()
}

pub fn parent_exclusion_preserves_comments_and_is_idempotent_test() {
  let root = "/tmp/vp-parent-plex-exclusion"
  let _ = simplifile.delete_all([root])
  simplifile.create_directory_all(root) |> should.be_ok()
  simplifile.write(
    root <> "/.plexignore",
    "# YouTube/* is not an active rule\ncustom/*",
  )
  |> should.be_ok()
  plex_scan_exclusion.ensure(root, root <> "/YouTube") |> should.be_ok()
  let expected = "# YouTube/* is not an active rule\ncustom/*\nYouTube/*\n"
  simplifile.read(root <> "/.plexignore") |> should.equal(Ok(expected))
  plex_scan_exclusion.ensure(root, root <> "/YouTube") |> should.be_ok()
  simplifile.read(root <> "/.plexignore") |> should.equal(Ok(expected))
  let _ = simplifile.delete_all([root])
}

pub fn parent_exclusion_refuses_symlinked_ignore_file_test() {
  let root = "/tmp/vp-parent-plex-symlink"
  let _ = simplifile.delete_all([root])
  simplifile.create_directory_all(root) |> should.be_ok()
  simplifile.write(root <> "/keep", "custom/*\n") |> should.be_ok()
  simplifile.create_symlink(root <> "/keep", root <> "/.plexignore")
  |> should.be_ok()
  plex_scan_exclusion.ensure(root, root <> "/YouTube") |> should.be_error()
  simplifile.read(root <> "/keep") |> should.equal(Ok("custom/*\n"))
  let _ = simplifile.delete_all([root])
}
