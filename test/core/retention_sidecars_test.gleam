import core/episode_sidecars
import core/library_retention
import core/watched_retention
import engine/download_history
import engine/ytdl_sub
import gleam/list
import gleam/option.{Some}
import gleeunit
import gleeunit/should
import simplifile

pub fn main() {
  gleeunit.main()
}

pub fn known_companions_require_exact_metadata_and_artwork_names_test() {
  list.each(
    [
      ".info.json",
      ".nfo",
      ".jpg",
      ".jpeg",
      ".png",
      ".webp",
      "-thumb.jpg",
      "-thumb.jpeg",
      "-thumb.png",
      "-thumb.webp",
      ".srt",
      ".vtt",
      ".ass",
      ".ssa",
      ".lrc",
      ".en-orig.srt",
      ".en-US.vtt",
    ],
    fn(suffix) {
      episode_sidecars.owned("Clip", "Clip" <> suffix, [])
      |> should.be_true()
    },
  )
  list.each(
    [
      "Clip.extended.info.json",
      "Clip-extended.info.json",
      "Clip.extended.nfo",
      "Clip-extended.nfo",
      "Clip.extended.jpg",
      "Clip-extended.jpg",
      "Clip.backup.txt",
      "Clip.mp4",
      "poster.jpg",
      "Clip..srt",
      "Clip.bad tag.srt",
      "Clip.abcdefghijklmnopqrstu.srt",
    ],
    fn(filename) {
      episode_sidecars.owned("Clip", filename, []) |> should.be_false()
    },
  )
}

pub fn longer_media_stems_own_their_subtitles_and_thumbnails_test() {
  list.each(
    [
      #("Clip.extended", "Clip.extended.srt"),
      #("Clip.en", "Clip.en.srt"),
      #("Clip-thumb", "Clip-thumb.jpg"),
    ],
    fn(example) {
      let #(other, sidecar) = example
      episode_sidecars.owned("Clip", sidecar, [other])
      |> should.be_false()
      episode_sidecars.owned(other, sidecar, ["Clip"])
      |> should.be_true()
    },
  )
  episode_sidecars.owned("Clip", "Clip.en-orig.srt", ["Unrelated"])
  |> should.be_true()
}

pub fn equal_media_stems_preserve_ambiguous_shared_sidecars_test() {
  list.each([".info.json", "-thumb.jpg", ".srt", ".en.srt"], fn(suffix) {
    episode_sidecars.owned("Clip", "Clip" <> suffix, ["Clip"])
    |> should.be_false()
  })
}

pub fn expired_cleanup_preserves_dot_prefixed_episode_test() {
  assert_expired_cleanup("expired_dot", "Clip.extended")
}

pub fn expired_cleanup_preserves_hyphen_prefixed_episode_test() {
  assert_expired_cleanup("expired_hyphen", "Clip-extended")
}

pub fn expired_cleanup_preserves_thumbnail_named_episode_test() {
  assert_expired_cleanup("expired_thumbnail", "Clip-thumb")
}

pub fn watched_cleanup_preserves_dot_prefixed_episode_test() {
  assert_watched_cleanup("watched_dot", "Clip.extended")
}

pub fn watched_cleanup_preserves_thumbnail_named_episode_test() {
  assert_watched_cleanup("watched_thumbnail", "Clip-thumb")
}

pub fn failed_expired_history_write_preserves_every_episode_file_test() {
  let fixture = fixture("expired_history_failure", "Clip.extended")
  let history_dir = fixture.layout.root <> "/ytdl-sub/download-history"
  simplifile.delete(history_dir) |> should.be_ok()
  // A file in place of the directory fails even when tests run as root.
  simplifile.write(history_dir, "blocked history directory") |> should.be_ok()

  library_retention.prune(fixture.layout, 40 * 86_400)
  |> should.be_error()
  assert_files(fixture.season, fixture.expired)
  assert_files(fixture.season, fixture.retained)
  simplifile.read(fixture.archive) |> should.equal(Ok("archive-state\n"))
  simplifile.read(history_dir)
  |> should.equal(Ok("blocked history directory"))
  simplifile.delete(fixture.layout.root) |> should.be_ok()
}

pub fn failed_watched_history_write_preserves_every_episode_file_test() {
  let fixture = fixture("watched_history_failure", "Clip.extended")
  watched_retention.prune(
    fixture.layout,
    [fixture.season <> "/Clip.mp4"],
    fn(_) { Error("history unavailable") },
  )
  |> should.be_error()
  assert_files(fixture.season, fixture.expired)
  assert_files(fixture.season, fixture.retained)
  simplifile.read(fixture.archive) |> should.equal(Ok("archive-state\n"))
  simplifile.read(download_history.archive_path(fixture.layout.root))
  |> should.equal(Ok("youtube q1w2e3r4t5Y\n"))
  simplifile.delete(fixture.layout.root) |> should.be_ok()
}

type Fixture {
  Fixture(
    layout: ytdl_sub.Layout,
    season: String,
    archive: String,
    expired: List(#(String, String)),
    retained: List(#(String, String)),
  )
}

fn fixture(name: String, retained_stem: String) -> Fixture {
  let root = "/tmp/test_retention_sidecars_" <> name
  let _ = simplifile.delete_all([root])
  let layout = ytdl_sub.Layout(..ytdl_sub.layout(root), download_days: 30)
  let show = layout.library_dir <> "/Managed"
  let season = show <> "/Season 1970"
  let archive = show <> "/.ytdl-sub-Managed-download-archive.json"
  simplifile.create_directory_all(season) |> should.be_ok()
  simplifile.create_directory_all(root <> "/ytdl-sub/download-history")
  |> should.be_ok()
  simplifile.write(
    layout.channels_file,
    "Managed = https://www.youtube.com/@Managed\n",
  )
  |> should.be_ok()
  simplifile.write(archive, "archive-state\n") |> should.be_ok()
  simplifile.write(download_history.archive_path(root), "youtube q1w2e3r4t5Y\n")
  |> should.be_ok()
  // Clip-thumb.jpg is the retained video's plain artwork in the thumb case.
  let thumb = case retained_stem {
    "Clip-thumb" -> []
    _ -> [#("Clip-thumb.jpg", "expired thumbnail")]
  }
  let expired =
    list.append(
      [
        #("Clip.mp4", "expired media"),
        #(
          "Clip.info.json",
          "{\"id\":\"abc123DEF_-\",\"upload_date\":\"19700101\","
            <> "\"webpage_url\":\"https://www.youtube.com/watch?v=abc123DEF_-\"}",
        ),
        #("Clip.nfo", "expired nfo"),
        #("Clip.jpg", "expired artwork"),
        #("Clip.srt", "expired default subtitles"),
        #("Clip.en-orig.srt", "expired original subtitles"),
      ],
      thumb,
    )
  let retained = [
    #(retained_stem <> ".mp4", "retained media"),
    #(
      retained_stem <> ".info.json",
      "{\"id\":\"z9y8x7w6v5U\",\"upload_date\":\"19700112\"}",
    ),
    #(retained_stem <> ".nfo", "retained nfo"),
    #(retained_stem <> ".jpg", "retained artwork"),
    #(retained_stem <> "-thumb.jpg", "retained thumbnail"),
    #(retained_stem <> ".srt", "retained default subtitles"),
    #(retained_stem <> ".en-orig.srt", "retained original subtitles"),
    #("Clip.backup.txt", "unrelated backup"),
    #("poster.jpg", "unrelated poster"),
  ]
  list.each(list.append(expired, retained), fn(file) {
    let #(filename, contents) = file
    simplifile.write(season <> "/" <> filename, contents) |> should.be_ok()
  })
  Fixture(layout, season, archive, expired, retained)
}

fn assert_expired_cleanup(name: String, retained_stem: String) {
  let fixture = fixture(name, retained_stem)
  let summary =
    library_retention.prune(fixture.layout, 40 * 86_400) |> should.be_ok()
  summary.deleted_ids |> should.equal(["abc123DEF_-"])
  summary.eligible_labels |> should.equal(["Managed"])
  summary.cutoff_timestamp |> should.equal(Some(10 * 86_400))
  assert_cleaned(fixture)
  let history =
    simplifile.read(download_history.archive_path(fixture.layout.root))
    |> should.be_ok()

  let repeated =
    library_retention.prune(fixture.layout, 40 * 86_400) |> should.be_ok()
  repeated.deleted_ids |> should.equal([])
  assert_cleaned(fixture)
  simplifile.read(download_history.archive_path(fixture.layout.root))
  |> should.equal(Ok(history))
  simplifile.delete(fixture.layout.root) |> should.be_ok()
}

fn assert_watched_cleanup(name: String, retained_stem: String) {
  let fixture = fixture(name, retained_stem)
  let watched = fixture.season <> "/Clip.mp4"
  let summary =
    watched_retention.prune(fixture.layout, [watched], fn(id) {
      id |> should.equal("abc123DEF_-")
      // Completion must be recorded before unlinking ANY companion or media.
      assert_files(fixture.season, fixture.expired)
      assert_files(fixture.season, fixture.retained)
      download_history.remember(fixture.layout.root, id)
    })
    |> should.be_ok()
  summary.deleted_ids |> should.equal(["abc123DEF_-"])
  summary.deleted_paths |> should.equal([watched])
  assert_cleaned(fixture)
  let history =
    simplifile.read(download_history.archive_path(fixture.layout.root))
    |> should.be_ok()

  let repeated =
    watched_retention.prune(fixture.layout, [watched], fn(_) {
      Error("must not record deleted media again")
    })
    |> should.be_ok()
  repeated.deleted_ids |> should.equal([])
  repeated.deleted_paths |> should.equal([])
  assert_cleaned(fixture)
  simplifile.read(download_history.archive_path(fixture.layout.root))
  |> should.equal(Ok(history))
  simplifile.delete(fixture.layout.root) |> should.be_ok()
}

fn assert_cleaned(fixture: Fixture) {
  list.each(fixture.expired, fn(file) {
    let #(filename, _) = file
    simplifile.is_file(fixture.season <> "/" <> filename)
    |> should.equal(Ok(False))
  })
  assert_files(fixture.season, fixture.retained)
  simplifile.read(fixture.archive) |> should.equal(Ok("archive-state\n"))
  let ids = download_history.known_ids(fixture.layout.root) |> should.be_ok()
  list.length(ids) |> should.equal(2)
  list.contains(ids, "abc123DEF_-") |> should.be_true()
  list.contains(ids, "q1w2e3r4t5Y") |> should.be_true()
  list.contains(ids, "z9y8x7w6v5U") |> should.be_false()
}

fn assert_files(season: String, files: List(#(String, String))) {
  list.each(files, fn(file) {
    let #(filename, contents) = file
    simplifile.read(season <> "/" <> filename) |> should.equal(Ok(contents))
  })
}
