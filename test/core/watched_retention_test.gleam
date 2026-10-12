import core/watched_retention
import engine/ytdl_sub
import gleam/erlang/process
import gleeunit
import gleeunit/should
import simplifile

pub fn main() {
  gleeunit.main()
}

pub fn deletes_only_watched_owned_media_after_durable_completion_test() {
  let root = "/tmp/test_watched_retention"
  let layout = ytdl_sub.layout(root)
  let managed = layout.library_dir <> "/Managed/Season 2026"
  let unmanaged = layout.library_dir <> "/Unmanaged/Season 2026"
  let _ = simplifile.create_directory_all(managed)
  let _ = simplifile.create_directory_all(unmanaged)
  let archive =
    layout.library_dir <> "/Managed/.ytdl-sub-Managed-download-archive.json"
  let _ = simplifile.write(archive, "archive")

  let watched = managed <> "/s2026.e101200 - Watched.mp4"
  let info = managed <> "/s2026.e101200 - Watched.info.json"
  let thumb = managed <> "/s2026.e101200 - Watched-thumb.jpg"
  let captions = managed <> "/s2026.e101200 - Watched.en-orig.srt"
  let unknown_sidecar = managed <> "/s2026.e101200 - Watched.backup.txt"
  let not_watched = managed <> "/s2026.e101201 - Not Watched.mp4"
  let unmanaged_media = unmanaged <> "/s2026.e101202 - Unmanaged.mp4"
  let outside = root <> "/outside.mp4"
  let symlink = managed <> "/s2026.e101203 - Linked.mp4"
  let _ = simplifile.write(watched, "media")
  let _ =
    simplifile.write(
      info,
      "{\"id\":\"abc123DEF_-\",\"webpage_url\":\"https://www.youtube.com/watch?v=abc123DEF_-\"}",
    )
  let _ = simplifile.write(thumb, "thumb")
  let _ = simplifile.write(captions, "captions")
  let _ = simplifile.write(unknown_sidecar, "keep")
  let _ = simplifile.write(not_watched, "keep")
  let _ = simplifile.write(unmanaged_media, "keep")
  let _ = simplifile.write(outside, "outside")
  let _ = simplifile.create_symlink(outside, symlink)

  let remembered = process.new_subject()
  let remember = fn(video_id) {
    simplifile.is_file(watched) |> should.equal(Ok(True))
    process.send(remembered, video_id)
    Ok(Nil)
  }
  let summary =
    watched_retention.prune(
      layout,
      [watched, unmanaged_media, outside, symlink, root <> "/../outside.mp4"],
      remember,
    )
    |> should.be_ok()

  process.receive(remembered, 1000) |> should.equal(Ok("abc123DEF_-"))
  summary.deleted_ids |> should.equal(["abc123DEF_-"])
  summary.deleted_paths |> should.equal([watched])
  simplifile.is_file(watched) |> should.equal(Ok(False))
  simplifile.is_file(info) |> should.equal(Ok(False))
  simplifile.is_file(thumb) |> should.equal(Ok(False))
  simplifile.is_file(captions) |> should.equal(Ok(False))
  simplifile.is_file(unknown_sidecar) |> should.equal(Ok(True))
  simplifile.is_file(not_watched) |> should.equal(Ok(True))
  simplifile.is_file(unmanaged_media) |> should.equal(Ok(True))
  simplifile.is_file(outside) |> should.equal(Ok(True))
  simplifile.is_symlink(symlink) |> should.equal(Ok(True))
  simplifile.is_file(archive) |> should.equal(Ok(True))
  let repeated =
    watched_retention.prune(layout, [watched], fn(_) {
      Error("must not re-record a deleted video")
    })
    |> should.be_ok()
  repeated.deleted_ids |> should.equal([])
  let _ = simplifile.delete(root)
  Nil
}

pub fn failed_completion_record_keeps_watched_media_test() {
  let root = "/tmp/test_watched_retention_history_error"
  let layout = ytdl_sub.layout(root)
  let season = layout.library_dir <> "/Managed/Season 2026"
  let _ = simplifile.create_directory_all(season)
  let _ =
    simplifile.write(
      layout.library_dir <> "/Managed/.ytdl-sub-Managed-download-archive.json",
      "archive",
    )
  let media = season <> "/s2026.e101200 - Watched.mp4"
  let _ = simplifile.write(media, "keep until recorded")
  let _ =
    simplifile.write(
      season <> "/s2026.e101200 - Watched.info.json",
      "{\"id\":\"abc123DEF_-\",\"webpage_url\":\"https://www.youtube.com/watch?v=abc123DEF_-\"}",
    )

  watched_retention.prune(layout, [media], fn(_) { Error("disk full") })
  |> should.be_error()
  simplifile.is_file(media) |> should.equal(Ok(True))
  let _ = simplifile.delete(root)
  Nil
}

pub fn unknown_id_media_is_never_deleted_test() {
  let root = "/tmp/test_watched_retention_unknown_id"
  let layout = ytdl_sub.layout(root)
  let season = layout.library_dir <> "/Managed/Season 2026"
  let _ = simplifile.create_directory_all(season)
  let _ =
    simplifile.write(
      layout.library_dir <> "/Managed/.ytdl-sub-Managed-download-archive.json",
      "archive",
    )
  let media = season <> "/s2026.e101200 - Watched.mp4"
  let _ = simplifile.write(media, "keep")

  let summary =
    watched_retention.prune(layout, [media], fn(_) { Ok(Nil) })
    |> should.be_ok()
  summary.deleted_ids |> should.equal([])
  simplifile.is_file(media) |> should.equal(Ok(True))
  let _ = simplifile.delete(root)
  Nil
}

pub fn symlinked_sidecar_preserves_media_and_retry_is_safe_test() {
  let root = "/tmp/test_watched_retention_partial_retry"
  let layout = ytdl_sub.layout(root)
  let season = layout.library_dir <> "/Managed/Season 2026"
  let _ = simplifile.create_directory_all(season)
  let _ =
    simplifile.write(
      layout.library_dir <> "/Managed/.ytdl-sub-Managed-download-archive.json",
      "archive",
    )
  let media = season <> "/s2026.e101200 - Watched.mp4"
  let sidecar = season <> "/s2026.e101200 - Watched.info.json"
  let unsafe_thumb = season <> "/s2026.e101200 - Watched-thumb.jpg"
  let outside = root <> "/outside.jpg"
  let _ = simplifile.write(media, "media")
  let _ =
    simplifile.write(
      sidecar,
      "{\"id\":\"abc123DEF_-\",\"webpage_url\":\"https://www.youtube.com/watch?v=abc123DEF_-\"}",
    )
  let _ = simplifile.write(outside, "outside")
  let _ = simplifile.create_symlink(outside, unsafe_thumb)
  let remembered = process.new_subject()
  let remember = fn(video_id) {
    process.send(remembered, video_id)
    Ok(Nil)
  }

  watched_retention.prune(layout, [media], remember)
  |> should.be_error()
  process.receive(remembered, 1000) |> should.equal(Ok("abc123DEF_-"))
  simplifile.is_file(media) |> should.equal(Ok(True))
  simplifile.is_file(outside) |> should.equal(Ok(True))

  let _ = simplifile.delete(unsafe_thumb)
  let retried =
    watched_retention.prune(layout, [media], remember) |> should.be_ok()
  retried.deleted_ids |> should.equal(["abc123DEF_-"])
  process.receive(remembered, 1000) |> should.equal(Ok("abc123DEF_-"))
  simplifile.is_file(media) |> should.equal(Ok(False))
  simplifile.is_file(sidecar) |> should.equal(Ok(False))
  simplifile.is_file(outside) |> should.equal(Ok(True))
  let _ = simplifile.delete(root)
  Nil
}
