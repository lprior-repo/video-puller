/// Remove completed media Plex reports as watched, while recording a durable
/// video-ID completion entry before unlinking any owned file.
import core/episode_sidecars
import engine/ytdl_sub
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import infra/plex
import simplifile

pub type WatchedSummary {
  WatchedSummary(deleted_ids: List(String), deleted_paths: List(String))
}

/// Query Plex and remove only watched media inside ytdl-sub managed show
/// folders. When Plex is not configured this is a no-op; request and parse
/// failures are surfaced so reconciliation can retry without deleting files.
pub fn cleanup(
  layout: ytdl_sub.Layout,
  remember: fn(String) -> Result(Nil, String),
) -> Result(WatchedSummary, String) {
  case plex.watched_video_paths_result() {
    Error(message) -> Error(message)
    Ok(None) -> Ok(WatchedSummary([], []))
    Ok(Some(paths)) -> prune(layout, paths, remember)
  }
}

/// Testable cleanup operation over an already validated Plex path snapshot.
/// Candidate paths are discovered by walking real directories under the
/// library root, rather than trusting paths supplied by Plex.
pub fn prune(
  layout: ytdl_sub.Layout,
  watched_paths: List(String),
  remember: fn(String) -> Result(Nil, String),
) -> Result(WatchedSummary, String) {
  case real_directory(layout.library_dir) {
    False -> Ok(WatchedSummary([], []))
    True -> {
      use candidates <- result.try(watched_candidates(
        layout.library_dir,
        watched_paths,
      ))
      prune_candidates(candidates, layout, remember, WatchedSummary([], []))
    }
  }
}

fn watched_candidates(
  library_dir: String,
  watched_paths: List(String),
) -> Result(List(String), String) {
  case simplifile.read_directory(library_dir) {
    Error(_) -> Error("cannot read managed library for watched cleanup")
    Ok(shows) ->
      list.try_map(shows, fn(show_name) {
        let show = library_dir <> "/" <> show_name
        case real_directory(show) && has_download_archive(show) {
          False -> Ok([])
          True -> watched_show_candidates(show, watched_paths)
        }
      })
      |> result.map(list.flatten)
  }
}

fn watched_show_candidates(
  show: String,
  watched_paths: List(String),
) -> Result(List(String), String) {
  case simplifile.read_directory(show) {
    Error(_) -> Error("cannot read managed show directory " <> show)
    Ok(entries) ->
      list.try_map(entries, fn(entry) {
        let season = show <> "/" <> entry
        case real_directory(season) {
          False -> Ok([])
          True -> watched_season_candidates(season, watched_paths)
        }
      })
      |> result.map(list.flatten)
  }
}

fn watched_season_candidates(
  season: String,
  watched_paths: List(String),
) -> Result(List(String), String) {
  case simplifile.read_directory(season) {
    Error(_) -> Error("cannot read managed season directory " <> season)
    Ok(entries) ->
      Ok(
        list.filter(entries, fn(name) {
          let path = season <> "/" <> name
          is_media_file(name)
          && real_file(path)
          && list.contains(watched_paths, path)
        })
        |> list.map(fn(name) { season <> "/" <> name }),
      )
  }
}

fn prune_candidates(
  candidates: List(String),
  layout: ytdl_sub.Layout,
  remember: fn(String) -> Result(Nil, String),
  summary: WatchedSummary,
) -> Result(WatchedSummary, String) {
  case candidates {
    [] -> Ok(summary)
    [path, ..rest] -> {
      let video = ytdl_sub.to_discovered_video(layout.library_dir, path)
      case stable_youtube_id(video.video_id, video.url) {
        False -> prune_candidates(rest, layout, remember, summary)
        True -> {
          use _ <- result.try(
            remember(video.video_id)
            |> result.map_error(fn(_) {
              "cannot record watched video completion before deleting " <> path
            }),
          )
          use _ <- result.try(delete_media_and_sidecars(path))
          prune_candidates(
            rest,
            layout,
            remember,
            WatchedSummary(
              deleted_ids: [video.video_id, ..summary.deleted_ids],
              deleted_paths: [path, ..summary.deleted_paths],
            ),
          )
        }
      }
    }
  }
}

fn delete_media_and_sidecars(media_path: String) -> Result(Nil, String) {
  let filename = basename(media_path)
  let parent = parent_path(media_path)
  let stem = drop_media_extension(filename)
  case simplifile.read_directory(parent) {
    Error(_) -> Error("cannot read managed episode directory " <> parent)
    Ok(entries) -> {
      // Include every sibling media name, not just cleanup candidates. A
      // retained (or symlinked) sibling can own an otherwise matching sidecar.
      let other_stems =
        entries
        |> list.filter(fn(name) { name != filename && is_media_file(name) })
        |> list.map(drop_media_extension)
      let companions =
        list.filter(entries, fn(name) {
          name == filename || episode_sidecars.owned(stem, name, other_stems)
        })
      let paths = list.map(companions, fn(name) { parent <> "/" <> name })
      case list.all(paths, real_file) {
        False ->
          Error("refusing to delete a symlinked episode file " <> media_path)
        True ->
          simplifile.delete_all(paths)
          |> result.map_error(fn(_) {
            "cannot delete watched media " <> media_path
          })
      }
    }
  }
}

fn has_download_archive(show: String) -> Bool {
  case simplifile.read_directory(show) {
    Error(_) -> False
    Ok(entries) ->
      list.any(entries, fn(name) {
        string.starts_with(name, ".ytdl-sub-")
        && string.ends_with(name, "-download-archive.json")
        && real_file(show <> "/" <> name)
      })
  }
}

fn real_directory(path: String) -> Bool {
  case simplifile.is_symlink(path), simplifile.is_directory(path) {
    Ok(False), Ok(True) -> True
    _, _ -> False
  }
}

fn real_file(path: String) -> Bool {
  case simplifile.is_symlink(path), simplifile.is_file(path) {
    Ok(False), Ok(True) -> True
    _, _ -> False
  }
}

fn is_media_file(filename: String) -> Bool {
  list.any(
    [".mp4", ".mkv", ".webm", ".mov", ".mp3", ".m4a", ".opus", ".flac", ".wav"],
    fn(extension) { string.ends_with(filename, extension) },
  )
}

fn drop_media_extension(filename: String) -> String {
  case
    list.find(
      [
        ".mp4",
        ".mkv",
        ".webm",
        ".mov",
        ".mp3",
        ".m4a",
        ".opus",
        ".flac",
        ".wav",
      ],
      fn(extension) { string.ends_with(filename, extension) },
    )
  {
    Ok(extension) -> string.drop_end(filename, string.length(extension))
    Error(_) -> filename
  }
}

fn basename(path: String) -> String {
  path |> string.split("/") |> list.reverse |> list.first |> result.unwrap(path)
}

fn parent_path(path: String) -> String {
  path
  |> string.split("/")
  |> list.reverse
  |> list.drop(1)
  |> list.reverse
  |> string.join("/")
}

fn stable_youtube_id(video_id: String, url: String) -> Bool {
  let youtube_url =
    string.starts_with(url, "https://www.youtube.com/watch?v=")
    || string.starts_with(url, "https://youtube.com/watch?v=")
    || string.starts_with(url, "https://youtu.be/")
    || string.starts_with(url, "https://www.youtube.com/shorts/")
  youtube_url
  && string.length(video_id) == 11
  && list.all(string.to_graphemes(video_id), fn(char) {
    string.length(char) == 1
    && {
      char == "_"
      || char == "-"
      || string.contains(
        "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789",
        char,
      )
    }
  })
}
