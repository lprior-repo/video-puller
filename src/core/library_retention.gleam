/// Delete old episodes from live, managed channel libraries while retaining
/// ytdl-sub archives so future polls do not fetch removed videos again.
import core/episode_sidecars
import engine/download_history
import engine/ytdl_sub
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import simplifile

const seconds_per_day = 86_400

pub type PruneSummary {
  PruneSummary(
    deleted_ids: List(String),
    eligible_labels: List(String),
    cutoff_timestamp: Option(Int),
  )
}

/// Prune date-known media older than the configured UTC-day cutoff.
///
/// Only active regular-channel show folders with a ytdl-sub archive marker
/// are eligible. One-off video subscriptions, unknown dates, symlinks, and
/// unrelated files are left untouched. The summary includes media IDs removed
/// in this call and stable eligible labels/cutoff for retryable DB cleanup.
pub fn prune(
  layout: ytdl_sub.Layout,
  now_seconds: Int,
) -> Result(PruneSummary, String) {
  case layout.download_days <= 0 {
    True -> Ok(PruneSummary([], [], None))
    False -> {
      use channels <- result.try(ytdl_sub.read_channels(layout.channels_file))
      let managed =
        ytdl_sub.managed_channels(
          channels,
          layout.channel_ids_file,
          layout.library_dir,
        )
      let regular_channels =
        list.filter(managed, fn(channel) {
          !ytdl_sub.is_explicit_video_url(channel.url)
        })
      let pinned_ids =
        list.fold(channels, [], fn(ids, channel) {
          case
            ytdl_sub.is_explicit_video_url(channel.url),
            ytdl_sub.youtube_video_id(channel.url)
          {
            True, Some(id) -> [id, ..ids]
            _, _ -> ids
          }
        })
      let cutoff =
        utc_midnight(now_seconds) - layout.download_days * seconds_per_day
      case regular_channels {
        [] -> Ok(PruneSummary([], [], Some(cutoff)))
        _ -> {
          let labels =
            list.map(regular_channels, fn(channel) {
              option.unwrap(
                channel.label,
                ytdl_sub.subscription_label(channel.url),
              )
            })
          let eligible_labels =
            list.filter(labels, fn(label) {
              let show = layout.library_dir <> "/" <> label
              real_directory(show) && has_download_archive(show)
            })
          prune_labels(eligible_labels, layout, cutoff, pinned_ids, [])
          |> result.map(fn(deleted_ids) {
            PruneSummary(
              deleted_ids: list.unique(deleted_ids),
              eligible_labels: eligible_labels,
              cutoff_timestamp: Some(cutoff),
            )
          })
        }
      }
    }
  }
}

fn prune_labels(
  labels: List(String),
  layout: ytdl_sub.Layout,
  cutoff: Int,
  pinned_ids: List(String),
  deleted_ids: List(String),
) -> Result(List(String), String) {
  case labels {
    [] -> Ok(deleted_ids)
    [label, ..rest] -> {
      let show = layout.library_dir <> "/" <> label
      case real_directory(show) && has_download_archive(show) {
        False -> prune_labels(rest, layout, cutoff, pinned_ids, deleted_ids)
        True -> {
          use removed <- result.try(prune_show(
            show,
            layout.library_dir,
            cutoff,
            pinned_ids,
          ))
          prune_labels(
            rest,
            layout,
            cutoff,
            pinned_ids,
            list.append(deleted_ids, removed),
          )
        }
      }
    }
  }
}

fn prune_show(
  show: String,
  library_dir: String,
  cutoff: Int,
  pinned_ids: List(String),
) -> Result(List(String), String) {
  case simplifile.read_directory(show) {
    Error(_) -> Error("cannot read managed show directory " <> show)
    Ok(entries) -> {
      let seasons =
        list.filter(entries, fn(name) { real_directory(show <> "/" <> name) })
      list.try_map(seasons, fn(season) {
        prune_season(show <> "/" <> season, library_dir, cutoff, pinned_ids)
      })
      |> result.map(list.flatten)
    }
  }
}

fn prune_season(
  season: String,
  library_dir: String,
  cutoff: Int,
  pinned_ids: List(String),
) -> Result(List(String), String) {
  case simplifile.read_directory(season) {
    Error(_) -> Error("cannot read season directory " <> season)
    Ok(entries) -> {
      let old_media =
        list.filter(entries, fn(name) {
          is_media_file(name)
          && real_file(season <> "/" <> name)
          && is_older_episode(
            library_dir,
            season <> "/" <> name,
            cutoff,
            pinned_ids,
          )
        })
      list.try_map(old_media, fn(name) {
        let path = season <> "/" <> name
        let video = ytdl_sub.to_discovered_video(library_dir, path)
        use _ <- result.try(record_completion(
          video.video_id,
          layout_root(library_dir),
        ))
        use _ <- result.try(delete_episode_files(season, name))
        Ok(video.video_id)
      })
    }
  }
}

fn record_completion(
  video_id: String,
  data_root: String,
) -> Result(Nil, String) {
  case download_history.is_youtube_id(video_id) {
    False -> Ok(Nil)
    True ->
      download_history.remember(data_root, video_id)
      |> result.map_error(fn(_) {
        "cannot record expired video completion before deletion"
      })
  }
}

fn layout_root(library_dir: String) -> String {
  case string.ends_with(library_dir, "/library") {
    True -> string.drop_end(library_dir, string.length("/library"))
    False -> library_dir
  }
}

fn is_older_episode(
  library_dir: String,
  path: String,
  cutoff: Int,
  pinned_ids: List(String),
) -> Bool {
  let video = ytdl_sub.to_discovered_video(library_dir, path)
  case list.contains(pinned_ids, video.video_id), video.published_at {
    True, _ -> False
    False, Some(published_at) -> published_at < cutoff
    False, None -> False
  }
}

fn delete_episode_files(
  season: String,
  media_filename: String,
) -> Result(Nil, String) {
  let media_path = season <> "/" <> media_filename
  let stem = drop_extension(media_filename)
  case simplifile.read_directory(season) {
    Error(_) -> Error("cannot read episode directory " <> season)
    Ok(entries) -> {
      // Include every sibling media name, not just cleanup candidates. A
      // retained (or symlinked) sibling can own an otherwise matching sidecar.
      let other_stems =
        entries
        |> list.filter(fn(name) {
          name != media_filename && is_media_file(name)
        })
        |> list.map(drop_extension)
      let related =
        list.filter(entries, fn(name) {
          let path = season <> "/" <> name
          let is_episode_file =
            name == media_filename
            || episode_sidecars.owned(stem, name, other_stems)
          is_episode_file && real_file(path)
        })
      let paths = list.map(related, fn(name) { season <> "/" <> name })
      simplifile.delete_all(paths)
      |> result.map_error(fn(_) {
        "cannot remove expired episode " <> media_path
      })
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

fn drop_extension(filename: String) -> String {
  case string.split(filename, ".") |> list.reverse {
    [_extension, ..rest] -> rest |> list.reverse |> string.join(".")
    [] -> filename
  }
}

fn utc_midnight(now_seconds: Int) -> Int {
  now_seconds - now_seconds % seconds_per_day
}
