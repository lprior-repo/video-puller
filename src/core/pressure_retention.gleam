/// Recover the storage reserve by evicting oldest completed regular-channel
/// episodes. Never remove completion history or unrelated movie libraries.
import core/storage_guard
import core/watched_retention
import engine/download_history
import engine/ytdl_sub
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/order
import gleam/result
import gleam/string
import infra/plex
import simplifile

type Candidate {
  Candidate(path: String, published_at: Int)
}

/// Called before opening SQLite at startup and before a subscription poll.
/// The guard stops running downloads first, so startup can reclaim space even
/// when SQLite cannot yet create a WAL. Recover an extra 5 GiB to avoid churn.
pub fn cleanup(layout: ytdl_sub.Layout) -> Result(List(String), String) {
  use space <- result.try(storage_guard.free_space_kib())
  let reserve = storage_guard.reserve_kib()
  case space {
    None -> Ok([])
    Some(free) if free >= reserve -> Ok([])
    Some(_) -> {
      use protected <- result.try(protected_paths())
      prune(
        layout,
        protected,
        fn() {
          use current <- result.try(storage_guard.free_space_kib())
          case current {
            Some(free) -> Ok(free >= reserve + 5 * 1024 * 1024)
            None -> Error("storage configuration disappeared during cleanup")
          }
        },
        fn(path) {
          use _ <- result.try(storage_guard.check_volume())
          use latest <- result.try(protected_paths())
          Ok(!list.contains(latest, path))
        },
      )
    }
  }
}

fn protected_paths() -> Result(List(String), String) {
  use paths <- result.try(plex.cleanup_protected_paths_result())
  case paths {
    Some(paths) -> Ok(paths)
    None -> Error("Plex playback state is required for low-space cleanup")
  }
}

/// Injectable checks allow testing space changes and drive disappearance
/// without filling a real disk. The live check also rechecks Plex playback.
pub fn prune(
  layout: ytdl_sub.Layout,
  protected: List(String),
  enough_space: fn() -> Result(Bool, String),
  can_delete: fn(String) -> Result(Bool, String),
) -> Result(List(String), String) {
  use candidates <- result.try(candidates(layout))
  let candidates =
    candidates
    |> list.filter(fn(item) { !list.contains(protected, item.path) })
    |> list.sort(fn(a, b) {
      case int.compare(a.published_at, b.published_at) {
        order.Eq -> string.compare(a.path, b.path)
        comparison -> comparison
      }
    })
  remove_oldest(candidates, layout, enough_space, can_delete, [])
}

fn remove_oldest(
  candidates: List(Candidate),
  layout: ytdl_sub.Layout,
  enough_space: fn() -> Result(Bool, String),
  can_delete: fn(String) -> Result(Bool, String),
  removed: List(String),
) -> Result(List(String), String) {
  use enough <- result.try(enough_space())
  case enough, candidates {
    True, _ | False, [] -> Ok(list.reverse(removed))
    False, [item, ..rest] -> {
      use permitted <- result.try(can_delete(item.path))
      case permitted {
        False -> remove_oldest(rest, layout, enough_space, can_delete, removed)
        True -> {
          // Existing watched-retention deletion validates every companion and
          // records the stable ID durably before unlinking the media.
          use summary <- result.try(
            watched_retention.prune(layout, [item.path], fn(id) {
              download_history.remember(layout.root, id)
            }),
          )
          remove_oldest(
            rest,
            layout,
            enough_space,
            can_delete,
            list.append(summary.deleted_ids, removed),
          )
        }
      }
    }
  }
}

fn candidates(layout: ytdl_sub.Layout) -> Result(List(Candidate), String) {
  use channels <- result.try(ytdl_sub.read_channels(layout.channels_file))
  let pinned =
    list.filter_map(channels, fn(channel) {
      case
        ytdl_sub.is_explicit_video_url(channel.url),
        ytdl_sub.youtube_video_id(channel.url)
      {
        True, Some(id) -> Ok(id)
        _, _ -> Error(Nil)
      }
    })
  let managed =
    ytdl_sub.managed_channels(
      channels,
      layout.channel_ids_file,
      layout.library_dir,
    )
  managed
  |> list.filter(fn(channel) { !ytdl_sub.is_explicit_video_url(channel.url) })
  |> list.try_map(fn(channel) {
    let label = case channel.label {
      Some(label) -> label
      None -> ytdl_sub.subscription_label(channel.url)
    }
    let show = layout.library_dir <> "/" <> label
    case
      real_directory(layout.root)
      && real_directory(layout.library_dir)
      && real_directory(show)
    {
      False -> Ok([])
      True -> {
        use entries <- result.try(read_directory(show))
        case
          list.any(entries, fn(name) {
            string.starts_with(name, ".ytdl-sub-")
            && string.ends_with(name, "-download-archive.json")
            && real_file(show <> "/" <> name)
          })
        {
          False -> Ok([])
          True ->
            entries
            |> list.filter(fn(name) { real_directory(show <> "/" <> name) })
            |> list.try_map(fn(season) {
              season_candidates(layout, show <> "/" <> season, pinned)
            })
            |> result.map(list.flatten)
        }
      }
    }
  })
  |> result.map(list.flatten)
}

fn season_candidates(
  layout: ytdl_sub.Layout,
  season: String,
  pinned: List(String),
) -> Result(List(Candidate), String) {
  use names <- result.try(read_directory(season))
  names
  |> list.filter(fn(name) {
    list.any([".mp4", ".mkv", ".webm", ".mov"], fn(ext) {
      string.ends_with(name, ext)
    })
    && real_file(season <> "/" <> name)
  })
  |> list.filter_map(fn(name) {
    let path = season <> "/" <> name
    let video = ytdl_sub.to_discovered_video(layout.library_dir, path)
    case
      download_history.is_youtube_id(video.video_id),
      list.contains(pinned, video.video_id),
      video.published_at
    {
      True, False, Some(date) -> Ok(Candidate(path, date))
      _, _, _ -> Error(Nil)
    }
  })
  |> Ok
}

fn real_directory(path: String) -> Bool {
  simplifile.is_symlink(path) == Ok(False)
  && simplifile.is_directory(path) == Ok(True)
}

fn real_file(path: String) -> Bool {
  simplifile.is_symlink(path) == Ok(False)
  && simplifile.is_file(path) == Ok(True)
}

fn read_directory(path: String) -> Result(List(String), String) {
  simplifile.read_directory(path)
  |> result.map_error(fn(_) {
    "cannot read low-space cleanup directory " <> path
  })
}
