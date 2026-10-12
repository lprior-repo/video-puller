/// Reconcile the desired subscriptions with managed media on the data volume.
/// Removed shows are moved on the same disk, retaining their download archive;
/// adding the show again restores it without downloading the same files twice.
import core/library_retention
import core/storage_guard
import core/watched_retention
import engine/channel_import
import engine/download_history
import engine/ytdl_sub
import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import infra/db.{type Db}
import infra/plex
import infra/subscription_repo
import simplifile

pub fn reconcile(db: Db, layout: ytdl_sub.Layout) -> Result(Nil, String) {
  use _ <- result.try(storage_guard.check_volume())
  use _ <- result.try(channel_import.sync_desired(layout.channels_file))
  use channels <- result.try(ytdl_sub.read_channels(layout.channels_file))
  // No retirement is allowed without a valid nonempty desired set.
  case channels {
    [] -> Error("refusing to reconcile an empty channel list")
    _ -> {
      let labels =
        ytdl_sub.managed_labels(
          channels,
          layout.channel_ids_file,
          layout.library_dir,
        )
      use _ <- result.try(reconcile_folders(layout.root, labels))
      // Seed durable IDs before any watched or age cleanup removes metadata.
      use _ <- result.try(download_history.refresh(layout.root))
      use _ <- result.try(
        subscription_repo.remove_inactive_channels(db, labels)
        |> result.map_error(fn(_) { "cannot reconcile channel records" }),
      )
      use pruned <- result.try(library_retention.prune(
        layout,
        system_time(Second),
      ))
      case
        watched_retention.cleanup(layout, fn(id) {
          download_history.remember(layout.root, id)
        })
      {
        Ok(watched) ->
          case watched.deleted_ids {
            [] -> Nil
            ids ->
              io.println(
                "Watched videos removed: " <> string.inspect(list.length(ids)),
              )
          }
        Error(error) -> io.println("Watched cleanup will retry: " <> error)
      }
      let live_ids =
        ytdl_sub.list_library_media(layout.library_dir)
        |> list.map(fn(path) {
          ytdl_sub.to_discovered_video(layout.library_dir, path).video_id
        })
      use _ <- result.try(
        subscription_repo.remove_missing_downloads(db, live_ids)
        |> result.map_error(fn(_) { "cannot reconcile missing video records" }),
      )
      use _ <- result.try(case pruned.cutoff_timestamp {
        None -> Ok(Nil)
        Some(cutoff) ->
          subscription_repo.remove_expired_videos(
            db,
            pruned.eligible_labels,
            cutoff,
          )
          |> result.map_error(fn(_) { "cannot remove expired video records" })
      })
      case pruned.deleted_ids {
        [] -> Nil
        ids ->
          io.println(
            "Expired channel videos removed: "
            <> string.inspect(list.length(ids)),
          )
      }
      plex.refresh_library()
      Ok(Nil)
    }
  }
}

type TimeUnit {
  Second
}

@external(erlang, "erlang", "system_time")
fn system_time(unit: TimeUnit) -> Int

/// Only directories containing a ytdl-sub archive are owned by this workflow.
/// Other directories are left alone, and conflicting destinations fail safely.
pub fn reconcile_folders(
  data_root: String,
  desired_labels: List(String),
) -> Result(Nil, String) {
  case desired_labels {
    [] -> Error("refusing to reconcile an empty channel list")
    _ -> {
      let library = data_root <> "/library"
      let retired = data_root <> "/retired-library"
      use _ <- result.try(
        simplifile.create_directory_all(retired)
        |> result.map_error(fn(_) { "cannot create retired library" }),
      )
      use archived <- result.try(read_directories(retired))
      use _ <- result.try(
        list.try_each(archived, fn(label) {
          case
            list.contains(desired_labels, label)
            && managed_show(retired <> "/" <> label)
          {
            True -> move_show(retired, library, label, "Restored")
            False -> Ok(Nil)
          }
        }),
      )
      use live <- result.try(read_directories(library))
      list.try_each(live, fn(label) {
        case
          !list.contains(desired_labels, label)
          && managed_show(library <> "/" <> label)
        {
          True -> move_show(library, retired, label, "Retired")
          False -> Ok(Nil)
        }
      })
    }
  }
}

fn read_directories(path: String) -> Result(List(String), String) {
  simplifile.read_directory(path)
  |> result.map_error(fn(_) { "cannot read " <> path })
  |> result.map(fn(names) {
    list.filter(names, fn(name) {
      simplifile.is_directory(path <> "/" <> name) |> result.unwrap(False)
    })
  })
}

fn managed_show(path: String) -> Bool {
  case simplifile.read_directory(path) {
    Error(_) -> False
    Ok(names) ->
      list.any(names, fn(name) {
        string.starts_with(name, ".ytdl-sub-")
        && string.ends_with(name, "-download-archive.json")
      })
  }
}

fn move_show(
  from: String,
  to: String,
  label: String,
  action: String,
) -> Result(Nil, String) {
  let destination = to <> "/" <> label
  case simplifile.is_directory(destination) {
    Ok(True) -> Error("refusing to overwrite existing show " <> destination)
    _ -> {
      use _ <- result.try(
        simplifile.rename(from <> "/" <> label, destination)
        |> result.map_error(fn(_) { "cannot move managed show " <> label }),
      )
      io.println(action <> " channel: " <> label)
      Ok(Nil)
    }
  }
}
