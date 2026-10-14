/// Safely clear missing items from only the dedicated managed Plex section.
/// Disabled by default; every filesystem and Plex precondition is fail-closed.
import core/storage_guard
import envoy
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import infra/plex
import simplifile

const max_depth = 8

/// Attempt a section-scoped Plex trash cleanup after the title-sync loop reads
/// the library. Returns false when disabled, gated, or any safety precondition
/// is not satisfied. Errors are safe to retry at the next title-sync tick.
pub fn cleanup(library_dir: String) -> Result(Bool, String) {
  case enabled() {
    False -> Ok(False)
    True -> {
      use volume <- result.try(required_env("STORAGE_VOLUME"))
      use volume_uuid <- result.try(required_env("STORAGE_VOLUME_UUID"))
      use data_dir <- result.try(required_env("DATA_DIR"))
      use _ <- result.try(storage_guard.check_volume())
      use _ <- result.try(validate_storage(
        library_dir,
        data_dir,
        volume,
        volume_uuid,
      ))
      use #(section_type, refreshing, locations, indexed_paths) <- result.try(
        plex.managed_section_inventory_result(),
      )
      use current_paths <- result.try(media_files(library_dir, 0))
      case
        inventory_is_safe(
          library_dir,
          section_type,
          refreshing,
          locations,
          indexed_paths,
          current_paths,
        )
      {
        False -> Ok(False)
        True -> {
          use missing <- result.try(missing_paths(
            library_dir,
            indexed_paths,
            current_paths,
          ))
          case missing {
            [] -> Ok(False)
            _ -> {
              // The SSD may have disappeared during the HTTP/filesystem checks.
              use _ <- result.try(storage_guard.check_volume())
              managed_empty_section_trash_result()
            }
          }
        }
      }
    }
  }
}

/// Pure gate used by tests: only the exact TV section and fully indexed current
/// managed files can proceed to missing-path verification.
pub fn inventory_is_safe(
  library_dir: String,
  section_type: String,
  refreshing: Bool,
  locations: List(String),
  indexed_paths: List(String),
  current_paths: List(String),
) -> Bool {
  canonical_absolute(library_dir)
  && section_type == "show"
  && !refreshing
  && locations == [library_dir]
  && current_paths != []
  && list.all(indexed_paths, fn(path) { within_root(path, library_dir) })
  && list.all(current_paths, fn(path) {
    within_root(path, library_dir) && list.contains(indexed_paths, path)
  })
}

fn enabled() -> Bool {
  case envoy.get("PLEX_MANAGED_TRASH_CLEANUP") {
    Ok(value) -> string.lowercase(string.trim(value)) == "true"
    Error(_) -> False
  }
}

fn required_env(name: String) -> Result(String, String) {
  case envoy.get(name) {
    Ok(value) -> {
      let value = string.trim(value)
      case value != "" {
        True -> Ok(value)
        False -> Error("managed Plex trash cleanup requires " <> name)
      }
    }
    _ -> Error("managed Plex trash cleanup requires " <> name)
  }
}

fn validate_storage(
  library_dir: String,
  data_dir: String,
  volume: String,
  _volume_uuid: String,
) -> Result(Nil, String) {
  let volume = drop_trailing_slashes(volume)
  let library_dir = drop_trailing_slashes(library_dir)
  let expected_library = drop_trailing_slashes(data_dir) <> "/library"
  use _ <- result.try(case library_dir == expected_library {
    True -> Ok(Nil)
    False -> Error("managed library must be DATA_DIR/library")
  })
  use _ <- result.try(
    case canonical_absolute(volume) && canonical_absolute(library_dir) {
      True -> Ok(Nil)
      False -> Error("managed storage paths must be canonical absolute paths")
    },
  )
  use _ <- result.try(case string.starts_with(library_dir, volume <> "/") {
    True -> Ok(Nil)
    False -> Error("managed Plex library is outside STORAGE_VOLUME")
  })
  use _ <- result.try(real_directory_chain(library_dir, volume))
  use volume_info <- result.try(
    simplifile.file_info(volume)
    |> result.map_error(fn(_) { "cannot inspect configured storage volume" }),
  )
  use library_info <- result.try(
    simplifile.file_info(library_dir)
    |> result.map_error(fn(_) { "cannot inspect managed library" }),
  )
  case volume_info.dev == library_info.dev {
    True -> Ok(Nil)
    False -> Error("managed library is not on the configured storage volume")
  }
}

fn real_directory(path: String, label: String) -> Result(Nil, String) {
  case simplifile.is_symlink(path), simplifile.is_directory(path) {
    Ok(False), Ok(True) -> Ok(Nil)
    Ok(True), _ -> Error(label <> " must not be a symbolic link")
    _, _ -> Error(label <> " is missing or unavailable")
  }
}

fn real_directory_chain(path: String, volume: String) -> Result(Nil, String) {
  use _ <- result.try(real_directory(path, "managed storage path"))
  case path == volume {
    True -> Ok(Nil)
    False -> {
      let parent = parent_path(path)
      case string.starts_with(parent, volume <> "/") || parent == volume {
        True -> real_directory_chain(parent, volume)
        False -> Error("managed storage path is outside STORAGE_VOLUME")
      }
    }
  }
}

fn media_files(directory: String, depth: Int) -> Result(List(String), String) {
  case depth > max_depth {
    True -> Error("managed library exceeds the expected nesting depth")
    False -> {
      use entries <- result.try(
        simplifile.read_directory(directory)
        |> result.map_error(fn(_) { "cannot read managed library directory" }),
      )
      list.try_map(entries, fn(entry) {
        let path = directory <> "/" <> entry
        case simplifile.is_symlink(path), simplifile.is_directory(path) {
          Ok(True), _ -> Error("managed library contains a symbolic link")
          Ok(False), Ok(True) -> media_files(path, depth + 1)
          Ok(False), Ok(False) -> media_file(path, entry)
          _, _ -> Error("cannot inspect managed library entry")
        }
      })
      |> result.map(list.flatten)
    }
  }
}

fn media_file(path: String, filename: String) -> Result(List(String), String) {
  case simplifile.is_file(path) {
    Ok(False) -> Ok([])
    Error(_) -> Error("cannot inspect managed library file")
    Ok(True) ->
      case is_media(filename) {
        True -> Ok([path])
        False -> Ok([])
      }
  }
}

pub fn missing_paths(
  library_dir: String,
  indexed_paths: List(String),
  current_paths: List(String),
) -> Result(List(String), String) {
  list.try_map(indexed_paths, fn(path) {
    case canonical_absolute(path) && within_root(path, library_dir) {
      False -> Error("Plex section contains an unsafe media path")
      True ->
        case list.contains(current_paths, path) {
          True -> Ok(None)
          False -> check_missing_file(path, library_dir)
        }
    }
  })
  |> result.map(fn(options) {
    options
    |> list.fold([], fn(found, value) {
      case value {
        Some(path) -> [path, ..found]
        None -> found
      }
    })
    |> list.reverse
  })
}

fn check_missing_file(
  path: String,
  library_dir: String,
) -> Result(Option(String), String) {
  case simplifile.is_symlink(path) {
    Ok(True) -> Error("Plex section contains a symlinked media path")
    Error(simplifile.Enoent) | Ok(False) ->
      check_media_file_info(path, library_dir)
    Error(_) -> Error("cannot inspect Plex media path")
  }
}

fn check_media_file_info(
  path: String,
  library_dir: String,
) -> Result(Option(String), String) {
  case simplifile.file_info(path) {
    Ok(_) -> Ok(None)
    Error(simplifile.Enoent) -> {
      use _ <- result.try(readable_existing_ancestor(
        parent_path(path),
        library_dir,
      ))
      Ok(Some(path))
    }
    Error(_) -> Error("Plex media path has a filesystem error")
  }
}

fn within_root(path: String, root: String) -> Bool {
  canonical_absolute(path)
  && canonical_absolute(drop_trailing_slashes(root))
  && string.starts_with(path, drop_trailing_slashes(root) <> "/")
}

fn parent_path(path: String) -> String {
  let segments = string.split(path, "/") |> list.drop(1)
  segments
  |> list.reverse
  |> list.drop(1)
  |> list.reverse
  |> string.join("/")
  |> fn(parent) { "/" <> parent }
}

fn readable_existing_ancestor(
  path: String,
  library_dir: String,
) -> Result(Nil, String) {
  case
    canonical_absolute(path)
    && { path == library_dir || within_root(path, library_dir) }
  {
    False -> Error("missing Plex media path has an unsafe parent")
    True ->
      case simplifile.file_info(path) {
        Error(simplifile.Enoent) -> {
          let parent = parent_path(path)
          case parent == path {
            True -> Error("no readable parent for missing Plex media path")
            False -> readable_existing_ancestor(parent, library_dir)
          }
        }
        Error(_) -> Error("cannot inspect parent of missing Plex media path")
        Ok(_) -> {
          use _ <- result.try(real_directory(path, "missing media ancestor"))
          use _ <- result.try(
            simplifile.read_directory(path)
            |> result.map_error(fn(_) {
              "cannot read parent of a missing Plex media path"
            }),
          )
          Ok(Nil)
        }
      }
  }
}

fn canonical_absolute(path: String) -> Bool {
  string.starts_with(path, "/")
  && path != "/"
  && case string.split(path, "/") {
    ["", ..segments] ->
      list.all(segments, fn(segment) {
        segment != "" && segment != "." && segment != ".."
      })
    _ -> False
  }
}

fn drop_trailing_slashes(path: String) -> String {
  case string.ends_with(path, "/") {
    True -> drop_trailing_slashes(string.drop_end(path, 1))
    False -> path
  }
}

fn is_media(filename: String) -> Bool {
  list.any(
    [".mp4", ".mkv", ".webm", ".mov", ".mp3", ".m4a", ".opus", ".flac", ".wav"],
    fn(extension) { string.ends_with(filename, extension) },
  )
}

@external(erlang, "plex_ffi", "empty_section_trash_result")
fn managed_empty_section_trash_result() -> Result(Bool, String)
