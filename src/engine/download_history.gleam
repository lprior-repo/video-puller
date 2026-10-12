/// Durable, global record of completed YouTube IDs.
///
/// The flat archive is compatible with yt-dlp's `--download-archive` format.
/// ytdl-sub's own per-show mapping archives are preserved separately and are
/// imported during refresh so prior downloads remain known across relabels.
import engine/media_info
import gleam/dict
import gleam/dynamic/decode
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import simplifile

/// Record a successfully completed YouTube video before its media is removed.
/// Repeated calls are safe and durable across process or machine restarts.
pub fn remember(data_root: String, video_id: String) -> Result(Nil, String) {
  case valid_id(video_id) {
    False -> Error("invalid YouTube video ID")
    True -> remember_ffi(data_root, video_id)
  }
}

/// Merge completed media and ytdl-sub mapping archives into the durable ledger.
/// Partially downloaded files are ignored; ytdl-sub mappings only contain
/// entries finalized by the download transaction.
pub fn refresh(data_root: String) -> Result(Int, String) {
  use _ <- result.try(ensure_cache(data_root))
  let previous = read_archive_ffi(data_root)
  use lines <- result.try(previous)
  use known <- result.try(parse_ids(lines))
  use from_archives <- result.try(mapping_ids(data_root))
  let from_media = completed_media_ids(data_root)
  let all_ids =
    list.unique(list.append(known, list.append(from_archives, from_media)))
  use _ <- result.try(remember_many(data_root, all_ids))
  Ok(list.length(all_ids))
}

/// Return the current archive path for ytdl-sub's per-job snapshot option.
pub fn archive_path(data_root: String) -> String {
  data_root <> "/ytdl-sub/download-history/archive.txt"
}

/// Write a fresh private yt-dlp archive snapshot for one ytdl-sub job.
/// ytdl-sub rolls back the configured archive after each URL, so this path is
/// never the shared ledger itself.
pub fn write_snapshot(
  data_root: String,
  snapshot_path: String,
) -> Result(Nil, String) {
  use _ <- result.try(refresh(data_root))
  use _ <- result.try(ensure_parent(snapshot_path))
  use entries <- result.try(read_archive_ffi(data_root))
  simplifile.write(snapshot_path, entries)
  |> result.map_error(fn(_) { "cannot write download archive snapshot" })
}

/// Read the known YouTube IDs from the shared ledger.
pub fn known_ids(data_root: String) -> Result(List(String), String) {
  read_archive_ffi(data_root)
  |> result.try(parse_ids)
}

/// Check whether a value can safely enter yt-dlp's archive as a YouTube ID.
pub fn is_youtube_id(value: String) -> Bool {
  valid_id(value)
}

fn remember_many(data_root: String, ids: List(String)) -> Result(Nil, String) {
  let ids = list.filter(ids, valid_id)
  remember_many_ffi(data_root, ids)
}

fn parse_ids(contents: String) -> Result(List(String), String) {
  let lines = string.split(contents, "\n")
  case list.all(lines, valid_archive_line) {
    False -> Error("download history archive contains an invalid entry")
    True ->
      Ok(
        lines
        |> list.filter_map(fn(line) {
          case string.split(string.trim(line), " ") {
            ["youtube", id] -> Ok(id)
            _ -> Error(Nil)
          }
        })
        |> list.unique,
      )
  }
}

fn valid_archive_line(line: String) -> Bool {
  let trimmed = string.trim(line)
  case string.is_empty(trimmed) || string.starts_with(trimmed, "#") {
    True -> True
    False ->
      case string.split(trimmed, " ") {
        ["youtube", id] -> valid_id(id)
        _ -> False
      }
  }
}

fn mapping_ids(data_root: String) -> Result(List(String), String) {
  use roots <- result.try(
    list.try_map(
      [data_root <> "/library", data_root <> "/retired-library"],
      fn(path) { walk_mappings(path, 0) },
    ),
  )
  Ok(list.flatten(roots) |> list.unique)
}

fn walk_mappings(
  directory: String,
  depth: Int,
) -> Result(List(String), String) {
  case depth > 8 || not_real_directory(directory) {
    True -> Ok([])
    False ->
      case simplifile.read_directory(directory) {
        Error(_) -> Error("cannot scan ytdl-sub download archives")
        Ok(entries) ->
          list.try_map(entries, fn(name) {
            let path = directory <> "/" <> name
            case
              string.starts_with(name, ".ytdl-sub-")
              && string.ends_with(name, "-download-archive.json")
            {
              True ->
                case real_file(path) {
                  True -> read_mapping_ids(path)
                  False -> Ok([])
                }
              False ->
                case real_directory(path) {
                  True -> walk_mappings(path, depth + 1)
                  False -> Ok([])
                }
            }
          })
          |> result.map(list.flatten)
      }
  }
}

fn read_mapping_ids(path: String) -> Result(List(String), String) {
  case simplifile.read(path) {
    Error(_) -> Error("cannot read ytdl-sub download archive " <> path)
    Ok(contents) ->
      case json.parse(contents, decode.dict(decode.string, mapping_decoder())) {
        Error(_) -> Error("cannot parse ytdl-sub download archive " <> path)
        Ok(entries) ->
          dict.to_list(entries)
          |> list.try_map(fn(entry) {
            case entry {
              #(id, extractor) ->
                case string.lowercase(extractor) == "youtube" && valid_id(id) {
                  True -> Ok([id])
                  False -> Ok([])
                }
            }
          })
          |> result.map(list.flatten)
      }
  }
}

fn mapping_decoder() -> decode.Decoder(String) {
  use extractor <- decode.field("extractor", decode.string)
  decode.success(extractor)
}

fn completed_media_ids(data_root: String) -> List(String) {
  let library_dir = data_root <> "/library"
  walk_media(library_dir, 0)
  |> list.filter_map(fn(path) {
    case media_info.read(path, data_root <> "/cache/video-metadata") {
      Some(metadata) ->
        case metadata.id {
          Some(id) ->
            case valid_id(id) {
              True -> Ok(id)
              False -> Error(Nil)
            }
          _ -> Error(Nil)
        }
      None -> Error(Nil)
    }
  })
  |> list.unique
}

fn walk_media(directory: String, depth: Int) -> List(String) {
  case depth > 8 || not_real_directory(directory) {
    True -> []
    False ->
      case simplifile.read_directory(directory) {
        Error(_) -> []
        Ok(entries) ->
          entries
          |> list.flat_map(fn(name) {
            let path = directory <> "/" <> name
            case not_real_directory(path) {
              True ->
                case is_media_file(name) && real_file(path) {
                  True -> [path]
                  False -> []
                }
              False -> walk_media(path, depth + 1)
            }
          })
      }
  }
}

fn is_media_file(name: String) -> Bool {
  list.any(
    [".mp4", ".mkv", ".webm", ".mov", ".mp3", ".m4a", ".opus", ".flac", ".wav"],
    fn(extension) { string.ends_with(name, extension) },
  )
}

fn valid_id(value: String) -> Bool {
  let allowed =
    string.to_graphemes(
      "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_-",
    )
  let characters = string.to_graphemes(value)
  list.length(characters) == 11
  && list.all(characters, fn(character) { list.contains(allowed, character) })
}

fn ensure_cache(data_root: String) -> Result(Nil, String) {
  simplifile.create_directory_all(data_root <> "/ytdl-sub/download-history")
  |> result.map_error(fn(_) { "cannot create download history directory" })
}

fn ensure_parent(path: String) -> Result(Nil, String) {
  let parent =
    string.split(path, "/")
    |> list.reverse
    |> list.drop(1)
    |> list.reverse
    |> string.join("/")
  let parent = case string.starts_with(path, "/") {
    True -> "/" <> parent
    False -> parent
  }
  simplifile.create_directory_all(parent)
  |> result.map_error(fn(_) { "cannot create archive snapshot directory" })
}

fn not_real_directory(path: String) -> Bool {
  case simplifile.is_symlink(path), simplifile.is_directory(path) {
    Ok(False), Ok(True) -> False
    _, _ -> True
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

@external(erlang, "download_history_ffi", "remember")
fn remember_ffi(data_root: String, video_id: String) -> Result(Nil, String)

@external(erlang, "download_history_ffi", "read_archive")
fn read_archive_ffi(data_root: String) -> Result(String, String)

@external(erlang, "download_history_ffi", "remember_many")
fn remember_many_ffi(
  data_root: String,
  ids: List(String),
) -> Result(Nil, String)
