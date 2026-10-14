/// Best-effort Plex library refresh and metadata repair.
///
/// Plex is optional at runtime. These operations quietly do nothing when the
/// local server is unavailable or has no configured Plex token. Configure the
/// server URL and library section with `PLEX_URL` and `PLEX_SECTION_ID`.
import engine/media_info
import engine/ytdl_sub
import envoy
import gleam/dict.{type Dict}
import gleam/dynamic/decode
import gleam/erlang/process
import gleam/int
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import infra/plex_scan_policy
import simplifile

/// Ask the configured local Plex server to refresh its video library section.
pub fn refresh_library() -> Nil {
  let _ = plex_refresh_library_result(scan_cooldown_ms())
  Nil
}

/// Plex scans asynchronously. Revisit titles after scans finish, including
/// while a poll is idle, so the final downloaded episode is enriched too.
pub fn start_title_sync(
  library_dir: String,
  trash_cleanup: fn(String) -> Result(Bool, String),
) -> Nil {
  case envoy.get("PLEX_SECTION_ID") {
    Ok(section) if section != "" -> {
      let _ =
        process.spawn(fn() { title_sync_loop(library_dir, trash_cleanup, None) })
      Nil
    }
    _ -> Nil
  }
  Nil
}

fn title_sync_loop(
  library_dir: String,
  trash_cleanup: fn(String) -> Result(Bool, String),
  last_error: Option(String),
) -> Nil {
  process.sleep(30_000)
  let error = case plex_library_videos_result() {
    Error(message) -> Some(message)
    Ok(videos) -> {
      cache_video_keys(videos)
      enrich_video_titles(library_dir, videos)
      let known = list.map(videos, fn(video) { video.1 })
      let files = ytdl_sub.list_library_media(library_dir)
      let scan_error = case needs_scan(files, known) {
        False -> None
        True ->
          case plex_refresh_library_result(scan_cooldown_ms()) {
            // Ok includes a shared-gate suppression. Keep any prior error until
            // a later library read confirms that Plex indexed every file.
            Ok(_) -> last_error
            Error(message) -> Some(message)
          }
      }
      case trash_cleanup(library_dir) {
        Ok(_) -> scan_error
        Error(message) ->
          case scan_error {
            Some(_) -> scan_error
            None -> Some(message)
          }
      }
    }
  }
  case error, last_error {
    Some(message), previous if previous != Some(message) ->
      io.println("Plex sync will retry: " <> message)
    None, Some(_) -> io.println("Plex sync recovered")
    _, _ -> Nil
  }
  title_sync_loop(library_dir, trash_cleanup, error)
}

fn scan_cooldown_ms() -> Int {
  plex_scan_policy.configured_cooldown_ms(
    case envoy.get("PLEX_SCAN_COOLDOWN_SECONDS") {
      Ok(value) -> Some(value)
      Error(_) -> None
    },
  )
}

/// A download completed while Plex was unavailable must trigger a later scan.
pub fn needs_scan(files: List(String), indexed: List(String)) -> Bool {
  list.any(files, fn(path) { !list.contains(indexed, path) })
}

/// Update existing Plex video titles from adjacent yt-dlp info JSON sidecars.
///
/// Plex's TV episode scanner may initially derive a date-prefixed title from
/// the season/episode filename. The sidecar preserves the original YouTube
/// title, which this function restores for files Plex already knows about.
pub fn enrich_titles(library_dir: String) -> Nil {
  case plex_library_videos_result() {
    Ok(videos) -> {
      cache_video_keys(videos)
      enrich_video_titles(library_dir, videos)
    }
    Error(_) -> Nil
  }
}

/// UI rendering uses the last successful Plex index without a network request.
pub fn cached_video_keys() -> Dict(String, String) {
  case simplifile.read(index_path()) {
    Ok(contents) ->
      json.parse(contents, decode.dict(decode.string, decode.string))
      |> result.unwrap(dict.new())
    Error(_) -> dict.new()
  }
}

pub fn video_page_url(rating_key: String) -> Option(String) {
  case envoy.get("PLEX_MACHINE_ID") {
    Ok(machine) if machine != "" -> {
      let base =
        envoy.get("PLEX_URL") |> result.unwrap("http://127.0.0.1:32400")
      let base = case string.ends_with(base, "/") {
        True -> string.drop_end(base, 1)
        False -> base
      }
      Some(
        base
        <> "/web/index.html#!/server/"
        <> machine
        <> "/details?key=%2Flibrary%2Fmetadata%2F"
        <> rating_key,
      )
    }
    _ -> None
  }
}

fn index_path() -> String {
  let root = envoy.get("DATA_DIR") |> result.unwrap("./data")
  root <> "/cache/plex-index.json"
}

fn cache_video_keys(videos: List(#(String, String, String))) -> Nil {
  let root = envoy.get("DATA_DIR") |> result.unwrap("./data")
  let directory = root <> "/cache"
  let path = index_path()
  let temporary = path <> ".tmp." <> int.to_string(unique_integer([Positive]))
  let content =
    videos
    |> list.map(fn(video) { #(video.1, json.string(video.0)) })
    |> json.object
    |> json.to_string
  case simplifile.create_directory_all(directory) {
    Error(_) -> Nil
    Ok(_) -> {
      case simplifile.write(temporary, content) {
        Ok(_) -> {
          let _ = simplifile.rename(temporary, path)
          Nil
        }
        Error(_) -> Nil
      }
    }
  }
}

fn enrich_video_titles(
  library_dir: String,
  videos: List(#(String, String, String)),
) -> Nil {
  let prefix = case string.ends_with(library_dir, "/") {
    True -> library_dir
    False -> library_dir <> "/"
  }
  let cache_dir =
    library_dir
    |> string.split("/")
    |> list.reverse
    |> list.drop(1)
    |> list.reverse
    |> string.join("/")
    |> fn(parent) { parent <> "/cache/video-metadata" }
  list.each(videos, fn(video) {
    let #(rating_key, path, plex_title) = video
    case string.starts_with(path, prefix), title_from_sidecar(path, cache_dir) {
      True, Some(title) if title != plex_title ->
        plex_update_title(rating_key, title)
      _, _ -> Nil
    }
  })
  Nil
}

fn title_from_sidecar(media_path: String, cache_dir: String) -> Option(String) {
  case media_info.read(media_path, cache_dir) {
    Some(metadata) ->
      case metadata.title {
        Some(title) ->
          case string.trim(title) {
            "" -> None
            title -> Some(title)
          }
        None -> None
      }
    None -> None
  }
}

/// ytdl-sub names sidecars after the media filename stem, so
/// `episode.mp4` has `episode.info.json` beside it.
pub fn info_sidecar_path(media_path: String) -> String {
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
      fn(extension) { string.ends_with(media_path, extension) },
    )
  {
    Ok(extension) ->
      string.drop_end(media_path, string.length(extension)) <> ".info.json"
    Error(_) -> media_path <> ".info.json"
  }
}

/// Parse a Plex media-container XML response into item key, file path, and
/// current title rows. Kept public so Plex's XML contract can be tested
/// without making a request to a running server.
pub fn parse_videos_xml(xml: String) -> List(#(String, String, String)) {
  plex_parse_videos(xml)
}

/// Parse a section-list XML document for one section key. The result contains
/// its type, scan state and configured locations, or an error on ambiguity.
pub fn parse_managed_section_xml(
  xml: String,
  section_id: String,
) -> Result(#(String, Bool, List(String)), String) {
  plex_parse_managed_section(xml, section_id)
}

/// Parse all media file paths in a Plex section response. Every Video must
/// include a Part file path for the response to be considered complete.
pub fn parse_section_paths_xml(xml: String) -> Result(List(String), String) {
  plex_parse_section_paths(xml)
}

/// Read a strict section snapshot for guarded trash cleanup.
pub fn managed_section_inventory_result() -> Result(
  #(String, Bool, List(String), List(String)),
  String,
) {
  plex_managed_section_inventory_result()
}

/// Return watched file paths, None when Plex cleanup is not configured, and an
/// error when a configured Plex server cannot be reached or parsed.
pub fn watched_video_paths_result() -> Result(Option(List(String)), String) {
  case plex_watched_videos_result() {
    Ok(#(False, _)) -> Ok(None)
    Ok(#(True, #(watched, active))) ->
      Ok(Some(exclude_active_paths(watched, active)))
    Error(message) -> Error(message)
  }
}

/// Keep media being watched in Plex out of any cleanup candidate set.
pub fn exclude_active_paths(
  watched: List(String),
  active: List(String),
) -> List(String) {
  list.filter(watched, fn(path) { !list.contains(active, path) })
}

/// Parse the file paths for Plex items with a positive viewCount.
pub fn parse_watched_paths_xml(xml: String) -> List(String) {
  plex_parse_watched_paths(xml)
}

/// Parse all active Plex session part paths. Invalid/non-container XML returns
/// an error so watched cleanup can fail closed instead of deleting a rewatched file.
pub fn parse_active_paths_xml(xml: String) -> Result(List(String), String) {
  plex_parse_active_paths(xml)
}

@external(erlang, "plex_ffi", "refresh_library_result")
fn plex_refresh_library_result(cooldown_ms: Int) -> Result(Nil, String)

@external(erlang, "plex_ffi", "library_videos_result")
fn plex_library_videos_result() -> Result(
  List(#(String, String, String)),
  String,
)

@external(erlang, "plex_ffi", "managed_section_inventory_result")
fn plex_managed_section_inventory_result() -> Result(
  #(String, Bool, List(String), List(String)),
  String,
)

@external(erlang, "plex_ffi", "watched_videos_result")
fn plex_watched_videos_result() -> Result(
  #(Bool, #(List(String), List(String))),
  String,
)

@external(erlang, "plex_ffi", "update_title")
fn plex_update_title(rating_key: String, title: String) -> Nil

@external(erlang, "plex_ffi", "parse_videos")
fn plex_parse_videos(xml: String) -> List(#(String, String, String))

@external(erlang, "plex_ffi", "parse_managed_section")
fn plex_parse_managed_section(
  xml: String,
  section_id: String,
) -> Result(#(String, Bool, List(String)), String)

@external(erlang, "plex_ffi", "parse_section_paths")
fn plex_parse_section_paths(xml: String) -> Result(List(String), String)

@external(erlang, "plex_ffi", "parse_watched_paths")
fn plex_parse_watched_paths(xml: String) -> List(String)

@external(erlang, "plex_ffi", "parse_active_paths")
fn plex_parse_active_paths(xml: String) -> Result(List(String), String)

type UniqueOption {
  Positive
}

@external(erlang, "erlang", "unique_integer")
fn unique_integer(options: List(UniqueOption)) -> Int
