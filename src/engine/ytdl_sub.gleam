/// ytdl-sub engine wrapper
///
/// Wraps the `ytdl-sub` subscription downloader so the application can pull
/// public channel URLs without browser cookies, sequentially, on the
/// application's own polling schedule.
import domain/subscription_types.{type DiscoveredVideo, DiscoveredVideo}
import engine/shell
import envoy
import gleam/dict.{type Dict}
import gleam/dynamic/decode
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import simplifile

/// A configured YouTube channel.
pub type Channel {
  Channel(label: Option(String), url: String)
}

type CacheRow {
  CacheRow(url: String, id: String, title: Option(String))
}

/// Paths used by the ytdl-sub integration
pub type Layout {
  Layout(
    root: String,
    config_file: String,
    subscriptions_file: String,
    channels_file: String,
    channel_ids_file: String,
    takeout_file: String,
    library_dir: String,
    work_dir: String,
  )
}

/// Identifier for the download engine executable
const engine_command = "ytdl-sub"

const default_data_dir = "./data"

const default_channels_template = "./priv/ytdl-sub/channels.txt"

/// Summary of a single ytdl-sub run
pub type PullSummary {
  PullSummary(downloaded: Int, added_files: List(String), errors: List(String))
}

/// Build the layout from environment configuration
pub fn layout_from_env() -> Layout {
  layout(get_env_string("DATA_DIR", default_data_dir))
}

/// Build the layout for a given data root
///
/// Paths are resolved to absolute ones: the engine prints library paths in its
/// transaction log verbatim, and the parser depends on absolute directory
/// lines, so a relative `DATA_DIR` must not leak into the generated config.
pub fn layout(root: String) -> Layout {
  let root = absolute_path(root)
  let ytdl_sub_dir = root <> "/ytdl-sub"
  Layout(
    root: root,
    config_file: ytdl_sub_dir <> "/config.yaml",
    subscriptions_file: ytdl_sub_dir <> "/subscriptions.yaml",
    channels_file: ytdl_sub_dir <> "/channels.txt",
    channel_ids_file: ytdl_sub_dir <> "/channel_ids.txt",
    takeout_file: ytdl_sub_dir <> "/subscriptions.csv",
    library_dir: root <> "/library",
    work_dir: ytdl_sub_dir <> "/working",
  )
}

/// Resolve a path against the working directory unless it is already absolute
fn absolute_path(path: String) -> String {
  case string.starts_with(path, "/") {
    True -> path
    False -> {
      let path = case string.starts_with(path, "./") {
        True -> string.drop_start(path, 2)
        False -> path
      }
      case simplifile.current_directory() {
        Ok(cwd) -> cwd <> "/" <> path
        Error(_) -> path
      }
    }
  }
}

/// Create the directories, config file and channel list the engine needs
pub fn ensure_layout(layout: Layout) -> Result(Nil, String) {
  use _ <- result.try(create_directory(layout.root))
  use _ <- result.try(create_directory(layout.work_dir))
  use _ <- result.try(create_directory(layout.library_dir))
  use _ <- result.try(ensure_config_file(layout))
  use _ <- result.try(ensure_channels_file(layout))
  Ok(Nil)
}

fn create_directory(path: String) -> Result(Nil, String) {
  simplifile.create_directory_all(path)
  |> result.map_error(fn(err) {
    "cannot create directory " <> path <> ": " <> file_error(err)
  })
}

fn ensure_config_file(layout: Layout) -> Result(Nil, String) {
  case simplifile.is_file(layout.config_file) {
    Ok(True) -> Ok(Nil)
    _ -> write_config_file(layout)
  }
}

fn write_config_file(layout: Layout) -> Result(Nil, String) {
  let content =
    "configuration:\n"
    <> "  working_directory: \""
    <> escape_yaml(layout.work_dir)
    <> "\"\n"
  simplifile.write(layout.config_file, content)
  |> result.map_error(fn(err) {
    "cannot write " <> layout.config_file <> ": " <> file_error(err)
  })
}

fn ensure_channels_file(layout: Layout) -> Result(Nil, String) {
  case simplifile.is_file(layout.channels_file) {
    Ok(True) -> Ok(Nil)
    _ -> copy_channels_template(layout)
  }
}

fn copy_channels_template(layout: Layout) -> Result(Nil, String) {
  let template = get_env_string("CHANNELS_TEMPLATE", default_channels_template)
  case simplifile.read(template) {
    Ok(content) ->
      simplifile.write(layout.channels_file, content)
      |> result.map_error(fn(err) {
        "cannot write " <> layout.channels_file <> ": " <> file_error(err)
      })
    Error(err) ->
      Error(
        "no channel list at "
        <> layout.channels_file
        <> " and template "
        <> template
        <> " is unreadable: "
        <> file_error(err),
      )
  }
}

/// Parse one channels.txt line.
pub fn parse_channel_line(line: String) -> Option(Channel) {
  let line = string.trim(line)
  case string.is_empty(line) || string.starts_with(line, "#") {
    True -> None
    False ->
      case string.split(line, " = ") {
        [label, url] ->
          case string.starts_with(url, "http"), string.is_empty(label) {
            True, False ->
              Some(Channel(Some(label), canonical_channel_url(url)))
            _, _ -> None
          }
        _ ->
          case string.starts_with(line, "http") {
            True -> Some(Channel(None, canonical_channel_url(line)))
            False -> None
          }
      }
  }
}

/// Remove URL decoration that does not identify a different YouTube channel
///
/// Fragments and decorative queries are dropped for channel URLs, but a query
/// that addresses the content (`/watch?v=`, `/playlist?list=`) is kept: those
/// URLs are accepted as subscriptions too, and stripping the query would turn
/// them into a different URL.
pub fn canonical_channel_url(url: String) -> String {
  let #(without_fragment, _) = split_once(url, "#")
  let #(base, query) = split_once(without_fragment, "?")
  let trimmed = case string.ends_with(string.trim(base), "/") {
    True -> string.drop_end(string.trim(base), 1)
    False -> string.trim(base)
  }
  let canonical = strip_trailing_tab(trimmed)
  case query {
    Some(query) ->
      case query_addresses_content(canonical) {
        True -> canonical <> "?" <> query
        False -> canonical
      }
    None -> canonical
  }
}

/// Whether the query string identifies the content instead of decorating it
fn query_addresses_content(url: String) -> Bool {
  list.any(["/watch", "/playlist", "/embed/"], fn(marker) {
    string.contains(url, marker)
  })
}

/// Split at the first occurrence of a delimiter
fn split_once(value: String, delimiter: String) -> #(String, Option(String)) {
  case string.split(value, delimiter) {
    [head, rest, ..] -> #(head, Some(rest))
    _ -> #(value, None)
  }
}

fn strip_trailing_tab(url: String) -> String {
  let last_segment = case list.last(string.split(url, "/")) {
    Ok(last) -> Some(last)
    Error(_) -> None
  }
  case last_segment {
    Some(last) ->
      case list.contains(tab_segments, last) {
        True ->
          strip_trailing_tab(string.drop_end(url, string.length(last) + 1))
        False -> url
      }
    None -> url
  }
}

/// Read and normalize the channel list (labels and # comments are supported).
pub fn read_channels(path: String) -> Result(List(Channel), String) {
  use content <- result.try(
    simplifile.read(path)
    |> result.map_error(fn(err) {
      "cannot read " <> path <> ": " <> file_error(err)
    }),
  )

  Ok(
    content
    |> string.split("\n")
    |> list.filter_map(fn(line) { found(parse_channel_line(line)) }),
  )
}

/// Resolve channel IDs from the on-disk cache or yt-dlp.
///
/// Every lookup is best effort: an unavailable binary, network error, non-zero
/// exit, or malformed output leaves that channel usable but unresolved.
pub fn resolve_channel_ids(
  channels: List(Channel),
  cache_path: String,
) -> List(Channel) {
  let cache = load_cache(cache_path)
  let #(resolved, updated_cache) =
    list.fold(channels, #([], cache), fn(acc, channel) {
      let #(resolved, cache) = acc
      let canonical = canonical_channel_url(channel.url)
      let channel = Channel(channel.label, canonical)
      let cached = cache_row(cache, canonical)
      let id = embedded_channel_id(canonical)
      case id, cached {
        Some(id), _ -> #(
          [apply_cache_title(channel, cached), ..resolved],
          ensure_cache(cache, canonical, id, cached),
        )
        None, Some(CacheRow(_, _cached_id, _)) -> #(
          [apply_cache_title(channel, cached), ..resolved],
          cache,
        )
        None, None ->
          case lookup_channel(canonical) {
            Some(#(found_id, title)) -> {
              let found = CacheRow(canonical, found_id, title)
              #(
                [apply_cache_title(channel, Some(found)), ..resolved],
                ensure_cache(cache, canonical, found_id, Some(found)),
              )
            }
            None -> #([channel, ..resolved], cache)
          }
      }
    })
  write_cache(cache_path, updated_cache)
  list.reverse(resolved)
}

/// Deduplicate channels by immutable ID where known, otherwise by URL.
pub fn dedupe_channels(
  channels: List(Channel),
  ids: Dict(String, String),
  library_dir: String,
) -> List(Channel) {
  let groups =
    list.fold(channels, [], fn(groups, channel) {
      let canonical = canonical_channel_url(channel.url)
      let key = case embedded_channel_id(canonical) {
        Some(id) -> "id:" <> id
        None ->
          case dict.get(ids, canonical) {
            Ok(id) -> "id:" <> id
            Error(_) -> "url:" <> canonical
          }
      }
      add_group(groups, key, canonical, channel.label)
    })
  list.map(groups, fn(group) {
    let derived = subscription_label(group.url)
    let label = choose_label(group.labels, derived, library_dir)
    Channel(Some(label), group.url)
  })
}

/// Generate the ytdl-sub subscriptions file for the configured channels.
pub fn write_subscriptions(
  layout: Layout,
  channels: List(Channel),
) -> Result(Nil, String) {
  let resolved = resolve_channel_ids(channels, layout.channel_ids_file)
  let ids = channel_ids(resolved, load_cache(layout.channel_ids_file))
  let channels = dedupe_channels(resolved, ids, layout.library_dir)
  let content =
    "__preset__:\n"
    <> "  overrides:\n"
    <> "    tv_show_directory: \""
    <> escape_yaml(layout.library_dir)
    <> "\"\n"
    // The resolution assert aborts on any download below 361p, which
    // false-positives on genuinely low-res uploads and skips them forever;
    // throttle protection's request pacing stays enabled.
    <> "    enable_resolution_assert: False\n"
    <> "\n"
    <> "Plex TV Show by Date:\n"
    <> "  = YouTube:\n"
    <> list.fold(channels, "", fn(acc, channel) {
      let label = case channel.label {
        Some(value) -> value
        None -> subscription_label(channel.url)
      }
      acc
      <> "    \""
      <> escape_yaml(label)
      <> "\": \""
      <> escape_yaml(channel.url)
      <> "\"\n"
    })

  simplifile.write(layout.subscriptions_file, content)
  |> result.map_error(fn(err) {
    "cannot write " <> layout.subscriptions_file <> ": " <> file_error(err)
  })
}

/// Label used as the subscription name for a channel URL.
pub fn subscription_label(url: String) -> String {
  let segments =
    canonical_channel_url(url)
    |> string.split("/")
    |> list.filter(fn(segment) { !string.is_empty(segment) })
    |> list.reverse
  let segment = case segments {
    [last, ..] -> last
    [] -> canonical_channel_url(url)
  }
  case string.starts_with(segment, "@") {
    True -> string.drop_start(segment, 1)
    False -> segment
  }
}

const tab_segments = [
  "videos",
  "streams",
  "shorts",
  "playlists",
  "featured",
  "live",
  "about",
]

type ChannelGroup {
  ChannelGroup(key: String, url: String, labels: List(String))
}

fn add_group(
  groups: List(ChannelGroup),
  key: String,
  url: String,
  label: Option(String),
) -> List(ChannelGroup) {
  case groups {
    [] -> [ChannelGroup(key, url, option_to_list(label))]
    [ChannelGroup(group_key, first_url, labels), ..rest] ->
      case group_key == key {
        True -> [
          ChannelGroup(
            group_key,
            first_url,
            list.append(labels, option_to_list(label)),
          ),
          ..rest
        ]
        False -> [
          ChannelGroup(group_key, first_url, labels),
          ..add_group(rest, key, url, label)
        ]
      }
  }
}

fn option_to_list(value: Option(String)) -> List(String) {
  case value {
    Some(item) -> [item]
    None -> []
  }
}

/// Bridge Option-returning parsers into list helpers that expect Result
fn found(value: Option(a)) -> Result(a, Nil) {
  case value {
    Some(inner) -> Ok(inner)
    None -> Error(Nil)
  }
}

fn choose_label(
  explicit: List(String),
  derived: String,
  library_dir: String,
) -> String {
  case
    list.find(explicit, fn(label) { is_library_directory(library_dir, label) })
  {
    Ok(label) -> label
    Error(_) ->
      case is_library_directory(library_dir, derived) {
        True -> derived
        False ->
          case explicit {
            [first, ..] -> first
            [] -> derived
          }
      }
  }
}

fn is_library_directory(library_dir: String, label: String) -> Bool {
  case simplifile.is_directory(library_dir <> "/" <> label) {
    Ok(True) -> True
    _ -> False
  }
}

fn embedded_channel_id(url: String) -> Option(String) {
  let parts =
    canonical_channel_url(url)
    |> string.split("/")
    |> list.filter(fn(part) { !string.is_empty(part) })
  case parts {
    [_, _, "channel", id, ..] -> Some(id)
    [_, "channel", id, ..] -> Some(id)
    _ -> None
  }
}

fn load_cache(path: String) -> List(CacheRow) {
  case simplifile.read(path) {
    Ok(content) ->
      content
      |> string.split("\n")
      |> list.filter_map(fn(line) { found(parse_cache_row(line)) })
    Error(_) -> []
  }
}

fn parse_cache_row(line: String) -> Option(CacheRow) {
  case string.split(string.trim(line), "\t") {
    [url, id, title, ..] ->
      case string.is_empty(url) || string.is_empty(id) {
        True -> None
        False ->
          Some(CacheRow(canonical_channel_url(url), id, nonempty_option(title)))
      }
    [url, id] ->
      case string.is_empty(url) || string.is_empty(id) {
        True -> None
        False -> Some(CacheRow(canonical_channel_url(url), id, None))
      }
    _ -> None
  }
}

fn nonempty_option(value: String) -> Option(String) {
  case string.is_empty(value) {
    True -> None
    False -> Some(value)
  }
}

fn cache_row(rows: List(CacheRow), url: String) -> Option(CacheRow) {
  case rows {
    [] -> None
    [row, ..rest] ->
      case row {
        CacheRow(row_url, _, _) ->
          case row_url == url {
            True -> Some(row)
            False -> cache_row(rest, url)
          }
      }
  }
}

fn ensure_cache(
  rows: List(CacheRow),
  url: String,
  id: String,
  row: Option(CacheRow),
) -> List(CacheRow) {
  let title = case row {
    Some(CacheRow(_, _, value)) -> value
    None -> None
  }
  let replacement = CacheRow(url, id, title)
  replace_cache_row(rows, url, replacement)
}

fn replace_cache_row(
  rows: List(CacheRow),
  url: String,
  replacement: CacheRow,
) -> List(CacheRow) {
  case rows {
    [] -> [replacement]
    [row, ..rest] ->
      case row {
        CacheRow(row_url, _, _) ->
          case row_url == url {
            True -> [replacement, ..rest]
            False -> [row, ..replace_cache_row(rest, url, replacement)]
          }
      }
  }
}

fn apply_cache_title(channel: Channel, cached: Option(CacheRow)) -> Channel {
  case channel.label, cached {
    None, Some(CacheRow(_, _, Some(title))) -> Channel(Some(title), channel.url)
    _, _ -> channel
  }
}

fn lookup_channel(url: String) -> Option(#(String, Option(String))) {
  let args = [
    "--no-warnings",
    "--flat-playlist",
    "--playlist-items",
    "1",
    // One field per line: yt-dlp escapes tabs in --print output, so a
    // tab-joined template arrives as a literal "\t" and parses as one field.
    "--print",
    "%(playlist_channel_id)s",
    "--print",
    "%(playlist_channel)s",
    url,
  ]
  case shell.run_with_timeout("yt-dlp", args, 30_000) {
    Ok(shell_result) ->
      case shell_result.exit_code {
        0 -> parse_lookup_output(shell_result.stdout)
        _ -> None
      }
    Error(_) -> None
  }
}

/// Read the channel id and title from the lookup's print output
pub fn parse_lookup_output(
  stdout: String,
) -> Option(#(String, Option(String))) {
  case string.split(string.trim(stdout), "\n") {
    [id, title, ..] ->
      case string.is_empty(string.trim(id)) {
        True -> None
        False -> Some(#(string.trim(id), nonempty_option(string.trim(title))))
      }
    [id] ->
      case string.is_empty(string.trim(id)) {
        True -> None
        False -> Some(#(string.trim(id), None))
      }
    _ -> None
  }
}

fn write_cache(path: String, rows: List(CacheRow)) -> Nil {
  let content =
    rows
    |> list.map(fn(row) {
      case row {
        CacheRow(url, id, Some(title)) -> url <> "\t" <> id <> "\t" <> title
        CacheRow(url, id, None) -> url <> "\t" <> id
      }
    })
    |> string.join("\n")
  case content {
    "" -> Nil
    _ ->
      case simplifile.write(path, content <> "\n") {
        Ok(_) -> Nil
        Error(_) -> Nil
      }
  }
}

fn channel_ids(
  channels: List(Channel),
  cache: List(CacheRow),
) -> Dict(String, String) {
  list.fold(channels, dict.new(), fn(ids, channel) {
    let canonical = canonical_channel_url(channel.url)
    case embedded_channel_id(canonical) {
      Some(id) -> dict.insert(ids, canonical, id)
      None ->
        case cache_row(cache, canonical) {
          Some(CacheRow(_, id, _)) -> dict.insert(ids, canonical, id)
          None -> ids
        }
    }
  })
}

/// Run a full ytdl-sub pass over the subscriptions file
pub fn run_pull(
  layout: Layout,
  timeout_ms: Int,
) -> Result(PullSummary, String) {
  let args = [
    "--suppress-colors",
    "--config",
    layout.config_file,
    "sub",
    layout.subscriptions_file,
  ]

  case shell.run_with_timeout(engine_command, args, timeout_ms) {
    Ok(result) -> interpret_run_result(result.exit_code, result.stdout)
    Error(shell.ExecutionError(message)) ->
      Error("execution error: " <> message)
    Error(shell.InvalidCommand(message)) ->
      Error("invalid command: " <> message)
  }
}

/// Decide the outcome of an engine run from its exit code and output
///
/// ytdl-sub exits non-zero when any subscription fails, even when other
/// subscriptions in the same run downloaded successfully. In that case the
/// added files are kept so callers still record them; the summary's errors
/// list carries the per-subscription failures.
pub fn interpret_run_result(
  exit_code: Int,
  stdout: String,
) -> Result(PullSummary, String) {
  let summary = parse_pull_output(stdout)
  case exit_code, summary.added_files {
    0, _ -> Ok(summary)
    _, [] ->
      Error(
        "ytdl-sub failed with exit code "
        <> int.to_string(exit_code)
        <> ": "
        <> first_error_line(stdout),
      )
    _, _ ->
      Ok(
        PullSummary(
          ..summary,
          // Partial success still keeps the files, but a non-zero exit must
          // never be reported as an error-free poll
          errors: case summary.errors {
            [] -> [
              "ytdl-sub exited with code "
              <> int.to_string(exit_code)
              <> ": "
              <> first_error_line(stdout),
            ]
            _ -> summary.errors
          },
        ),
      )
  }
}

/// Parse the transaction log and download summary from a ytdl-sub run
///
/// The engine prints created files as an absolute directory line at the outer
/// indent followed by the file names written there:
///
///     Files created:
///     ----------------------------------------
///     /library/Channel One/Season 2026
///       s2026.e091101 - Video.mp4
///         Video Tags:
///           ...
///
/// Only those directory lines and their media files are read; the per-file
/// detail block below a file (which can contain arbitrary text, blank lines
/// and URLs) is ignored.
pub fn parse_pull_output(output: String) -> PullSummary {
  let lines = string.split(output, "\n")

  let #(files, _dir, _in_section) =
    list.fold(lines, #([], "", False), fn(acc, line) {
      let #(found, dir, in_section) = acc
      let trimmed = string.trim_start(line)
      let indent = string.length(line) - string.length(trimmed)

      case classify_log_line(trimmed, indent, in_section) {
        SectionStart -> #(found, dir, True)
        SectionEnd -> #(found, dir, False)
        CreatedDirectory(path) -> #(found, path, True)
        CreatedFile(name) ->
          case dir {
            "" -> #(found, dir, True)
            current -> #([current <> "/" <> name, ..found], dir, True)
          }
        LogIgnored -> #(found, dir, in_section)
      }
    })

  let added = list.reverse(files)

  PullSummary(
    downloaded: list.length(added),
    added_files: added,
    errors: list.filter(lines, is_error_line),
  )
}

type LogLine {
  SectionStart
  SectionEnd
  CreatedDirectory(String)
  CreatedFile(String)
  LogIgnored
}

fn classify_log_line(
  trimmed: String,
  indent: Int,
  in_section: Bool,
) -> LogLine {
  case string.is_empty(trimmed) {
    True -> LogIgnored
    False ->
      case string.contains(trimmed, " | ") {
        True -> LogIgnored
        False ->
          case trimmed {
            "Files created:" -> SectionStart
            _ ->
              // "Files modified:" / "Files removed:" are their own sets: a media
              // name under them was not created by this run
              case is_file_set_heading(trimmed, indent) {
                True -> SectionEnd
                False ->
                  case string.starts_with(trimmed, "[") {
                    True -> SectionEnd
                    False ->
                      case in_section, indent {
                        True, 0 ->
                          case string.starts_with(trimmed, "/") {
                            True -> CreatedDirectory(trimmed)
                            False -> LogIgnored
                          }
                        True, 2 ->
                          case is_media_file(trimmed) {
                            True -> CreatedFile(trimmed)
                            False -> LogIgnored
                          }
                        _, _ -> LogIgnored
                      }
                  }
              }
          }
      }
  }
}

/// The engine's transaction-log set headings, e.g. "Files modified:"
fn is_file_set_heading(trimmed: String, indent: Int) -> Bool {
  indent == 0
  && string.starts_with(trimmed, "Files ")
  && string.ends_with(trimmed, ":")
}

/// Convert a downloaded file path into a seen-video record
pub fn to_discovered_video(
  library_dir: String,
  path: String,
) -> DiscoveredVideo {
  let relative = case string.starts_with(path, library_dir) {
    True -> string.drop_start(path, string.length(library_dir))
    False -> path
  }
  let relative = case string.starts_with(relative, "/") {
    True -> string.drop_start(relative, 1)
    False -> relative
  }

  let channel = case relative |> string.split("/") {
    [first, ..] -> first
    [] -> ""
  }

  let filename = case list.last(string.split(relative, "/")) {
    Ok(name) -> name
    Error(_) -> relative
  }

  let basename = drop_extension(filename)
  let #(title, published_at) = split_episode_title(basename)
  // Prefer the stable YouTube id from the engine's sidecar: the file name can
  // change (re-titles, aliases), the id cannot, so records stay unique
  let #(video_id, channel_id) = sidecar_identity(path, relative)

  DiscoveredVideo(
    video_id: video_id,
    channel_id: channel_id,
    channel_name: case channel {
      "" -> None
      name -> Some(name)
    },
    title: title,
    url: "file://" <> path,
    published_at: published_at,
    duration_seconds: None,
    thumbnail_url: None,
  )
}

/// Identity for a media file: the sidecar's id/channel_id when available
fn sidecar_identity(
  media_path: String,
  fallback_id: String,
) -> #(String, Option(String)) {
  let sidecar = drop_extension(media_path) <> ".info.json"
  case simplifile.read(sidecar) {
    Ok(content) ->
      case json.parse(content, sidecar_identity_decoder()) {
        Ok(#(Some(id), channel_id)) -> #(id, channel_id)
        _ -> #(fallback_id, None)
      }
    Error(_) -> #(fallback_id, None)
  }
}

fn sidecar_identity_decoder() -> decode.Decoder(
  #(Option(String), Option(String)),
) {
  use id <- decode.optional_field("id", None, decode.optional(decode.string))
  use channel_id <- decode.optional_field(
    "channel_id",
    None,
    decode.optional(decode.string),
  )
  decode.success(#(id, channel_id))
}

/// Split "s2026.e091101 - Title" into title and upload timestamp
fn split_episode_title(basename: String) -> #(String, Option(Int)) {
  case parse_episode_date(basename) {
    Some(timestamp) -> {
      let title = case list.drop(string.split(basename, " - "), 1) {
        [] -> basename
        rest -> string.join(rest, " - ")
      }
      #(title, Some(timestamp))
    }
    None -> #(basename, None)
  }
}

/// Parse the sYYYY.eMMDDNN episode prefix into a Unix timestamp
pub fn parse_episode_date(basename: String) -> Option(Int) {
  case string.length(basename) >= 13 {
    True ->
      case
        string.slice(basename, 0, 1) == "s"
        && string.slice(basename, 5, 2) == ".e"
      {
        True -> {
          let year = int.parse(string.slice(basename, 1, 4))
          let month = int.parse(string.slice(basename, 7, 2))
          let day = int.parse(string.slice(basename, 9, 2))
          case year, month, day {
            Ok(y), Ok(m), Ok(d) ->
              case m >= 1 && m <= 12 && d >= 1 && d <= 31 {
                True -> Some(days_from_civil(y, m, d) * 86_400)
                False -> None
              }
            _, _, _ -> None
          }
        }
        False -> None
      }
    False -> None
  }
}

/// Days since 1970-01-01 for a proleptic Gregorian date
fn days_from_civil(year: Int, month: Int, day: Int) -> Int {
  let y = case month <= 2 {
    True -> year - 1
    False -> year
  }
  let era = y / 400
  let yoe = y - era * 400
  let mp = case month > 2 {
    True -> month - 3
    False -> month + 9
  }
  let doy = { 153 * mp + 2 } / 5 + day - 1
  let doe = yoe * 365 + yoe / 4 - yoe / 100 + doy
  era * 146_097 + doe - 719_468
}

fn drop_extension(filename: String) -> String {
  case string.contains(filename, ".") {
    False -> filename
    True ->
      case list.reverse(string.split(filename, ".")) {
        [_extension, ..rest] -> rest |> list.reverse |> string.join(".")
        [] -> filename
      }
  }
}

/// List the media files currently in the library
///
/// Walks `<library>/<Show>/<Season>/` so callers can reconcile their records
/// after a poll that ended before the engine printed its "Files created"
/// report. Non-media siblings (thumbnails, `.info.json`, the download
/// archives) are skipped.
pub fn list_library_media(library_dir: String) -> List(String) {
  walk_library(library_dir, 0)
}

/// Nesting limit for the library walk: show / season / file
const max_library_depth = 3

fn walk_library(dir: String, depth: Int) -> List(String) {
  case depth >= max_library_depth {
    True -> []
    False ->
      case simplifile.read_directory(dir) {
        Error(_) -> []
        Ok(entries) ->
          entries
          |> list.flat_map(fn(entry) {
            let path = dir <> "/" <> entry
            case simplifile.is_directory(path) {
              Ok(True) -> walk_library(path, depth + 1)
              _ ->
                case is_media_file(path) {
                  True -> [path]
                  False -> []
                }
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

fn is_error_line(line: String) -> Bool {
  string.contains(line, "ERROR")
  || string.contains(line, "Error:")
  || string.contains(line, "[error]")
}

fn first_error_line(output: String) -> String {
  case list.find(string.split(output, "\n"), is_error_line) {
    Ok(line) -> string.trim(line)
    Error(_) -> "no error detail in output"
  }
}

fn get_env_string(key: String, default: String) -> String {
  case envoy.get(key) {
    Ok(value) -> value
    Error(_) -> default
  }
}

fn file_error(err: simplifile.FileError) -> String {
  string.inspect(err)
}

fn escape_yaml(text: String) -> String {
  text
  |> string.replace("\\", "\\\\")
  |> string.replace("\"", "\\\"")
}
