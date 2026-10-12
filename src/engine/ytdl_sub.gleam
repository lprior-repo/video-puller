/// ytdl-sub engine wrapper
///
/// Wraps the `ytdl-sub` subscription downloader so the application can pull
/// public channel URLs without browser cookies, sequentially, on the
/// application's own polling schedule.
import domain/subscription_types.{type DiscoveredVideo, DiscoveredVideo}
import engine/download_history
import engine/media_info
import engine/shell
import engine/subscription_metadata
import engine/subscription_pool
import engine/subscription_progress
import engine/subscription_settings
import envoy
import gleam/dict.{type Dict}
import gleam/int
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
    recent_subscriptions_file: String,
    channels_file: String,
    channel_ids_file: String,
    takeout_file: String,
    library_dir: String,
    work_dir: String,
    workers: Int,
    download_days: Int,
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
  let settings = subscription_settings.load()
  let ytdl_sub_dir = root <> "/ytdl-sub"
  Layout(
    root: root,
    config_file: ytdl_sub_dir <> "/config.yaml",
    subscriptions_file: ytdl_sub_dir <> "/subscriptions.yaml",
    recent_subscriptions_file: ytdl_sub_dir <> "/subscriptions-recent.yaml",
    channels_file: ytdl_sub_dir <> "/channels.txt",
    channel_ids_file: ytdl_sub_dir <> "/channel_ids.txt",
    takeout_file: ytdl_sub_dir <> "/subscriptions.csv",
    library_dir: root <> "/library",
    work_dir: ytdl_sub_dir <> "/working",
    workers: settings.workers,
    download_days: settings.days,
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
  ensure_plexignore(layout.root)
  use _ <- result.try(ensure_config_file(layout))
  use _ <- result.try(ensure_channels_file(layout))
  Ok(Nil)
}

/// Keep ytdl-sub scratch files out of Plex scans while leaving library/*
/// visible to the library section.
fn ensure_plexignore(data_root: String) -> Nil {
  let path = data_root <> "/.plexignore"
  let rules = ["ytdl-sub/*", "tmp/*", "cache/*", "retired-library/*", "run/*"]
  case simplifile.read(path) {
    Ok(existing) -> {
      let updated =
        list.fold(rules, existing, fn(content, rule) {
          case string.contains(content, rule) {
            True -> content
            False ->
              content
              <> case
                string.is_empty(content) || string.ends_with(content, "\n")
              {
                True -> ""
                False -> "\n"
              }
              <> rule
              <> "\n"
          }
        })
      case updated == existing {
        True -> Nil
        False -> {
          let _ = simplifile.write(path, updated)
          Nil
        }
      }
    }
    Error(_) ->
      case simplifile.is_file(path) {
        Ok(False) -> {
          let _ = simplifile.write(path, string.join(rules, "\n") <> "\n")
          Nil
        }
        _ -> Nil
      }
  }
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
    <> "  ffmpeg_path: \""
    <> escape_yaml(executable_path("ffmpeg"))
    <> "\"\n"
    <> "  ffprobe_path: \""
    <> escape_yaml(executable_path("ffprobe"))
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
  let no_trailing_slash = case string.ends_with(without_fragment, "/") {
    True -> string.drop_end(without_fragment, 1)
    False -> without_fragment
  }
  case youtube_video_id(no_trailing_slash) {
    Some(id) -> "https://www.youtube.com/watch?v=" <> id
    None -> canonical_non_video_url(without_fragment)
  }
}

fn canonical_non_video_url(without_fragment: String) -> String {
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

/// Return the stable YouTube ID from watch, short, live, embed, or youtu.be URLs.
pub fn youtube_video_id(url: String) -> Option(String) {
  let #(without_fragment, _) = split_once(url, "#")
  let #(base, query) = split_once(without_fragment, "?")
  let parts = string.split(base, "/")
  case parts {
    [_, "", "youtu.be", id] -> nonempty_id(id)
    [_, "", host, "watch"] ->
      case is_youtube_host(host) {
        True -> query_video_id(query)
        False -> None
      }
    [_, "", host, kind, id] ->
      case
        is_youtube_host(host)
        && list.contains(["shorts", "live", "embed"], kind)
      {
        True -> nonempty_id(id)
        False -> None
      }
    _ -> None
  }
}

fn nonempty_id(id: String) -> Option(String) {
  case string.is_empty(id) {
    True -> None
    False -> Some(id)
  }
}

fn query_video_id(query: Option(String)) -> Option(String) {
  case query {
    Some(value) ->
      case
        list.find(string.split(value, "&"), fn(part) {
          string.starts_with(part, "v=")
        })
      {
        Ok(part) -> nonempty_id(string.drop_start(part, 2))
        Error(_) -> None
      }
    None -> None
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
  resolve_channel_ids_with_budget(channels, cache_path, 30_000)
}

@external(erlang, "erlang", "monotonic_time")
fn monotonic_time(unit: MonotonicUnit) -> Int

type MonotonicUnit {
  Millisecond
}

fn resolve_channel_ids_with_budget(
  channels: List(Channel),
  cache_path: String,
  budget_ms: Int,
) -> List(Channel) {
  let deadline = monotonic_time(Millisecond) + int.max(budget_ms, 0)
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
          case
            lookup_channel(canonical, deadline - monotonic_time(Millisecond))
          {
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
      let key = case is_channel_url(canonical) {
        False -> "url:" <> canonical
        True ->
          case embedded_channel_id(canonical) {
            Some(id) -> "id:" <> id
            None ->
              case dict.get(ids, canonical) {
                Ok(id) -> "id:" <> id
                Error(_) -> "url:" <> canonical
              }
          }
      }
      add_group(groups, key, canonical, channel.label)
    })
  list.map(groups, fn(group) {
    let derived = subscription_label(group.url)
    let label = choose_label(group.labels, derived, library_dir)
    Channel(Some(label), group.url)
  })
  |> unique_labels
}

/// Return the exact filesystem show names ytdl-sub will manage for these
/// subscriptions, using the ID cache for duplicate channel identities.
pub fn managed_labels(
  channels: List(Channel),
  cache_file: String,
  library_dir: String,
) -> List(String) {
  managed_channels(channels, cache_file, library_dir)
  |> list.map(fn(channel) {
    option.unwrap(channel.label, subscription_label(channel.url))
  })
}

/// Resolve cached identities, deduplicate the full desired set, and return
/// channels labeled with the exact sanitized names used as show directories.
pub fn managed_channels(
  channels: List(Channel),
  cache_file: String,
  library_dir: String,
) -> List(Channel) {
  let channels =
    list.map(channels, fn(channel) {
      Channel(channel.label, canonical_channel_url(channel.url))
    })
  let cache = load_cache(cache_file)
  let channels =
    list.map(channels, fn(channel) {
      apply_cache_title(channel, cache_row(cache, channel.url))
    })
  let ids = channel_ids(channels, cache)
  let channels = dedupe_channels(channels, ids, library_dir)
  list.map(channels, fn(channel) {
    let label = option.unwrap(channel.label, subscription_label(channel.url))
    Channel(Some(sanitize_show_directory(label)), channel.url)
  })
}

/// Newest uploads the recent pass inspects per channel, unless overridden
pub const default_recent_videos = 5

/// Generate the ytdl-sub subscription files for the configured channels.
///
/// Two files are written for every poll: the full list, which the backfill pass
/// works through, and the same list limited to each channel's newest uploads,
/// so a fresh upload is fetched even while a long backlog is still being
/// downloaded.
pub fn write_subscriptions(
  layout: Layout,
  channels: List(Channel),
) -> Result(Nil, String) {
  write_subscriptions_with(layout, channels, recent_video_count())
}

/// Write both subscription files with an explicit recent-pass window
pub fn write_subscriptions_with(
  layout: Layout,
  channels: List(Channel),
  recent_videos: Int,
) -> Result(Nil, String) {
  write_subscription_files(layout, channels, recent_videos, 30_000)
}

/// Limit cold-cache lookups across the whole list, reserving time for downloads.
pub fn write_subscriptions_with_budget(
  layout: Layout,
  channels: List(Channel),
  budget_ms: Int,
) -> Result(Nil, String) {
  write_subscription_files(
    layout,
    channels,
    recent_video_count(),
    int.min(30_000, int.max(budget_ms / 10, 0)),
  )
}

fn write_subscription_files(
  layout: Layout,
  channels: List(Channel),
  recent_videos: Int,
  budget_ms: Int,
) -> Result(Nil, String) {
  let cursor_path = layout.root <> "/ytdl-sub/poll_cursor.txt"
  let offset = case simplifile.read(cursor_path) {
    Ok(raw) -> int.parse(string.trim(raw)) |> result.unwrap(0)
    Error(_) -> 0
  }
  // Rotate lookup work too: an unreachable first channel cannot monopolize
  // the identity-resolution budget on every cold-cache poll.
  let resolved =
    resolve_channel_ids_with_budget(
      rotate_channels(channels, offset),
      layout.channel_ids_file,
      budget_ms,
    )
  // Assign labels in original order before rotating downloads. A collision
  // must never make two channels swap archive names from one poll to another.
  let resolved =
    list.map(channels, fn(channel) {
      let found =
        list.find(resolved, fn(value) {
          value.url == canonical_channel_url(channel.url)
        })
        |> result.unwrap(channel)
      case channel.label {
        Some(label) -> Channel(Some(label), canonical_channel_url(channel.url))
        None -> found
      }
    })
  let ids = channel_ids(resolved, load_cache(layout.channel_ids_file))
  let channels = dedupe_channels(resolved, ids, layout.library_dir)
  let channels = rotate_channels(channels, offset)
  // Explicit requests should start immediately rather than wait behind the
  // channel backlog. Label assignment still happens before reordering.
  let channels =
    list.append(
      list.filter(channels, fn(channel) { is_explicit_video_url(channel.url) }),
      list.filter(channels, fn(channel) { !is_explicit_video_url(channel.url) }),
    )
  let recent_videos = case recent_videos > 0 {
    True -> recent_videos
    False -> default_recent_videos
  }
  use _ <- result.try(
    write_subscription_file(layout.subscriptions_file, layout, channels, [
      #("break_on_existing", "False"),
    ]),
  )
  use _ <- result.try(
    write_subscription_file(layout.recent_subscriptions_file, layout, channels, [
      #("playlist_end", int.to_string(recent_videos)),
    ]),
  )
  use _ <- result.try(write_parallel_files(layout, channels, recent_videos))
  // Advance before execution so a killed or timed-out poll still gives the
  // next channel first chance after restart. Archive names stay unchanged.
  simplifile.write(cursor_path, int.to_string(offset + 1) <> "\n")
  |> result.map_error(fn(err) { "cannot save poll cursor: " <> file_error(err) })
}

/// Prepare independent working directories while preserving shared channel archives.
fn write_parallel_files(
  layout: Layout,
  channels: List(Channel),
  recent_videos: Int,
) -> Result(Nil, String) {
  let cache = load_cache(layout.channel_ids_file)
  let ids = channel_ids(channels, cache)
  use manifests <- result.try(
    list.try_map(channels, fn(channel) {
      let key = channel_key(channel.url)
      let directory = layout.root <> "/ytdl-sub/parallel/" <> key
      use _ <- result.try(create_directory(directory))
      let snapshot =
        layout.root <> "/ytdl-sub/archive-snapshots/" <> key <> ".txt"
      let serial = serialize_before_parallel(channel, ids)
      let child =
        Layout(
          ..layout,
          config_file: directory <> "/config.yaml",
          work_dir: directory <> "/working",
          download_days: case is_explicit_video_url(channel.url) {
            True -> 0
            False -> layout.download_days
          },
        )
      use _ <- result.try(write_config_file(child))
      let full = directory <> "/subscriptions.yaml"
      let recent = directory <> "/subscriptions-recent.yaml"
      let archive_option = [
        #("download_archive", "\"" <> escape_yaml(snapshot) <> "\""),
      ]
      use _ <- result.try(write_subscription_file(
        full,
        child,
        [channel],
        list.append(archive_option, [#("break_on_existing", "False")]),
      ))
      use _ <- result.try(write_subscription_file(
        recent,
        child,
        [channel],
        list.append(archive_option, [
          #("playlist_end", int.to_string(recent_videos)),
        ]),
      ))
      Ok(#(
        job_manifest_line(child.config_file, full, snapshot, serial),
        job_manifest_line(child.config_file, recent, snapshot, serial),
      ))
    }),
  )
  use _ <- result.try(
    simplifile.write(
      layout.subscriptions_file <> ".jobs",
      string.join(list.map(manifests, fn(row) { row.0 }), "\n"),
    )
    |> result.map_error(fn(_) { "Cannot write full-pass channel jobs" }),
  )
  simplifile.write(
    layout.recent_subscriptions_file <> ".jobs",
    string.join(list.map(manifests, fn(row) { row.1 }), "\n"),
  )
  |> result.map_error(fn(_) { "Cannot write recent-pass channel jobs" })
}

fn job_manifest_line(
  config: String,
  subscriptions: String,
  snapshot: String,
  serial: Bool,
) -> String {
  config
  <> "\t"
  <> subscriptions
  <> "\t"
  <> case serial {
    True -> "serial"
    False -> "parallel"
  }
  <> "\t"
  <> snapshot
}

fn serialize_before_parallel(
  channel: Channel,
  ids: Dict(String, String),
) -> Bool {
  is_explicit_video_url(channel.url)
  || is_playlist_url(channel.url)
  || {
    is_channel_url(channel.url)
    && dict.get(ids, canonical_channel_url(channel.url)) == Error(Nil)
  }
}

fn is_playlist_url(url: String) -> Bool {
  let #(base, _) = split_once(canonical_channel_url(url), "?")
  string.ends_with(base, "/playlist")
}

type HashAlgorithm {
  Sha256
}

@external(erlang, "crypto", "hash")
fn hash(algorithm: HashAlgorithm, data: String) -> BitArray

@external(erlang, "binary", "encode_hex")
fn encode_hex(data: BitArray) -> String

fn channel_key(url: String) -> String {
  hash(Sha256, url) |> encode_hex
}

/// Newest-uploads window for the recent pass (`RECENT_VIDEOS`)
fn recent_video_count() -> Int {
  case envoy.get("RECENT_VIDEOS") {
    Ok(raw) ->
      case int.parse(raw) {
        Ok(count) if count > 0 -> count
        _ -> default_recent_videos
      }
    Error(_) -> default_recent_videos
  }
}

fn write_subscription_file(
  path: String,
  layout: Layout,
  channels: List(Channel),
  ytdl_options: List(#(String, String)),
) -> Result(Nil, String) {
  simplifile.write(path, subscriptions_content(layout, channels, ytdl_options))
  |> result.map_error(fn(err) {
    "cannot write " <> path <> ": " <> file_error(err)
  })
}

fn subscriptions_content(
  layout: Layout,
  channels: List(Channel),
  ytdl_options: List(#(String, String)),
) -> String {
  "__preset__:\n"
  <> "  overrides:\n"
  <> "    tv_show_directory: \""
  <> escape_yaml(layout.library_dir)
  <> "\"\n"
  // The resolution assert aborts on any download below 361p, which
  // false-positives on genuinely low-res uploads and skips them forever;
  // throttle protection's request pacing stays enabled.
  <> "    enable_resolution_assert: False\n"
  <> subscription_metadata.overrides_block()
  <> subscription_metadata.preset_block(layout.download_days)
  <> subscription_metadata.restricted_filter_block(
    subscription_settings.load().skip_restricted,
  )
  <> ytdl_options_block(ytdl_options)
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
}

/// Render the ytdl_options plugin block that sits beside `overrides`
///
/// `ytdl_options` is a ytdl-sub plugin, so it is declared as a sibling of the
/// `overrides` block; nesting it under `overrides` fails validation.
fn ytdl_options_block(options: List(#(String, String))) -> String {
  case options {
    [] -> ""
    [_, ..] ->
      "  ytdl_options:\n"
      <> list.fold(options, "", fn(acc, option) {
        let #(key, value) = option
        acc <> "    " <> key <> ": " <> value <> "\n"
      })
  }
}

/// Combine the two passes of one poll into a single outcome
///
/// Files the surviving pass downloaded are kept even when the other pass
/// failed, files are never counted twice, and both failures are reported when
/// neither pass produced anything.
pub fn merge_summaries(
  recent: Result(PullSummary, String),
  backfill: Result(PullSummary, String),
) -> Result(PullSummary, String) {
  let files =
    list.unique(list.append(summary_files(recent), summary_files(backfill)))
  let errors = list.append(summary_errors(recent), summary_errors(backfill))
  case recent, backfill {
    Error(recent_error), Error(backfill_error) ->
      Error(recent_error <> " | " <> backfill_error)
    _, _ -> Ok(PullSummary(list.length(files), files, errors))
  }
}

fn summary_files(outcome: Result(PullSummary, String)) -> List(String) {
  case outcome {
    Ok(summary) -> summary.added_files
    Error(_) -> []
  }
}

fn summary_errors(outcome: Result(PullSummary, String)) -> List(String) {
  case outcome {
    Ok(summary) -> summary.errors
    Error(error) -> [error]
  }
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
      case !is_channel_url(url) || !valid_channel_id(id) {
        True -> None
        False ->
          Some(CacheRow(canonical_channel_url(url), id, nonempty_option(title)))
      }
    [url, id] ->
      case !is_channel_url(url) || !valid_channel_id(id) {
        True -> None
        False -> Some(CacheRow(canonical_channel_url(url), id, None))
      }
    _ -> None
  }
}

fn nonempty_option(value: String) -> Option(String) {
  case
    string.is_empty(value)
    || value == "NA"
    || value == "None"
    || value == "null"
  {
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

fn lookup_channel(
  url: String,
  remaining_ms: Int,
) -> Option(#(String, Option(String))) {
  case remaining_ms <= 0 || !is_channel_url(url) {
    True -> None
    False -> lookup_channel_with_timeout(url, int.min(30_000, remaining_ms))
  }
}

fn lookup_channel_with_timeout(
  url: String,
  timeout_ms: Int,
) -> Option(#(String, Option(String))) {
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
  case shell.run_with_timeout("yt-dlp", args, timeout_ms) {
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
      case valid_channel_id(string.trim(id)) {
        False -> None
        True -> Some(#(string.trim(id), nonempty_option(string.trim(title))))
      }
    [id] ->
      case valid_channel_id(string.trim(id)) {
        False -> None
        True -> Some(#(string.trim(id), None))
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
  subscriptions_file: String,
  timeout_ms: Int,
) -> Result(PullSummary, String) {
  run_pull_with_progress(layout, subscriptions_file, timeout_ms, fn(_) { Nil })
}

/// Record each finished channel before the complete pass returns.
pub fn run_pull_with_progress(
  layout: Layout,
  subscriptions_file: String,
  timeout_ms: Int,
  finished: fn(Result(PullSummary, String)) -> Nil,
) -> Result(PullSummary, String) {
  let phase = phase_name(subscriptions_file)
  let workers = layout.workers
  case simplifile.read(subscriptions_file <> ".jobs") {
    Ok(manifest) -> {
      case parse_job_manifest(manifest) {
        Error(message) -> {
          subscription_progress.prepare_phase(layout.root, phase, [], 0)
          let outcome = Error(message)
          finished(outcome)
          outcome
        }
        Ok(jobs) -> {
          subscription_progress.prepare_phase(
            layout.root,
            phase,
            list.map(jobs, fn(job) {
              #(worker_key(job.0), channel_label(job.1))
            }),
            list.length(jobs),
          )
          let serial_jobs = list.filter(jobs, fn(job) { job.2 })
          let parallel_jobs = list.filter(jobs, fn(job) { !job.2 })
          let outcomes =
            subscription_pool.run_ordered(
              serial_jobs,
              parallel_jobs,
              workers,
              timeout_ms,
              fn(job, budget) {
                run_single(
                  layout.root,
                  phase,
                  worker_key(job.0),
                  channel_label(job.1),
                  list.length(jobs),
                  job.0,
                  job.1,
                  job.3,
                  budget,
                )
              },
              finished,
            )
          subscription_progress.close_phase(
            layout.root,
            phase,
            "Worker stopped before completion (subscription deadline or worker exit)",
          )
          case outcomes {
            [] -> Error("No parallel subscription jobs prepared")
            [first, ..rest] -> list.fold(rest, first, merge_summaries)
          }
        }
      }
    }
    Error(_) -> {
      let error =
        "subscription job manifest missing; refusing untracked download"
      subscription_progress.prepare_phase(layout.root, phase, [], 0)
      let outcome = Error(error)
      finished(outcome)
      outcome
    }
  }
}

/// Parse only the current four-column manifest format. Older manifests do not
/// carry a private archive snapshot and therefore cannot safely start a job.
fn parse_job_manifest(
  manifest: String,
) -> Result(List(#(String, String, Bool, String)), String) {
  let lines =
    manifest
    |> string.split("\n")
    |> trim_manifest_tail
  case lines {
    [] ->
      Error("subscription job manifest is empty; refusing untracked download")
    _ ->
      list.try_map(lines, fn(line) {
        let line = string.trim(line)
        case string.is_empty(line) {
          True ->
            Error(
              "invalid subscription job manifest; refusing untracked download",
            )
          False -> parse_job_manifest_line(line)
        }
      })
  }
}

fn trim_manifest_tail(lines: List(String)) -> List(String) {
  case lines {
    [] -> []
    [line] ->
      case string.is_empty(string.trim(line)) {
        True -> []
        False -> [line]
      }
    [line, ..rest] -> [line, ..trim_manifest_tail(rest)]
  }
}

fn parse_job_manifest_line(
  line: String,
) -> Result(#(String, String, Bool, String), String) {
  case string.split(line, "\t") {
    [config, subscriptions, mode, snapshot] ->
      case
        string.is_empty(config)
        || string.is_empty(subscriptions)
        || string.is_empty(snapshot)
      {
        True ->
          Error(
            "invalid subscription job manifest; refusing untracked download",
          )
        False ->
          case mode {
            "serial" -> Ok(#(config, subscriptions, True, snapshot))
            "parallel" -> Ok(#(config, subscriptions, False, snapshot))
            _ ->
              Error(
                "invalid subscription job manifest; refusing untracked download",
              )
          }
      }
    _ -> Error("invalid subscription job manifest; refusing untracked download")
  }
}

fn executable_path(command: String) -> String {
  case shell.run("which", [command]) {
    Ok(result) if result.exit_code == 0 -> string.trim(result.stdout)
    _ -> "/usr/bin/" <> command
  }
}

fn run_single(
  data_root: String,
  phase: String,
  worker_key: String,
  channel: String,
  phase_total: Int,
  config_file: String,
  subscriptions_file: String,
  snapshot_path: String,
  timeout_ms: Int,
) -> Result(PullSummary, String) {
  subscription_progress.start(
    data_root,
    phase,
    worker_key,
    channel,
    phase_total,
  )
  let archive_ready = case string.is_empty(snapshot_path) {
    True -> Ok(Nil)
    False -> download_history.write_snapshot(data_root, snapshot_path)
  }
  case archive_ready {
    Error(message) -> {
      let error = "cannot prepare idempotency archive: " <> message
      subscription_progress.finish(data_root, phase, worker_key, Error(error))
      Error(error)
    }
    Ok(Nil) ->
      run_engine(
        data_root,
        phase,
        worker_key,
        config_file,
        subscriptions_file,
        timeout_ms,
      )
  }
}

fn run_engine(
  data_root: String,
  phase: String,
  worker_key: String,
  config_file: String,
  subscriptions_file: String,
  timeout_ms: Int,
) -> Result(PullSummary, String) {
  let args = [
    "--suppress-colors",
    "--config",
    config_file,
    "sub",
    subscriptions_file,
  ]
  let outcome = case
    shell.run_with_timeout_and_output(
      engine_command,
      args,
      timeout_ms,
      fn(line) {
        subscription_progress.observe(data_root, phase, worker_key, line)
      },
    )
  {
    Ok(output) -> {
      // Preserve the engine output on the data disk for troubleshooting.
      let directory =
        subscriptions_file
        |> string.split("/")
        |> list.reverse
        |> list.drop(1)
        |> list.reverse
        |> string.join("/")
      let _ = simplifile.write(directory <> "/download.log", output.stdout)
      interpret_run_result(output.exit_code, output.stdout)
    }
    Error(shell.ExecutionError(message)) ->
      Error("execution error: " <> message)
    Error(shell.InvalidCommand(message)) ->
      Error("invalid command: " <> message)
  }
  let outcome = case download_history.refresh(data_root) {
    Ok(_) -> outcome
    Error(message) ->
      case outcome {
        Ok(_) ->
          Error(
            "download history refresh failed after engine completion: "
            <> message,
          )
        Error(engine_error) ->
          Error(
            engine_error
            <> "; download history refresh also failed: "
            <> message,
          )
      }
  }
  subscription_progress.finish(data_root, phase, worker_key, outcome)
  outcome
}

fn phase_name(subscriptions_file: String) -> String {
  case string.contains(subscriptions_file, "recent") {
    True -> "recent"
    False -> "backfill"
  }
}

fn worker_key(config_file: String) -> String {
  case string.split(config_file, "/parallel/") {
    [_, rest, ..] ->
      case string.split(rest, "/") {
        [key, ..] -> key
        [] -> "single"
      }
    _ -> "single"
  }
}

fn channel_label(subscriptions_file: String) -> String {
  case simplifile.read(subscriptions_file) {
    Error(_) -> "Subscription run"
    Ok(content) ->
      case
        list.find(string.split(content, "\n"), fn(line) {
          string.contains(line, "\": \"https://")
        })
      {
        Ok(line) ->
          case string.split(string.trim(line), "\": \"") {
            [label, ..] -> string.drop_start(label, 1)
            _ -> "Subscription run"
          }
        Error(_) -> "Subscription run"
      }
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
  let #(filename_title, filename_published_at) = split_episode_title(basename)
  let metadata = media_info.read(path, metadata_cache_dir(library_dir))
  let title = case metadata {
    Some(value) -> option_unwrap(value.title, filename_title)
    None -> filename_title
  }
  let published_at = case metadata {
    Some(value) ->
      case value.upload_date {
        Some(date) ->
          option_int_unwrap(upload_date_timestamp(date), filename_published_at)
        None -> filename_published_at
      }
    None -> filename_published_at
  }
  let channel_name = case channel {
    "" ->
      case metadata {
        Some(value) -> option_unwrap(value.channel, channel)
        None -> channel
      }
    _ -> channel
  }
  let webpage_url = case metadata {
    Some(value) -> option_unwrap(value.webpage_url, "file://" <> path)
    None -> "file://" <> path
  }
  let duration = case metadata {
    Some(value) -> value.duration
    None -> None
  }
  let thumbnail = case metadata {
    Some(value) -> value.thumbnail
    None -> None
  }
  // Prefer the stable YouTube id from the engine's sidecar: the file name can
  // change (re-titles, aliases), the id cannot, so records stay unique
  let video_id = case metadata {
    Some(value) -> option_unwrap(value.id, relative)
    None -> relative
  }
  let channel_id = case metadata {
    Some(value) -> value.channel_id
    None -> None
  }

  DiscoveredVideo(
    video_id: video_id,
    channel_id: channel_id,
    channel_name: case channel_name {
      "" -> None
      name -> Some(name)
    },
    title: title,
    url: webpage_url,
    published_at: published_at,
    duration_seconds: duration,
    thumbnail_url: thumbnail,
  )
}

fn option_unwrap(value: Option(String), fallback: String) -> String {
  case value {
    Some(nonempty) ->
      case string.is_empty(string.trim(nonempty)) {
        True -> fallback
        False -> string.trim(nonempty)
      }
    _ -> fallback
  }
}

fn option_int_unwrap(value: Option(Int), fallback: Option(Int)) -> Option(Int) {
  case value {
    Some(_) -> value
    None -> fallback
  }
}

fn upload_date_timestamp(date: String) -> Option(Int) {
  case string.length(date) == 8 {
    True ->
      parse_episode_date(
        "s"
        <> string.slice(date, 0, 4)
        <> ".e"
        <> string.slice(date, 4, 4)
        <> "00",
      )
    False -> None
  }
}

/// Identity for a media file: the sidecar's id/channel_id when available
fn metadata_cache_dir(library_dir: String) -> String {
  case string.ends_with(library_dir, "/library") {
    True -> string.drop_end(library_dir, string.length("/library"))
    False -> library_dir
  }
  <> "/cache/video-metadata"
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

/// Return the ytdl-sub info sidecar path for a media file.
pub fn info_json_path(media_path: String) -> String {
  drop_extension(media_path) <> ".info.json"
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

/// Rotate the first channel across polls without altering subscription labels.
pub fn rotate_channels(channels: List(Channel), offset: Int) -> List(Channel) {
  case list.length(channels) {
    0 -> []
    count -> {
      let offset = int.max(offset, 0) % count
      list.append(list.drop(channels, offset), list.take(channels, offset))
    }
  }
}

/// Only channel endpoints share a channel identity. Distinct playlists and
/// individual videos from the same uploader remain distinct subscriptions.
fn is_channel_url(url: String) -> Bool {
  let parts = string.split(canonical_channel_url(url), "/")
  case parts {
    [_, "", host, path] ->
      is_youtube_host(host) && string.starts_with(path, "@")
    [_, "", host, kind, _] ->
      is_youtube_host(host) && list.contains(["channel", "user", "c"], kind)
    _ -> False
  }
}

pub fn is_explicit_video_url(url: String) -> Bool {
  let #(base, query) = split_once(canonical_channel_url(url), "?")
  let parts = string.split(base, "/")
  case parts {
    [_, "", "youtu.be", video_id] -> !string.is_empty(video_id)
    [_, "", host, "watch"] ->
      case is_youtube_host(host) {
        True -> query_has_video_id(query)
        False -> False
      }
    [_, "", host, kind, video_id] ->
      is_youtube_host(host)
      && list.contains(["shorts", "live", "embed"], kind)
      && !string.is_empty(video_id)
    _ -> False
  }
}

fn query_has_video_id(query: Option(String)) -> Bool {
  case query {
    Some(value) ->
      list.any(string.split(value, "&"), fn(part) {
        string.starts_with(part, "v=") && string.length(part) > 2
      })
    None -> False
  }
}

fn is_youtube_host(host: String) -> Bool {
  list.contains(["youtube.com", "www.youtube.com", "m.youtube.com"], host)
}

/// Match yt-dlp's default filename sanitization for subscription directory
/// labels. The installed ytdl-sub preset uses tv_show_name_sanitized, backed
/// by yt-dlp sanitize_filename(restricted=False).
fn sanitize_show_directory(label: String) -> String {
  let label =
    label
    |> string.replace("/", "⧸")
    |> string.replace("\\", "⧹")
    |> string.replace("\"", "＂")
    |> string.replace("*", "＊")
    |> string.replace(":", "：")
    |> string.replace("<", "＜")
    |> string.replace(">", "＞")
    |> string.replace("?", "？")
    |> string.replace("|", "｜")
    |> string.replace("\n", " ")
  let label = drop_leading_dots(label)
  case string.starts_with(label, "-") {
    True -> "_" <> string.drop_start(label, 1)
    False ->
      case string.is_empty(label) {
        True -> "_"
        False -> label
      }
  }
}

fn drop_leading_dots(value: String) -> String {
  case string.starts_with(value, ".") {
    True -> drop_leading_dots(string.drop_start(value, 1))
    False -> value
  }
}

fn valid_channel_id(id: String) -> Bool {
  string.starts_with(id, "UC")
  && string.length(id) > 2
  && !string.contains(id, " ")
  && !string.contains(id, "\t")
}

/// Duplicate YAML keys silently drop subscriptions. Preserve the first label
/// and give later collisions a stable URL-derived suffix, also checking the
/// suffix against explicitly named channels.
fn unique_labels(channels: List(Channel)) -> List(Channel) {
  let reserved =
    list.map(channels, fn(channel) {
      option.unwrap(channel.label, subscription_label(channel.url))
      |> sanitize_show_directory
    })
  let #(reversed, _) =
    list.fold(channels, #([], []), fn(acc, channel) {
      let #(out, used) = acc
      let label = option.unwrap(channel.label, subscription_label(channel.url))
      let label = case list.contains(used, sanitize_show_directory(label)) {
        False -> label
        True -> {
          let suffix =
            subscription_label(channel.url)
            |> string.replace("/", "_")
            |> string.replace(":", "_")
            |> string.replace("?", "_")
            |> string.replace("\\", "_")
          available_label(label <> " (" <> suffix <> ")", reserved, used, 1)
        }
      }
      #([Channel(Some(label), channel.url), ..out], [
        sanitize_show_directory(label),
        ..used
      ])
    })
  list.reverse(reversed)
}

fn available_label(
  base: String,
  reserved: List(String),
  used: List(String),
  number: Int,
) -> String {
  let candidate = case number {
    1 -> base
    _ -> base <> " " <> int.to_string(number)
  }
  let safe_candidate = sanitize_show_directory(candidate)
  case
    list.contains(reserved, safe_candidate)
    || list.contains(used, safe_candidate)
  {
    True -> available_label(base, reserved, used, number + 1)
    False -> candidate
  }
}
