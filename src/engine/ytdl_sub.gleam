/// ytdl-sub engine wrapper
///
/// Wraps the `ytdl-sub` subscription downloader so the application can pull
/// public channel URLs without browser cookies, sequentially, on the
/// application's own polling schedule.
import domain/subscription_types.{type DiscoveredVideo, DiscoveredVideo}
import engine/shell
import envoy
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import simplifile

/// Paths used by the ytdl-sub integration
pub type Layout {
  Layout(
    root: String,
    config_file: String,
    subscriptions_file: String,
    channels_file: String,
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
pub fn layout(root: String) -> Layout {
  let ytdl_sub_dir = root <> "/ytdl-sub"
  Layout(
    root: root,
    config_file: ytdl_sub_dir <> "/config.yaml",
    subscriptions_file: ytdl_sub_dir <> "/subscriptions.yaml",
    channels_file: ytdl_sub_dir <> "/channels.txt",
    library_dir: root <> "/library",
    work_dir: ytdl_sub_dir <> "/working",
  )
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

/// Read and normalize the channel list (one URL per line, # comments allowed)
pub fn read_channels(path: String) -> Result(List(String), String) {
  use content <- result.try(
    simplifile.read(path)
    |> result.map_error(fn(err) {
      "cannot read " <> path <> ": " <> file_error(err)
    }),
  )

  Ok(
    content
    |> string.split("\n")
    |> list.map(string.trim)
    |> list.filter(fn(line) {
      !string.is_empty(line) && !string.starts_with(line, "#")
    }),
  )
}

/// Generate the ytdl-sub subscriptions file for the configured channels
pub fn write_subscriptions(
  layout: Layout,
  channels: List(String),
) -> Result(Nil, String) {
  let content =
    "__preset__:\n"
    <> "  overrides:\n"
    <> "    tv_show_directory: \""
    <> escape_yaml(layout.library_dir)
    <> "\"\n"
    <> "    only_recent_date_range: \"7days\"\n"
    <> "\n"
    <> "Plex TV Show by Date | Only Recent Archive:\n"
    <> "  = YouTube:\n"
    <> list.fold(channels, "", fn(acc, url) {
      acc
      <> "    \""
      <> escape_yaml(subscription_label(url))
      <> "\": \""
      <> escape_yaml(url)
      <> "\"\n"
    })

  simplifile.write(layout.subscriptions_file, content)
  |> result.map_error(fn(err) {
    "cannot write " <> layout.subscriptions_file <> ": " <> file_error(err)
  })
}

/// Label used as the subscription name for a channel URL
pub fn subscription_label(url: String) -> String {
  let without_query = case string.split(url, "?") {
    [base, ..] -> base
    [] -> url
  }

  let trimmed = case string.ends_with(without_query, "/") {
    True -> string.drop_end(without_query, 1)
    False -> without_query
  }

  let segments =
    string.split(trimmed, "/")
    |> list.filter(fn(segment) { !string.is_empty(segment) })
    |> list.reverse

  let segment = case segments {
    [last, previous, ..] ->
      case list.contains(tab_segments, last) {
        True -> previous
        False -> last
      }
    [last, ..] -> last
    [] -> trimmed
  }

  case string.starts_with(segment, "@") {
    True -> string.drop_start(segment, 1)
    False -> segment
  }
}

const tab_segments = ["videos", "streams", "shorts", "playlists", "featured"]

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
    Ok(result) ->
      case result.exit_code {
        0 -> Ok(parse_pull_output(result.stdout))
        _ ->
          Error(
            "ytdl-sub failed with exit code "
            <> int.to_string(result.exit_code)
            <> ": "
            <> first_error_line(result.stdout),
          )
      }
    Error(shell.ExecutionError(message)) ->
      Error("execution error: " <> message)
    Error(shell.InvalidCommand(message)) ->
      Error("invalid command: " <> message)
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

  DiscoveredVideo(
    video_id: relative,
    channel_id: None,
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
