/// Parse authoritative YouTube channel lists and stage them as desired state.
import engine/ytdl_sub
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import simplifile

pub type ImportSummary {
  ImportSummary(added: Int, removed: List(ytdl_sub.Channel), found: Int)
}

/// Read an authoritative channel list from `path` and persist its desired state.
pub fn reconcile_file(
  path: String,
  channels_file: String,
) -> Result(ImportSummary, String) {
  use content <- result.try(
    simplifile.read(path)
    |> result.map_error(fn(_) { "cannot read import file " <> path }),
  )
  reconcile_content(content, channels_file)
}

/// Compatibility alias for callers that still call this an import.
pub fn import_file(
  path: String,
  channels_file: String,
) -> Result(ImportSummary, String) {
  reconcile_file(path, channels_file)
}

/// Parse pasted content, preserve labels for retained channels, and atomically
/// persist a desired channel snapshot beside the active channels.txt. The
/// active file is left for `sync_desired` so the coordinator can retire removed
/// channels before publishing the new set.
pub fn reconcile_content(
  content: String,
  channels_file: String,
) -> Result(ImportSummary, String) {
  let requested = parse_channels(content)
  case requested {
    [] -> Error("no YouTube channels found in the supplied text")
    _ -> {
      use active <- result.try(read_channel_file(channels_file))
      let existing = parse_channels(active)
      let desired = preserve_existing_labels(requested, existing)
      let #(added, removed) = differences(existing, desired)
      use _ <- result.try(write_if_changed(
        desired_path(channels_file),
        render_channels(desired),
      ))
      Ok(ImportSummary(
        added: list.length(added),
        removed: removed,
        found: list.length(desired),
      ))
    }
  }
}

/// Apply the persisted desired list. A missing desired file is a no-op, which
/// keeps existing installations from losing their seeded channel list.
pub fn sync_desired(channels_file: String) -> Result(ImportSummary, String) {
  let path = desired_path(channels_file)
  case simplifile.is_file(path) {
    Ok(False) -> Ok(ImportSummary(added: 0, removed: [], found: 0))
    Error(_) -> Error("cannot check desired channel file " <> path)
    Ok(True) -> {
      use content <- result.try(
        simplifile.read(path)
        |> result.map_error(fn(_) {
          "cannot read desired channel file " <> path
        }),
      )
      use requested <- result.try(parse_persisted_channels(content))
      case requested {
        [] ->
          Error(
            "desired channel file contains no channels; active list unchanged",
          )
        _ -> {
          use active <- result.try(read_channel_file(channels_file))
          use existing <- result.try(parse_persisted_channels(active))
          let desired = preserve_existing_labels(requested, existing)
          let #(added, removed) = differences(existing, desired)
          use _ <- result.try(write_if_changed(
            channels_file,
            render_channels(desired),
          ))
          use _ <- result.try(write_if_changed(path, render_channels(desired)))
          Ok(ImportSummary(
            added: list.length(added),
            removed: removed,
            found: list.length(desired),
          ))
        }
      }
    }
  }
}

/// Report whether a valid, nonempty desired snapshot differs from the active
/// channel file. Missing desired state is normal for older installations.
pub fn has_pending_changes(channels_file: String) -> Result(Bool, String) {
  let path = desired_path(channels_file)
  case simplifile.is_file(path) {
    Ok(False) -> Ok(False)
    Error(_) -> Error("cannot check desired channel file " <> path)
    Ok(True) -> {
      use desired <- result.try(
        simplifile.read(path)
        |> result.map_error(fn(_) {
          "cannot read desired channel file " <> path
        }),
      )
      use channels <- result.try(parse_persisted_channels(desired))
      case channels {
        [] -> Error("desired channel file contains no channels")
        _ -> {
          use active <- result.try(read_channel_file(channels_file))
          use _ <- result.try(parse_persisted_channels(active))
          Ok(active != desired)
        }
      }
    }
  }
}

/// Add one channel to desired state without replacing the rest of the list.
/// If no desired snapshot exists yet, the current active list is the base.
pub fn add_desired_channel(
  channel: ytdl_sub.Channel,
  channels_file: String,
) -> Result(ImportSummary, String) {
  add_desired_channels([channel], channels_file)
}

/// Add channels in one atomic desired-state update; used by other additive
/// import sources such as Google Takeout.
pub fn add_desired_channels(
  channels: List(ytdl_sub.Channel),
  channels_file: String,
) -> Result(ImportSummary, String) {
  let path = desired_path(channels_file)
  use base <- result.try(read_first_existing(path, channels_file))
  use existing <- result.try(parse_persisted_channels(base))
  let requested =
    list.fold(channels, existing, fn(requested, channel) {
      let normalized =
        ytdl_sub.Channel(
          channel.label,
          ytdl_sub.canonical_channel_url(channel.url),
        )
      let already_present =
        list.any(requested, fn(old) {
          identity_key(old.url) == identity_key(normalized.url)
        })
      case already_present {
        True -> requested
        False -> list.append(requested, [normalized])
      }
    })
  let desired = preserve_existing_labels(requested, existing)
  let #(added, removed) = differences(existing, desired)
  use _ <- result.try(write_if_changed(path, render_channels(desired)))
  Ok(ImportSummary(
    added: list.length(added),
    removed: removed,
    found: list.length(desired),
  ))
}

/// Compatibility alias; imports are authoritative desired-state updates.
pub fn import_content(
  content: String,
  channels_file: String,
) -> Result(ImportSummary, String) {
  reconcile_content(content, channels_file)
}

/// Parse either `Label = URL` / URL lines or copied YouTube channel entries.
pub fn parse_channels(content: String) -> List(ytdl_sub.Channel) {
  let lines = string.split(content, "\n")
  let #(channels, _) =
    list.fold(lines, #([], None), fn(acc, raw_line) {
      let #(channels, previous_name) = acc
      let line = string.trim(raw_line)
      case parse_supported_channel_line(line) {
        Some(channel) -> #([channel, ..channels], None)
        None ->
          case handle_from_line(line) {
            Some(handle) -> {
              let label = case previous_name {
                Some(name) -> Some(name)
                None -> None
              }
              let channel =
                ytdl_sub.Channel(
                  label,
                  ytdl_sub.canonical_channel_url(
                    "https://www.youtube.com/" <> handle,
                  ),
                )
              #([channel, ..channels], None)
            }
            None ->
              case string.is_empty(line) || is_subscription_marker(line) {
                True -> #(channels, previous_name)
                False -> #(channels, Some(line))
              }
          }
      }
    })
  dedupe_channels(list.reverse(channels))
}

fn differences(
  existing: List(ytdl_sub.Channel),
  desired: List(ytdl_sub.Channel),
) -> #(List(ytdl_sub.Channel), List(ytdl_sub.Channel)) {
  let desired_keys =
    list.map(desired, fn(channel) { identity_key(channel.url) })
  let existing_keys =
    list.map(existing, fn(channel) { identity_key(channel.url) })
  let added =
    list.filter(desired, fn(channel) {
      !list.contains(existing_keys, identity_key(channel.url))
    })
  let removed =
    list.filter(existing, fn(channel) {
      !list.contains(desired_keys, identity_key(channel.url))
    })
  #(dedupe_channels(added), dedupe_channels(removed))
}

fn preserve_existing_labels(
  requested: List(ytdl_sub.Channel),
  existing: List(ytdl_sub.Channel),
) -> List(ytdl_sub.Channel) {
  requested
  |> list.map(fn(channel) {
    let normalized = ytdl_sub.canonical_channel_url(channel.url)
    case
      list.find(existing, fn(old) {
        identity_key(old.url) == identity_key(normalized)
      })
    {
      Ok(old) ->
        ytdl_sub.Channel(old.label, ytdl_sub.canonical_channel_url(old.url))
      Error(_) -> ytdl_sub.Channel(channel.label, normalized)
    }
  })
  |> dedupe_channels
}

fn render_channels(channels: List(ytdl_sub.Channel)) -> String {
  channels
  |> list.map(fn(channel) {
    case channel.label {
      Some(label) -> label <> " = " <> channel.url
      None -> channel.url
    }
  })
  |> string.join("\n")
  |> fn(body) {
    case string.is_empty(body) {
      True -> ""
      False -> body <> "\n"
    }
  }
}

fn handle_from_line(line: String) -> Option(String) {
  let #(head, _) = case string.split(line, "•") {
    [head, ..rest] -> #(head, rest)
    _ -> #(line, [])
  }
  let head = string.trim(head)
  case string.starts_with(head, "@") && string.length(head) > 1 {
    True -> Some(head)
    False -> None
  }
}

fn identity_key(url: String) -> String {
  let canonical = ytdl_sub.canonical_channel_url(url)
  case string.contains(canonical, "/@") {
    True -> string.lowercase(canonical)
    False -> canonical
  }
}

fn dedupe_channels(channels: List(ytdl_sub.Channel)) -> List(ytdl_sub.Channel) {
  let #(out, _) =
    list.fold(channels, #([], []), fn(acc, channel) {
      let #(out, seen) = acc
      let key = identity_key(channel.url)
      case list.contains(seen, key) {
        True -> acc
        False -> #([channel, ..out], [key, ..seen])
      }
    })
  list.reverse(out)
}

fn is_subscription_marker(line: String) -> Bool {
  string.lowercase(string.trim(line)) == "subscribed"
}

fn read_channel_file(path: String) -> Result(String, String) {
  case simplifile.is_file(path) {
    Ok(False) -> Ok("")
    Ok(True) ->
      simplifile.read(path)
      |> result.map_error(fn(_) { "cannot read channels file " <> path })
    Error(_) -> Error("cannot check channels file " <> path)
  }
}

fn read_first_existing(
  preferred_path: String,
  fallback_path: String,
) -> Result(String, String) {
  case simplifile.is_file(preferred_path) {
    Ok(True) ->
      simplifile.read(preferred_path)
      |> result.map_error(fn(_) {
        "cannot read desired channel file " <> preferred_path
      })
    Ok(False) -> read_channel_file(fallback_path)
    Error(_) -> Error("cannot check desired channel file " <> preferred_path)
  }
}

/// Parse the normalized on-disk format used by active and desired channel
/// files. Unlike pasted YouTube exports, persisted files must not silently
/// discard malformed rows because they define an authoritative set.
fn parse_persisted_channels(
  content: String,
) -> Result(List(ytdl_sub.Channel), String) {
  content
  |> string.split("\n")
  |> list.fold(Ok([]), fn(acc, raw_line) {
    use channels <- result.try(acc)
    let line = string.trim(raw_line)
    case string.is_empty(line) || string.starts_with(line, "#") {
      True -> Ok(channels)
      False ->
        case parse_supported_channel_line(line) {
          Some(channel) -> Ok([channel, ..channels])
          None -> Error("invalid channel list line: " <> line)
        }
    }
  })
  |> result.map(list.reverse)
}

fn parse_supported_channel_line(line: String) -> Option(ytdl_sub.Channel) {
  case ytdl_sub.parse_channel_line(line) {
    Some(channel) ->
      case is_supported_youtube_url(channel.url) {
        True -> Some(channel)
        False -> None
      }
    None -> None
  }
}

fn is_supported_youtube_url(url: String) -> Bool {
  let parts = string.split(url, "://")
  case parts {
    [scheme, remainder] -> {
      let remainder_parts = string.split(remainder, "/")
      case remainder_parts {
        [host, path, ..] ->
          list.contains(["http", "https"], string.lowercase(scheme))
          && list.contains(
            [
              "youtube.com", "www.youtube.com", "m.youtube.com",
              "music.youtube.com", "youtu.be",
            ],
            string.lowercase(host),
          )
          && !string.is_empty(path)
          && !list.any([" ", "\t", "\r"], fn(space) {
            string.contains(url, space)
          })
        _ -> False
      }
    }
    _ -> False
  }
}

fn desired_path(channels_file: String) -> String {
  case string.ends_with(channels_file, "channels.txt") {
    True ->
      string.drop_end(channels_file, string.length("channels.txt"))
      <> "desired-channels.txt"
    False -> channels_file <> ".desired-channels.txt"
  }
}

fn write_if_changed(path: String, content: String) -> Result(Nil, String) {
  use existing <- result.try(read_channel_file(path))
  case existing == content {
    True -> Ok(Nil)
    False -> write_atomic(path, content)
  }
}

fn write_atomic(path: String, content: String) -> Result(Nil, String) {
  let temporary = path <> ".import.tmp"
  use _ <- result.try(
    simplifile.write(temporary, content)
    |> result.map_error(fn(_) {
      "cannot write temporary channels file " <> temporary
    }),
  )
  simplifile.rename(temporary, path)
  |> result.map_error(fn(_) {
    let _ = simplifile.delete(temporary)
    "cannot atomically replace channels file " <> path
  })
}
