/// Google Takeout subscription import
///
/// Merges the channel URLs from a `subscriptions.csv` export (YouTube and
/// YouTube Music -> subscriptions) into `channels.txt`, so the subscription
/// list can be populated without any account access or browser cookies. Drop
/// the CSV at `<DATA_DIR>/ytdl-sub/subscriptions.csv`; once merged it is
/// renamed to `subscriptions.csv.imported`.
import engine/ytdl_sub
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import simplifile

/// Outcome of a Takeout import
pub type ImportSummary {
  ImportSummary(added: Int, found: Int)
}

/// Import the Takeout export when one is waiting beside channels.txt
///
/// Returns None when no export file exists; otherwise merges the channels that
/// are not already listed and renames the export so it is imported once.
pub fn import_if_present(
  layout: ytdl_sub.Layout,
) -> Result(Option(ImportSummary), String) {
  case simplifile.is_file(layout.takeout_file) {
    Ok(False) -> Ok(None)
    Ok(True) -> import_export(layout)
    Error(err) ->
      Error("cannot check " <> layout.takeout_file <> ": " <> file_error(err))
  }
}

/// Import and report the outcome on stdout; never fails the caller
pub fn import_and_log(layout: ytdl_sub.Layout) -> Nil {
  case import_if_present(layout) {
    Ok(Some(ImportSummary(added, found))) ->
      io.println(
        "📥 Takeout import: "
        <> int.to_string(found)
        <> " channel URL(s) found, "
        <> int.to_string(added)
        <> " added to channels.txt",
      )
    Ok(None) -> Nil
    Error(err) -> io.println("⚠ Takeout import failed: " <> err)
  }
}

fn import_export(
  layout: ytdl_sub.Layout,
) -> Result(Option(ImportSummary), String) {
  use content <- result.try(read_file(layout.takeout_file))
  use channels <- result.try(case parse_subscription_channels(content) {
    [] ->
      Error(
        "no channel URLs found in "
        <> layout.takeout_file
        <> " (expected a YouTube Takeout subscriptions.csv)",
      )
    channels -> Ok(channels)
  })
  use existing <- result.try(read_file(layout.channels_file))

  let #(updated, added) = merge_channels(existing, channels)
  use _ <- result.try(case added {
    0 -> Ok(Nil)
    _ -> write_file(layout.channels_file, updated)
  })
  use _ <- result.try(
    simplifile.rename(layout.takeout_file, layout.takeout_file <> ".imported")
    |> result.map_error(fn(err) {
      "cannot rename " <> layout.takeout_file <> ": " <> file_error(err)
    }),
  )

  Ok(Some(ImportSummary(added: added, found: list.length(channels))))
}

/// A parsed Takeout row, retaining the title when it is available.
pub fn parse_subscription_channels(content: String) -> List(ytdl_sub.Channel) {
  content
  |> csv_rows
  |> list.filter_map(fn(row) {
    case parse_channel_row(row) {
      Some(channel) -> Ok(channel)
      None -> Error(Nil)
    }
  })
}

/// Extract channel URLs from a Takeout subscriptions.csv.
///
/// Kept as a compatibility API for callers that only need URLs.
pub fn parse_subscription_urls(content: String) -> List(String) {
  parse_subscription_channels(content)
  |> list.map(fn(channel) { channel.url })
  |> dedupe
}

/// Merge imported channels into the channel list content.
///
/// Existing lines (comments included) are preserved; imported channels that
/// are not already listed are appended as `Title = URL` where a title exists.
pub fn merge_channels(
  existing_content: String,
  imported: List(ytdl_sub.Channel),
) -> #(String, Int) {
  let known =
    existing_content
    |> string.split("\n")
    |> list.filter_map(fn(line) {
      case ytdl_sub.parse_channel_line(line) {
        Some(channel) -> Ok(ytdl_sub.canonical_channel_url(channel.url))
        None -> Error(Nil)
      }
    })
    |> dedupe

  let #(new_channels, _) =
    list.fold(imported, #([], known), fn(acc, channel) {
      let #(new_channels, seen) = acc
      let url = ytdl_sub.canonical_channel_url(channel.url)
      case list.contains(seen, url) {
        True -> acc
        False -> #([ytdl_sub.Channel(channel.label, url), ..new_channels], [
          url,
          ..seen
        ])
      }
    })

  let new_channels = list.reverse(new_channels)
  case new_channels {
    [] -> #(existing_content, 0)
    _ -> {
      let base = case string.is_empty(existing_content) {
        True -> ""
        False ->
          case string.ends_with(existing_content, "\n") {
            True -> existing_content
            False -> existing_content <> "\n"
          }
      }
      let lines =
        list.map(new_channels, fn(channel) {
          case channel.label {
            Some(title) -> title <> " = " <> channel.url
            None -> channel.url
          }
        })
      #(base <> string.join(lines, "\n") <> "\n", list.length(new_channels))
    }
  }
}

type CsvState {
  CsvState(
    rows: List(List(String)),
    row: List(String),
    field: List(String),
    quoted: Bool,
  )
}

fn csv_rows(content: String) -> List(List(String)) {
  let state =
    csv_scan(string.to_graphemes(content), CsvState([], [], [], False))
  let state = finish_csv_row(state)
  list.reverse(state.rows)
}

fn csv_scan(chars: List(String), state: CsvState) -> CsvState {
  case chars {
    [] -> state
    [char, ..rest] ->
      case state.quoted, char, rest {
        True, "\"", ["\"", ..tail] ->
          csv_scan(
            tail,
            CsvState(state.rows, state.row, ["\"", ..state.field], True),
          )
        True, "\"", _ ->
          csv_scan(rest, CsvState(state.rows, state.row, state.field, False))
        False, "\"", _ ->
          csv_scan(rest, CsvState(state.rows, state.row, state.field, True))
        False, ",", _ ->
          csv_scan(
            rest,
            CsvState(
              state.rows,
              [finish_field(state.field), ..state.row],
              [],
              False,
            ),
          )
        False, "\n", _ -> csv_scan(rest, finish_csv_row(state))
        False, "\r", _ -> csv_scan(rest, state)
        _, _, _ ->
          csv_scan(
            rest,
            CsvState(state.rows, state.row, [char, ..state.field], state.quoted),
          )
      }
  }
}

fn finish_field(field: List(String)) -> String {
  field |> list.reverse |> string.join("") |> string.trim
}

fn finish_csv_row(state: CsvState) -> CsvState {
  case state.field, state.row {
    [], [] -> state
    _, _ ->
      CsvState(
        [list.reverse([finish_field(state.field), ..state.row]), ..state.rows],
        [],
        [],
        False,
      )
  }
}

fn parse_channel_row(row: List(String)) -> Option(ytdl_sub.Channel) {
  case find_url(row, 0) {
    Some(#(url, index)) -> {
      let title = case list.drop(row, index + 1) {
        [candidate, ..] ->
          case is_url(candidate), string.is_empty(candidate) {
            True, _ -> None
            False, True -> None
            False, False -> Some(candidate)
          }
        [] -> None
      }
      Some(ytdl_sub.Channel(title, ytdl_sub.canonical_channel_url(url)))
    }
    None -> None
  }
}

fn find_url(cells: List(String), index: Int) -> Option(#(String, Int)) {
  case cells {
    [] -> None
    [cell, ..rest] ->
      case is_channel_url(cell) {
        True -> Some(#(cell, index))
        False -> find_url(rest, index + 1)
      }
  }
}

fn is_url(value: String) -> Bool {
  string.starts_with(value, "http://") || string.starts_with(value, "https://")
}

fn is_channel_url(value: String) -> Bool {
  is_url(value) && string.contains(value, "youtube.com/")
}

fn dedupe(items: List(String)) -> List(String) {
  list.fold(items, [], fn(acc, item) {
    case list.contains(acc, item) {
      True -> acc
      False -> [item, ..acc]
    }
  })
  |> list.reverse
}

fn read_file(path: String) -> Result(String, String) {
  simplifile.read(path)
  |> result.map_error(fn(err) {
    "cannot read " <> path <> ": " <> file_error(err)
  })
}

fn write_file(path: String, content: String) -> Result(Nil, String) {
  simplifile.write(path, content)
  |> result.map_error(fn(err) {
    "cannot write " <> path <> ": " <> file_error(err)
  })
}

fn file_error(err: simplifile.FileError) -> String {
  string.inspect(err)
}
