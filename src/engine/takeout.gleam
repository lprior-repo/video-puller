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
/// Returns None when no export file exists; otherwise merges the URLs that
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
  use urls <- result.try(case parse_subscription_urls(content) {
    [] ->
      Error(
        "no channel URLs found in "
        <> layout.takeout_file
        <> " (expected a YouTube Takeout subscriptions.csv)",
      )
    urls -> Ok(urls)
  })
  use existing <- result.try(read_file(layout.channels_file))

  let #(updated, added) = merge_channels(existing, urls)
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

  Ok(Some(ImportSummary(added: added, found: list.length(urls))))
}

/// Extract the channel URLs from a Takeout subscriptions.csv
///
/// Handles both the current `Channel Id,Channel Url,Channel Title` layout and
/// legacy column orders: any cell containing a URL is taken, so the header row
/// and title columns (which may hold commas inside quotes) are ignored.
pub fn parse_subscription_urls(content: String) -> List(String) {
  content
  |> string.split("\n")
  |> list.filter_map(fn(row) {
    row
    |> string.split(",")
    |> list.find(fn(cell) { is_url(clean_cell(cell)) })
    |> result.map(clean_cell)
  })
  |> dedupe
}

/// Merge imported URLs into the channel list content
///
/// Existing lines (comments included) are preserved; imported URLs that are
/// not already listed are appended. Returns the new content and the number of
/// appended lines.
pub fn merge_channels(
  existing_content: String,
  imported: List(String),
) -> #(String, Int) {
  let known =
    existing_content
    |> string.split("\n")
    |> list.map(string.trim)
    |> list.filter(fn(line) {
      !string.is_empty(line) && !string.starts_with(line, "#")
    })
    |> list.map(channel_key)
    |> dedupe

  let #(new_urls, _) =
    list.fold(imported, #([], known), fn(acc, url) {
      let #(new_urls, seen) = acc
      let key = channel_key(url)
      case list.contains(seen, key) {
        True -> acc
        False -> #([url, ..new_urls], [key, ..seen])
      }
    })

  let new_urls = new_urls |> list.reverse |> dedupe

  case new_urls {
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
      #(base <> string.join(new_urls, "\n") <> "\n", list.length(new_urls))
    }
  }
}

fn clean_cell(cell: String) -> String {
  cell |> string.trim |> string.replace("\"", "") |> string.trim
}

fn is_url(value: String) -> Bool {
  string.starts_with(value, "http://") || string.starts_with(value, "https://")
}

fn channel_key(url: String) -> String {
  let trimmed = string.trim(url)
  case string.ends_with(trimmed, "/") {
    True -> string.drop_end(trimmed, 1)
    False -> trimmed
  }
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
