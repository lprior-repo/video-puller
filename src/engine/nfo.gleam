/// NFO sidecar file generator for Plex/Kodi compatibility
///
/// Generates .nfo XML files from yt-dlp .info.json metadata files.
/// These sidecar files enable Plex and Kodi to properly display
/// video metadata in their libraries.
import gleam/dynamic/decode
import gleam/float
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import simplifile

/// Extended video metadata for NFO generation
/// Contains all fields needed to generate a complete Plex/Kodi .nfo file
pub type NfoMetadata {
  NfoMetadata(
    title: String,
    description: Option(String),
    channel: Option(String),
    uploader: Option(String),
    upload_date: Option(String),
    duration: Option(Int),
    thumbnail: Option(String),
    webpage_url: Option(String),
    view_count: Option(Int),
    like_count: Option(Int),
    categories: List(String),
    tags: List(String),
    video_id: Option(String),
    age_limit: Option(Int),
  )
}

/// Parse .info.json file into NfoMetadata
pub fn parse_info_json(json_string: String) -> Result(NfoMetadata, String) {
  case json.parse(json_string, nfo_metadata_decoder()) {
    Ok(metadata) -> Ok(metadata)
    Error(_) -> Error("Failed to decode info.json")
  }
}

/// Decoder for NfoMetadata from yt-dlp info.json
fn nfo_metadata_decoder() -> decode.Decoder(NfoMetadata) {
  use title <- decode.then(decode.at(["title"], decode.string))
  use description <- decode.optional_field(
    "description",
    None,
    decode.optional(decode.string),
  )
  use channel <- decode.optional_field(
    "channel",
    None,
    decode.optional(decode.string),
  )
  use uploader <- decode.optional_field(
    "uploader",
    None,
    decode.optional(decode.string),
  )
  use upload_date <- decode.optional_field(
    "upload_date",
    None,
    decode.optional(decode.string),
  )
  use duration <- decode.optional_field(
    "duration",
    None,
    decode.optional(decode.int),
  )
  use thumbnail <- decode.optional_field(
    "thumbnail",
    None,
    decode.optional(decode.string),
  )
  use webpage_url <- decode.optional_field(
    "webpage_url",
    None,
    decode.optional(decode.string),
  )
  use view_count <- decode.optional_field(
    "view_count",
    None,
    decode.optional(decode.int),
  )
  use like_count <- decode.optional_field(
    "like_count",
    None,
    decode.optional(decode.int),
  )
  use categories <- decode.optional_field(
    "categories",
    [],
    decode.list(decode.string),
  )
  use tags <- decode.optional_field("tags", [], decode.list(decode.string))
  use video_id <- decode.optional_field(
    "id",
    None,
    decode.optional(decode.string),
  )
  use age_limit <- decode.optional_field(
    "age_limit",
    None,
    decode.optional(decode.int),
  )

  decode.success(NfoMetadata(
    title: title,
    description: description,
    channel: channel,
    uploader: uploader,
    upload_date: upload_date,
    duration: duration,
    thumbnail: thumbnail,
    webpage_url: webpage_url,
    view_count: view_count,
    like_count: like_count,
    categories: categories,
    tags: tags,
    video_id: video_id,
    age_limit: age_limit,
  ))
}

/// Generate Kodi/Plex compatible .nfo XML content
pub fn generate_nfo_xml(metadata: NfoMetadata) -> String {
  let xml_header = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"

  let content =
    string.join(
      [
        "<movie>",
        xml_element("title", metadata.title),
        xml_element("originaltitle", metadata.title),
        option_element("plot", truncate_description(metadata.description)),
        option_element("outline", first_line(metadata.description)),
        xml_element("runtime", format_runtime(metadata.duration)),
        format_year(metadata.upload_date),
        format_dates(metadata.upload_date),
        option_element("studio", get_studio(metadata)),
        format_director(metadata),
        format_categories(metadata.categories),
        format_tags(list.take(metadata.tags, 20)),
        format_unique_id(metadata.video_id),
        option_element("website", metadata.webpage_url),
        format_thumbnail(metadata.thumbnail),
        format_ratings(metadata.like_count, metadata.view_count),
        format_views(metadata.view_count),
        format_age_rating(metadata.age_limit),
        format_actor(metadata),
        "</movie>",
      ],
      "\n",
    )

  xml_header <> content
}

/// Generate .nfo file from .info.json file
pub fn generate_nfo_from_info_json(
  info_json_path: String,
) -> Result(String, String) {
  // Read the info.json file
  use json_content <- result.try(
    simplifile.read(info_json_path)
    |> result.map_error(fn(_) { "Failed to read info.json: " <> info_json_path }),
  )

  // Parse the JSON
  use metadata <- result.try(parse_info_json(json_content))

  // Determine output path (replace .info.json with .nfo)
  let nfo_path = info_json_to_nfo_path(info_json_path)

  // Generate XML content
  let xml_content = generate_nfo_xml(metadata)

  // Write the .nfo file
  case simplifile.write(nfo_path, xml_content) {
    Ok(_) -> Ok(nfo_path)
    Error(_) -> Error("Failed to write .nfo file: " <> nfo_path)
  }
}

/// Convert .info.json path to .nfo path
fn info_json_to_nfo_path(path: String) -> String {
  case string.ends_with(path, ".info.json") {
    True -> string.drop_end(path, 10) <> ".nfo"
    False -> path <> ".nfo"
  }
}

// XML helper functions

fn xml_element(tag: String, content: String) -> String {
  "  <" <> tag <> ">" <> escape_xml(content) <> "</" <> tag <> ">"
}

fn option_element(tag: String, content: Option(String)) -> String {
  case content {
    Some(value) -> xml_element(tag, value)
    None -> ""
  }
}

fn escape_xml(text: String) -> String {
  text
  |> string.replace("&", "&amp;")
  |> string.replace("<", "&lt;")
  |> string.replace(">", "&gt;")
  |> string.replace("\"", "&quot;")
  |> string.replace("'", "&apos;")
}

fn truncate_description(desc: Option(String)) -> Option(String) {
  case desc {
    Some(d) ->
      case string.length(d) > 2000 {
        True -> Some(string.slice(d, 0, 2000) <> "...")
        False -> Some(d)
      }
    None -> None
  }
}

fn first_line(desc: Option(String)) -> Option(String) {
  case desc {
    Some(d) -> {
      let lines = string.split(d, "\n")
      case lines {
        [first, ..] ->
          case string.length(first) > 500 {
            True -> Some(string.slice(first, 0, 500))
            False -> Some(first)
          }
        [] -> None
      }
    }
    None -> None
  }
}

fn format_runtime(duration: Option(Int)) -> String {
  case duration {
    Some(seconds) -> int.to_string(seconds / 60)
    None -> "0"
  }
}

fn format_year(upload_date: Option(String)) -> String {
  case upload_date {
    Some(date) -> {
      case string.length(date) >= 4 {
        True -> xml_element("year", string.slice(date, 0, 4))
        False -> ""
      }
    }
    None -> ""
  }
}

fn format_dates(upload_date: Option(String)) -> String {
  case upload_date {
    Some(date) -> {
      case string.length(date) == 8 {
        True -> {
          let formatted = format_date_string(date)
          xml_element("premiered", formatted)
          <> "\n"
          <> xml_element("aired", formatted)
        }
        False -> ""
      }
    }
    None -> ""
  }
}

fn format_date_string(date: String) -> String {
  // Convert YYYYMMDD to YYYY-MM-DD
  let year = string.slice(date, 0, 4)
  let month = string.slice(date, 4, 2)
  let day = string.slice(date, 6, 2)
  year <> "-" <> month <> "-" <> day
}

fn get_studio(metadata: NfoMetadata) -> Option(String) {
  case metadata.channel {
    Some(c) -> Some(c)
    None -> metadata.uploader
  }
}

fn format_director(metadata: NfoMetadata) -> String {
  let uploader = case metadata.uploader {
    Some(u) -> Some(u)
    None -> metadata.channel
  }
  case uploader {
    Some(u) -> "  <director>" <> escape_xml(u) <> "</director>"
    None -> ""
  }
}

fn format_categories(categories: List(String)) -> String {
  categories
  |> list.map(fn(cat) { xml_element("genre", cat) })
  |> string.join("\n")
}

fn format_tags(tags: List(String)) -> String {
  tags
  |> list.map(fn(tag) { xml_element("tag", tag) })
  |> string.join("\n")
}

fn format_unique_id(video_id: Option(String)) -> String {
  case video_id {
    Some(id) ->
      "  <uniqueid type=\"youtube\" default=\"true\">"
      <> escape_xml(id)
      <> "</uniqueid>"
    None -> ""
  }
}

fn format_thumbnail(thumbnail: Option(String)) -> String {
  case thumbnail {
    Some(url) ->
      "  <thumb aspect=\"poster\">"
      <> escape_xml(url)
      <> "</thumb>\n"
      <> "  <fanart>\n    <thumb>"
      <> escape_xml(url)
      <> "</thumb>\n  </fanart>"
    None -> ""
  }
}

fn format_ratings(like_count: Option(Int), view_count: Option(Int)) -> String {
  let rating_value = case like_count, view_count {
    Some(likes), Some(views) -> {
      case views > 0 {
        True -> {
          // Calculate pseudo-rating based on like ratio
          let ratio = int.to_float(likes) /. int.to_float(views)
          let stars = float_min(5.0, 1.0 +. ratio *. 100.0)
          format_float(stars, 1)
        }
        False -> "0.0"
      }
    }
    _, _ -> "0.0"
  }

  let votes = case like_count {
    Some(l) -> int.to_string(l)
    None -> "0"
  }

  "  <ratings>\n"
  <> "    <rating name=\"youtube\" max=\"5\" default=\"true\">\n"
  <> "      <value>"
  <> rating_value
  <> "</value>\n"
  <> "      <votes>"
  <> votes
  <> "</votes>\n"
  <> "    </rating>\n"
  <> "  </ratings>"
}

fn format_views(view_count: Option(Int)) -> String {
  case view_count {
    Some(v) -> xml_element("views", int.to_string(v))
    None -> ""
  }
}

fn format_age_rating(age_limit: Option(Int)) -> String {
  case age_limit {
    Some(age) -> {
      case age > 0 {
        True -> xml_element("mpaa", "Rated " <> int.to_string(age) <> "+")
        False -> ""
      }
    }
    None -> ""
  }
}

fn format_actor(metadata: NfoMetadata) -> String {
  let channel = case metadata.channel {
    Some(c) -> c
    None ->
      case metadata.uploader {
        Some(u) -> u
        None -> ""
      }
  }

  case channel {
    "" -> ""
    c ->
      "  <actor>\n"
      <> "    <name>"
      <> escape_xml(c)
      <> "</name>\n"
      <> "    <role>Creator</role>\n"
      <> format_actor_thumb(metadata.thumbnail)
      <> "  </actor>"
  }
}

fn format_actor_thumb(thumbnail: Option(String)) -> String {
  case thumbnail {
    Some(url) -> "    <thumb>" <> escape_xml(url) <> "</thumb>\n"
    None -> ""
  }
}

// Float helpers
@external(erlang, "erlang", "min")
fn float_min(a: Float, b: Float) -> Float

/// Format a float with a fixed number of decimals
///
/// `io_lib:format` returns an iolist, which cannot cross the FFI boundary as a
/// `String` and raised `badarg` (killing the downloader actor), so the digits
/// are assembled here instead.
fn format_float(f: Float, decimals: Int) -> String {
  case decimals <= 0 {
    True -> int.to_string(float.round(f))
    False -> {
      let scaled = float.round(f *. pow10(decimals))
      let negative = scaled < 0
      let digits = int.to_string(int.absolute_value(scaled))
      let padded = case string.length(digits) > decimals {
        True -> digits
        False ->
          string.repeat("0", decimals + 1 - string.length(digits)) <> digits
      }
      let whole = string.drop_end(padded, decimals)
      let fraction = string.slice(padded, string.length(whole), decimals)
      let sign = case negative {
        True -> "-"
        False -> ""
      }
      sign <> whole <> "." <> fraction
    }
  }
}

fn pow10(exponent: Int) -> Float {
  case exponent <= 0 {
    True -> 1.0
    False ->
      list.fold(list.repeat(10.0, exponent), 1.0, fn(acc, _) { acc *. 10.0 })
  }
}
