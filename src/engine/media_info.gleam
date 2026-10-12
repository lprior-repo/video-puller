/// Compact, persistent cache for metadata extracted from yt-dlp sidecars.
///
/// Raw `.info.json` files remain untouched. Cache entries are small JSON files
/// keyed by media path and validated against the adjacent sidecar's size and
/// modification time before being reused.
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/float
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import simplifile

const cache_schema_version = 2

pub type Metadata {
  Metadata(
    id: Option(String),
    channel_id: Option(String),
    title: Option(String),
    channel: Option(String),
    upload_date: Option(String),
    duration: Option(Int),
    thumbnail: Option(String),
    webpage_url: Option(String),
    description: Option(String),
    view_count: Option(Int),
  )
}

type CacheEntry {
  CacheEntry(
    schema_version: Int,
    media_path: String,
    mtime: Int,
    size: Int,
    metadata: Option(Metadata),
  )
}

/// Read one compact cached row, parsing the source sidecar only after a miss or
/// when its mtime/size has changed. `cache_dir` is normally DATA_DIR/cache/video-metadata.
pub fn read(media_path: String, cache_dir: String) -> Option(Metadata) {
  let sidecar_path = info_json_path(media_path)
  case simplifile.file_info(sidecar_path) {
    Error(_) -> None
    Ok(info) -> {
      let cache_path = cache_dir <> "/" <> hash_path(media_path) <> ".json"
      case read_cache(cache_path) {
        Some(entry)
          if entry.schema_version == cache_schema_version
          && entry.media_path == media_path
          && entry.mtime == info.mtime_seconds
          && entry.size == info.size
        ->
          case entry.metadata {
            Some(metadata) -> Some(metadata)
            None ->
              refresh_cache(
                media_path,
                sidecar_path,
                cache_dir,
                cache_path,
                info.mtime_seconds,
                info.size,
              )
          }
        _ -> {
          refresh_cache(
            media_path,
            sidecar_path,
            cache_dir,
            cache_path,
            info.mtime_seconds,
            info.size,
          )
        }
      }
    }
  }
}

fn refresh_cache(
  media_path: String,
  sidecar_path: String,
  cache_dir: String,
  cache_path: String,
  mtime: Int,
  size: Int,
) -> Option(Metadata) {
  let metadata = case simplifile.read(sidecar_path) {
    Ok(content) -> parse(content)
    Error(_) -> None
  }
  case metadata {
    Some(value) -> {
      write_cache(
        cache_dir,
        cache_path,
        CacheEntry(cache_schema_version, media_path, mtime, size, Some(value)),
      )
      Some(value)
    }
    None -> None
  }
}

/// Extract the established yt-dlp info path from a media filename.
pub fn info_json_path(media_path: String) -> String {
  case list_find_extension(media_path) {
    Some(extension) ->
      string.drop_end(media_path, string.length(extension)) <> ".info.json"
    None -> media_path <> ".info.json"
  }
}

fn parse(content: String) -> Option(Metadata) {
  case json.parse(content, metadata_decoder()) {
    Ok(metadata) -> Some(metadata)
    Error(_) -> None
  }
}

fn metadata_decoder() -> decode.Decoder(Metadata) {
  use id <- decode.optional_field("id", None, decode.optional(decode.string))
  use channel_id <- decode.optional_field(
    "channel_id",
    None,
    decode.optional(decode.string),
  )
  use title <- decode.optional_field(
    "title",
    None,
    decode.optional(decode.string),
  )
  use channel <- decode.optional_field(
    "channel",
    None,
    decode.optional(decode.string),
  )
  use upload_date <- decode.optional_field(
    "upload_date",
    None,
    decode.optional(decode.string),
  )
  use duration_value <- decode.optional_field(
    "duration",
    None,
    decode.optional(decode.dynamic),
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
  use description <- decode.optional_field(
    "description",
    None,
    decode.optional(decode.string),
  )
  use view_count <- decode.optional_field(
    "view_count",
    None,
    decode.optional(decode.int),
  )
  decode.success(Metadata(
    id: id,
    channel_id: channel_id,
    title: title,
    channel: channel,
    upload_date: upload_date,
    duration: duration_from_dynamic(duration_value),
    thumbnail: thumbnail,
    webpage_url: webpage_url,
    description: description,
    view_count: view_count,
  ))
}

fn duration_from_dynamic(value: Option(Dynamic)) -> Option(Int) {
  case value {
    Some(dynamic) ->
      case decode.run(dynamic, decode.int) {
        Ok(seconds) -> Some(seconds)
        Error(_) ->
          case decode.run(dynamic, decode.float) {
            Ok(seconds) -> Some(float.round(seconds))
            Error(_) -> None
          }
      }
    None -> None
  }
}

fn read_cache(path: String) -> Option(CacheEntry) {
  case simplifile.read(path) {
    Error(_) -> None
    Ok(content) ->
      case json.parse(content, cache_decoder()) {
        Ok(entry) -> Some(entry)
        Error(_) -> None
      }
  }
}

fn cache_decoder() -> decode.Decoder(CacheEntry) {
  use schema_version <- decode.field("schema_version", decode.int)
  use media_path <- decode.field("media_path", decode.string)
  use mtime <- decode.field("mtime", decode.int)
  use size <- decode.field("size", decode.int)
  use metadata <- decode.field("metadata", decode.optional(metadata_decoder()))
  decode.success(CacheEntry(schema_version, media_path, mtime, size, metadata))
}

fn write_cache(dir: String, path: String, entry: CacheEntry) -> Nil {
  case simplifile.create_directory_all(dir) {
    Error(_) -> Nil
    Ok(_) -> {
      let temporary_path =
        path <> ".tmp." <> int.to_string(unique_integer([Positive]))
      let content = json.to_string(cache_json(entry))
      case simplifile.write(temporary_path, content) {
        Error(_) -> Nil
        Ok(_) -> {
          let _ = simplifile.rename(temporary_path, path)
          Nil
        }
      }
    }
  }
}

fn cache_json(entry: CacheEntry) -> json.Json {
  json.object([
    #("schema_version", json.int(entry.schema_version)),
    #("media_path", json.string(entry.media_path)),
    #("mtime", json.int(entry.mtime)),
    #("size", json.int(entry.size)),
    #("metadata", json.nullable(entry.metadata, metadata_json)),
  ])
}

fn metadata_json(metadata: Metadata) -> json.Json {
  json.object([
    #("id", json.nullable(metadata.id, json.string)),
    #("channel_id", json.nullable(metadata.channel_id, json.string)),
    #("title", json.nullable(metadata.title, json.string)),
    #("channel", json.nullable(metadata.channel, json.string)),
    #("upload_date", json.nullable(metadata.upload_date, json.string)),
    #("duration", json.nullable(metadata.duration, json.int)),
    #("thumbnail", json.nullable(metadata.thumbnail, json.string)),
    #("webpage_url", json.nullable(metadata.webpage_url, json.string)),
    #("description", json.nullable(metadata.description, json.string)),
    #("view_count", json.nullable(metadata.view_count, json.int)),
  ])
}

fn list_find_extension(path: String) -> Option(String) {
  case
    [".mp4", ".mkv", ".webm", ".mov", ".mp3", ".m4a", ".opus", ".flac", ".wav"]
    |> list.find(fn(extension) { string.ends_with(path, extension) })
  {
    Ok(extension) -> Some(extension)
    Error(_) -> None
  }
}

type HashAlgorithm {
  Sha256
}

@external(erlang, "crypto", "hash")
fn hash(algorithm: HashAlgorithm, data: String) -> BitArray

@external(erlang, "binary", "encode_hex")
fn encode_hex(data: BitArray) -> String

fn hash_path(path: String) -> String {
  hash(Sha256, path) |> encode_hex
}

type UniqueOption {
  Positive
}

@external(erlang, "erlang", "unique_integer")
fn unique_integer(options: List(UniqueOption)) -> Int
