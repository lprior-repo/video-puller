/// Repository layer for subscription feature
///
/// Data access functions for subscription configuration,
/// seen videos tracking, and channel settings.
/// Uses Cake query builder exclusively.
import cake/select
import cake/update
import cake/where
import domain/subscription_types.{
  type ChannelSettings, type DiscoveredVideo, type SeenVideo,
  type SubscriptionConfig, ChannelSettings, SeenVideo, SubscriptionConfig,
}
import gleam/dynamic/decode
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import infra/db.{type Db, type DbError}

/// Get subscription config (singleton row)
pub fn get_config(conn: Db) -> Result(SubscriptionConfig, DbError) {
  let query =
    select.new()
    |> select.from_table("subscription_config")
    |> select.select_cols([
      "enabled", "poll_interval_minutes", "browser", "cookies_path",
      "max_age_days", "min_duration_seconds", "max_duration_seconds",
      "keyword_filter", "keyword_exclude", "last_poll_at",
    ])
    |> select.where(where.eq(where.col("id"), where.int(1)))
    |> select.to_query()

  use rows <- result.try(db.run_read(conn, query, config_decoder()))

  case list.first(rows) {
    Ok(config) -> Ok(config)
    Error(_) -> Ok(subscription_types.default_config())
  }
}

/// Update subscription config
pub fn update_config(
  conn: Db,
  config: SubscriptionConfig,
  updated_at: Int,
) -> Result(Nil, DbError) {
  let enabled_int = case config.enabled {
    True -> 1
    False -> 0
  }

  let keyword_filter_json = encode_string_list(config.keyword_filter)
  let keyword_exclude_json = encode_string_list(config.keyword_exclude)

  let base_query =
    update.new()
    |> update.table("subscription_config")
    |> update.set(update.set_int("enabled", enabled_int))
    |> update.set(update.set_int(
      "poll_interval_minutes",
      config.poll_interval_minutes,
    ))
    |> update.set(update.set_string(
      "browser",
      subscription_types.browser_to_string(config.browser),
    ))
    |> update.set(update.set_int("max_age_days", config.max_age_days))
    |> update.set(update.set_int(
      "min_duration_seconds",
      config.min_duration_seconds,
    ))
    |> update.set(update.set_string("keyword_filter", keyword_filter_json))
    |> update.set(update.set_string("keyword_exclude", keyword_exclude_json))
    |> update.set(update.set_int("updated_at", updated_at))
    |> update.where(where.eq(where.col("id"), where.int(1)))

  let with_cookies = case config.cookies_path {
    Some(path) ->
      update.set(base_query, update.set_string("cookies_path", path))
    None -> update.set(base_query, update.set_null("cookies_path"))
  }

  let with_max_duration = case config.max_duration_seconds {
    Some(d) ->
      update.set(with_cookies, update.set_int("max_duration_seconds", d))
    None -> update.set(with_cookies, update.set_null("max_duration_seconds"))
  }

  let query =
    case config.last_poll_at {
      Some(ts) ->
        update.set(with_max_duration, update.set_int("last_poll_at", ts))
      None -> update.set(with_max_duration, update.set_null("last_poll_at"))
    }
    |> update.to_query()

  db.run_write(conn, query, decode.dynamic)
  |> result.replace(Nil)
}

/// Update last poll timestamp
pub fn update_last_poll(conn: Db, timestamp: Int) -> Result(Nil, DbError) {
  let query =
    update.new()
    |> update.table("subscription_config")
    |> update.set(update.set_int("last_poll_at", timestamp))
    |> update.set(update.set_int("updated_at", timestamp))
    |> update.where(where.eq(where.col("id"), where.int(1)))
    |> update.to_query()

  db.run_write(conn, query, decode.dynamic)
  |> result.replace(Nil)
}

/// Check if a video has been seen before
pub fn is_seen(conn: Db, video_id: String) -> Result(Bool, DbError) {
  let query =
    select.new()
    |> select.from_table("seen_videos")
    |> select.select_cols(["COUNT(*)"])
    |> select.where(where.eq(where.col("video_id"), where.string(video_id)))
    |> select.to_query()

  use rows <- result.try(db.run_read(conn, query, decode.at([0], decode.int)))

  case list.first(rows) {
    Ok(count) -> Ok(count > 0)
    Error(_) -> Ok(False)
  }
}

/// Get a seen video by ID
pub fn get_seen_video(
  conn: Db,
  video_id: String,
) -> Result(Option(SeenVideo), DbError) {
  let query =
    select.new()
    |> select.from_table("seen_videos")
    |> select.select_cols([
      "video_id", "channel_id", "channel_name", "title", "url", "published_at",
      "duration_seconds", "thumbnail_url", "first_seen_at", "downloaded",
      "skipped", "skip_reason", "job_id",
    ])
    |> select.where(where.eq(where.col("video_id"), where.string(video_id)))
    |> select.to_query()

  use rows <- result.try(db.run_read(conn, query, seen_video_decoder()))

  case list.first(rows) {
    Ok(video) -> Ok(Some(video))
    Error(_) -> Ok(None)
  }
}

/// Mark a video as seen (insert or replace)
/// Uses raw SQL for INSERT OR REPLACE as Cake doesn't have direct SQLite upsert support
pub fn mark_seen(
  conn: Db,
  video: DiscoveredVideo,
  downloaded: Bool,
  skip_reason: Option(String),
  job_id: Option(String),
  timestamp: Int,
) -> Result(Nil, DbError) {
  let downloaded_int = case downloaded {
    True -> "1"
    False -> "0"
  }

  let skipped_int = case skip_reason {
    Some(_) -> "1"
    None -> "0"
  }

  let sql =
    "INSERT OR REPLACE INTO seen_videos "
    <> "(video_id, channel_id, channel_name, title, url, published_at, "
    <> "duration_seconds, thumbnail_url, first_seen_at, downloaded, skipped, "
    <> "skip_reason, job_id) VALUES ("
    <> escape_string(video.video_id)
    <> ", "
    <> option_to_sql_string(video.channel_id)
    <> ", "
    <> option_to_sql_string(video.channel_name)
    <> ", "
    <> escape_string(video.title)
    <> ", "
    <> escape_string(video.url)
    <> ", "
    <> option_to_sql_int(video.published_at)
    <> ", "
    <> option_to_sql_int(video.duration_seconds)
    <> ", "
    <> option_to_sql_string(video.thumbnail_url)
    <> ", "
    <> int.to_string(timestamp)
    <> ", "
    <> downloaded_int
    <> ", "
    <> skipped_int
    <> ", "
    <> option_to_sql_string(skip_reason)
    <> ", "
    <> option_to_sql_string(job_id)
    <> ")"

  db.exec_raw(conn, sql)
}

/// Update seen video to mark as downloaded
pub fn mark_downloaded(
  conn: Db,
  video_id: String,
  job_id: String,
) -> Result(Nil, DbError) {
  let query =
    update.new()
    |> update.table("seen_videos")
    |> update.set(update.set_int("downloaded", 1))
    |> update.set(update.set_string("job_id", job_id))
    |> update.where(where.eq(where.col("video_id"), where.string(video_id)))
    |> update.to_query()

  db.run_write(conn, query, decode.dynamic)
  |> result.replace(Nil)
}

/// List recent seen videos
pub fn list_seen_videos(
  conn: Db,
  limit_count: Int,
  offset_count: Int,
) -> Result(List(SeenVideo), DbError) {
  let query =
    select.new()
    |> select.from_table("seen_videos")
    |> select.select_cols([
      "video_id", "channel_id", "channel_name", "title", "url", "published_at",
      "duration_seconds", "thumbnail_url", "first_seen_at", "downloaded",
      "skipped", "skip_reason", "job_id",
    ])
    |> select.order_by_desc("first_seen_at")
    |> select.limit(limit_count)
    |> select.offset(offset_count)
    |> select.to_query()

  db.run_read(conn, query, seen_video_decoder())
}

/// Get channel settings override
pub fn get_channel_settings(
  conn: Db,
  channel_id: String,
) -> Result(Option(ChannelSettings), DbError) {
  let query =
    select.new()
    |> select.from_table("channel_settings")
    |> select.select_cols([
      "channel_id", "channel_name", "enabled", "priority", "max_age_days",
      "min_duration_seconds", "max_duration_seconds", "keyword_filter",
      "keyword_exclude",
    ])
    |> select.where(where.eq(where.col("channel_id"), where.string(channel_id)))
    |> select.to_query()

  use rows <- result.try(db.run_read(conn, query, channel_settings_decoder()))

  case list.first(rows) {
    Ok(settings) -> Ok(Some(settings))
    Error(_) -> Ok(None)
  }
}

/// Save or update channel settings
/// Uses raw SQL for INSERT OR REPLACE as Cake doesn't have direct SQLite upsert support
pub fn upsert_channel_settings(
  conn: Db,
  settings: ChannelSettings,
  timestamp: Int,
) -> Result(Nil, DbError) {
  let enabled_int = case settings.enabled {
    True -> "1"
    False -> "0"
  }

  let sql =
    "INSERT OR REPLACE INTO channel_settings "
    <> "(channel_id, channel_name, enabled, priority, max_age_days, "
    <> "min_duration_seconds, max_duration_seconds, keyword_filter, "
    <> "keyword_exclude, created_at, updated_at) VALUES ("
    <> escape_string(settings.channel_id)
    <> ", "
    <> escape_string(settings.channel_name)
    <> ", "
    <> enabled_int
    <> ", "
    <> int.to_string(settings.priority)
    <> ", "
    <> option_to_sql_int(settings.max_age_days)
    <> ", "
    <> option_to_sql_int(settings.min_duration_seconds)
    <> ", "
    <> option_to_sql_int(settings.max_duration_seconds)
    <> ", "
    <> escape_string(encode_string_list(settings.keyword_filter))
    <> ", "
    <> escape_string(encode_string_list(settings.keyword_exclude))
    <> ", "
    <> int.to_string(timestamp)
    <> ", "
    <> int.to_string(timestamp)
    <> ")"

  db.exec_raw(conn, sql)
}

/// Count total seen videos
pub fn count_seen_videos(conn: Db) -> Result(Int, DbError) {
  let query =
    select.new()
    |> select.from_table("seen_videos")
    |> select.select_cols(["COUNT(*)"])
    |> select.to_query()

  use rows <- result.try(db.run_read(conn, query, decode.at([0], decode.int)))

  case list.first(rows) {
    Ok(count) -> Ok(count)
    Error(_) -> Ok(0)
  }
}

/// Count downloaded videos
pub fn count_downloaded(conn: Db) -> Result(Int, DbError) {
  let query =
    select.new()
    |> select.from_table("seen_videos")
    |> select.select_cols(["COUNT(*)"])
    |> select.where(where.eq(where.col("downloaded"), where.int(1)))
    |> select.to_query()

  use rows <- result.try(db.run_read(conn, query, decode.at([0], decode.int)))

  case list.first(rows) {
    Ok(count) -> Ok(count)
    Error(_) -> Ok(0)
  }
}

// Decoders

fn config_decoder() -> decode.Decoder(SubscriptionConfig) {
  use enabled <- decode.then(decode.at([0], decode.int))
  use poll_interval <- decode.then(decode.at([1], decode.int))
  use browser <- decode.then(decode.at([2], decode.string))
  use cookies_path <- decode.then(decode.at([3], decode.optional(decode.string)))
  use max_age_days <- decode.then(decode.at([4], decode.int))
  use min_duration <- decode.then(decode.at([5], decode.int))
  use max_duration <- decode.then(decode.at([6], decode.optional(decode.int)))
  use keyword_filter <- decode.then(decode.at(
    [7],
    decode.optional(decode.string),
  ))
  use keyword_exclude <- decode.then(decode.at(
    [8],
    decode.optional(decode.string),
  ))
  use last_poll_at <- decode.then(decode.at([9], decode.optional(decode.int)))

  decode.success(SubscriptionConfig(
    enabled: enabled == 1,
    poll_interval_minutes: poll_interval,
    browser: subscription_types.string_to_browser(browser),
    cookies_path: cookies_path,
    max_age_days: max_age_days,
    min_duration_seconds: min_duration,
    max_duration_seconds: max_duration,
    keyword_filter: decode_string_list(keyword_filter),
    keyword_exclude: decode_string_list(keyword_exclude),
    last_poll_at: last_poll_at,
  ))
}

fn seen_video_decoder() -> decode.Decoder(SeenVideo) {
  use video_id <- decode.then(decode.at([0], decode.string))
  use channel_id <- decode.then(decode.at([1], decode.optional(decode.string)))
  use channel_name <- decode.then(decode.at([2], decode.optional(decode.string)))
  use title <- decode.then(decode.at([3], decode.string))
  use url <- decode.then(decode.at([4], decode.string))
  use published_at <- decode.then(decode.at([5], decode.optional(decode.int)))
  use duration <- decode.then(decode.at([6], decode.optional(decode.int)))
  use thumbnail <- decode.then(decode.at([7], decode.optional(decode.string)))
  use first_seen_at <- decode.then(decode.at([8], decode.int))
  use downloaded <- decode.then(decode.at([9], decode.int))
  use skipped <- decode.then(decode.at([10], decode.int))
  use skip_reason <- decode.then(decode.at([11], decode.optional(decode.string)))
  use job_id <- decode.then(decode.at([12], decode.optional(decode.string)))

  decode.success(SeenVideo(
    video_id: video_id,
    channel_id: channel_id,
    channel_name: channel_name,
    title: title,
    url: url,
    published_at: published_at,
    duration_seconds: duration,
    thumbnail_url: thumbnail,
    first_seen_at: first_seen_at,
    downloaded: downloaded == 1,
    skipped: skipped == 1,
    skip_reason: skip_reason,
    job_id: job_id,
  ))
}

fn channel_settings_decoder() -> decode.Decoder(ChannelSettings) {
  use channel_id <- decode.then(decode.at([0], decode.string))
  use channel_name <- decode.then(decode.at([1], decode.string))
  use enabled <- decode.then(decode.at([2], decode.int))
  use priority <- decode.then(decode.at([3], decode.int))
  use max_age_days <- decode.then(decode.at([4], decode.optional(decode.int)))
  use min_duration <- decode.then(decode.at([5], decode.optional(decode.int)))
  use max_duration <- decode.then(decode.at([6], decode.optional(decode.int)))
  use keyword_filter <- decode.then(decode.at(
    [7],
    decode.optional(decode.string),
  ))
  use keyword_exclude <- decode.then(decode.at(
    [8],
    decode.optional(decode.string),
  ))

  decode.success(ChannelSettings(
    channel_id: channel_id,
    channel_name: channel_name,
    enabled: enabled == 1,
    priority: priority,
    max_age_days: max_age_days,
    min_duration_seconds: min_duration,
    max_duration_seconds: max_duration,
    keyword_filter: decode_string_list(keyword_filter),
    keyword_exclude: decode_string_list(keyword_exclude),
  ))
}

// SQL Helpers

fn escape_string(s: String) -> String {
  "'" <> string.replace(s, "'", "''") <> "'"
}

fn option_to_sql_string(opt: Option(String)) -> String {
  case opt {
    Some(s) -> escape_string(s)
    None -> "NULL"
  }
}

fn option_to_sql_int(opt: Option(Int)) -> String {
  case opt {
    Some(n) -> int.to_string(n)
    None -> "NULL"
  }
}

fn encode_string_list(items: List(String)) -> String {
  case items {
    [] -> ""
    _ -> json.to_string(json.array(items, json.string))
  }
}

fn decode_string_list(json_str: Option(String)) -> List(String) {
  case json_str {
    None -> []
    Some("") -> []
    Some(s) -> {
      case string.starts_with(s, "[") && string.ends_with(s, "]") {
        True -> {
          s
          |> string.drop_start(1)
          |> string.drop_end(1)
          |> string.split(",")
          |> list.map(fn(item) {
            item
            |> string.trim
            |> string.replace("\"", "")
          })
          |> list.filter(fn(item) { !string.is_empty(item) })
        }
        False -> []
      }
    }
  }
}
