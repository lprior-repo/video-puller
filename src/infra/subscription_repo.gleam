/// Repository layer for subscription feature
///
/// Data access functions for subscription configuration,
/// seen videos tracking, and channel settings.
/// Uses Cake query builder exclusively.
import cake/select
import cake/update
import cake/where
import domain/subscription_types.{
  type DiscoveredVideo, type SeenVideo, type SubscriptionConfig, SeenVideo,
  SubscriptionConfig,
}
import gleam/dynamic/decode
import gleam/int
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
    |> select.select_cols(["enabled", "poll_interval_minutes", "last_poll_at"])
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

  let base_query =
    update.new()
    |> update.table("subscription_config")
    |> update.set(update.set_int("enabled", enabled_int))
    |> update.set(update.set_int(
      "poll_interval_minutes",
      config.poll_interval_minutes,
    ))
    |> update.set(update.set_int("updated_at", updated_at))
    |> update.where(where.eq(where.col("id"), where.int(1)))

  let query =
    case config.last_poll_at {
      Some(ts) -> update.set(base_query, update.set_int("last_poll_at", ts))
      None -> update.set(base_query, update.set_null("last_poll_at"))
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
  use last_poll_at <- decode.then(decode.at([2], decode.optional(decode.int)))

  decode.success(SubscriptionConfig(
    enabled: enabled == 1,
    poll_interval_minutes: poll_interval,
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
