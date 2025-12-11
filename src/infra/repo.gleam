/// Repository layer for video jobs
///
/// Data access objects (DAOs) for CRUD operations on video_jobs table.
/// All queries use Cake query builder for type-safe SQL generation.
import cake/insert
import cake/select
import cake/update
import cake/where
import domain/core_types
import domain/types.{
  type JobId, type VideoJob, type VideoStatus, job_id_to_string, new_job_id,
  new_random_job_id, status_to_string,
}
import engine/ytdlp
import gleam/dynamic/decode
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import infra/db.{type Db, type DbError}

/// Insert a new video job into the database
pub fn insert_job(
  conn: Db,
  job_id: JobId,
  url: String,
  created_at: Int,
) -> Result(Nil, DbError) {
  let id_str = job_id_to_string(job_id)

  let query =
    insert.from_values(
      table_name: "video_jobs",
      columns: [
        "id", "url", "status", "progress", "path", "error_message", "title",
        "thumbnail_url", "duration_seconds", "format_code", "created_at",
        "updated_at",
      ],
      values: [
        insert.row([
          insert.string(id_str),
          insert.string(url),
          insert.string("pending"),
          insert.int(0),
          insert.null(),
          insert.null(),
          insert.null(),
          insert.null(),
          insert.null(),
          insert.null(),
          insert.int(created_at),
          insert.int(created_at),
        ]),
      ],
    )
    |> insert.to_query()

  db.run_write(conn, query, decode.dynamic)
  |> result.replace(Nil)
}

/// Batch insert multiple video jobs efficiently
/// Returns the count of successfully inserted jobs
pub fn insert_jobs_batch(
  conn: Db,
  urls: List(String),
  created_at: Int,
) -> Result(Int, DbError) {
  // Process in batches of 100 to avoid SQLite limits
  let batch_size = 100
  let batches = list.sized_chunk(urls, batch_size)

  list.fold(batches, Ok(0), fn(acc_result, batch) {
    use acc <- result.try(acc_result)
    use count <- result.try(insert_batch_internal(conn, batch, created_at))
    Ok(acc + count)
  })
}

/// Internal function to insert a single batch of jobs
fn insert_batch_internal(
  conn: Db,
  urls: List(String),
  created_at: Int,
) -> Result(Int, DbError) {
  case urls {
    [] -> Ok(0)
    _ -> {
      let rows =
        list.map(urls, fn(url) {
          let job_id = new_random_job_id()
          let id_str = job_id_to_string(job_id)
          insert.row([
            insert.string(id_str),
            insert.string(url),
            insert.string("pending"),
            insert.int(0),
            insert.null(),
            insert.null(),
            insert.null(),
            insert.null(),
            insert.null(),
            insert.null(),
            insert.int(created_at),
            insert.int(created_at),
          ])
        })

      let query =
        insert.from_values(
          table_name: "video_jobs",
          columns: [
            "id", "url", "status", "progress", "path", "error_message", "title",
            "thumbnail_url", "duration_seconds", "format_code", "created_at",
            "updated_at",
          ],
          values: rows,
        )
        |> insert.to_query()

      db.run_write(conn, query, decode.dynamic)
      |> result.map(fn(_) { list.length(urls) })
    }
  }
}

/// List all jobs with optional status filter
pub fn list_jobs(
  conn: Db,
  status_filter: Option(String),
) -> Result(List(VideoJob), DbError) {
  let base_query =
    select.new()
    |> select.from_table("video_jobs")
    |> select.select_cols([
      "id", "url", "status", "progress", "path", "error_message", "title",
      "thumbnail_url", "duration_seconds", "format_code", "created_at",
      "updated_at",
    ])
    |> select.order_by_desc("created_at")

  let query =
    case status_filter {
      None -> base_query
      Some(status) ->
        base_query
        |> select.where(where.eq(where.col("status"), where.string(status)))
    }
    |> select.to_query()

  db.run_read(conn, query, video_job_decoder())
}

/// Update job status
pub fn update_status(
  conn: Db,
  job_id: JobId,
  status: VideoStatus,
  updated_at: Int,
) -> Result(Nil, DbError) {
  let status_str = status_to_string(status)
  let id_str = job_id_to_string(job_id)

  let base_update =
    update.new()
    |> update.table("video_jobs")
    |> update.set(update.set_string("status", status_str))
    |> update.set(update.set_int("updated_at", updated_at))
    |> update.where(where.eq(where.col("id"), where.string(id_str)))

  let query =
    case status {
      core_types.Downloading(progress) ->
        base_update
        |> update.set(update.set_int("progress", progress))
      core_types.Completed ->
        base_update
        |> update.set(update.set_int("progress", 100))
      core_types.Failed(reason) ->
        base_update
        |> update.set(update.set_string("error_message", reason))
      core_types.Pending ->
        base_update
        |> update.set(update.set_int("progress", 0))
    }
    |> update.to_query()

  db.run_write(conn, query, decode.dynamic)
  |> result.replace(Nil)
}

/// Update job path when download completes
pub fn update_path(
  conn: Db,
  job_id: JobId,
  path: String,
  updated_at: Int,
) -> Result(Nil, DbError) {
  let id_str = job_id_to_string(job_id)

  let query =
    update.new()
    |> update.table("video_jobs")
    |> update.set(update.set_string("path", path))
    |> update.set(update.set_int("updated_at", updated_at))
    |> update.where(where.eq(where.col("id"), where.string(id_str)))
    |> update.to_query()

  db.run_write(conn, query, decode.dynamic)
  |> result.replace(Nil)
}

/// Update job metadata (title, thumbnail, duration)
pub fn update_metadata(
  conn: Db,
  job_id: JobId,
  title: Option(String),
  thumbnail_url: Option(String),
  duration_seconds: Option(Int),
  updated_at: Int,
) -> Result(Nil, DbError) {
  let id_str = job_id_to_string(job_id)

  let base_update =
    update.new()
    |> update.table("video_jobs")
    |> update.set(update.set_int("updated_at", updated_at))
    |> update.where(where.eq(where.col("id"), where.string(id_str)))

  let with_title = case title {
    Some(t) -> update.set(base_update, update.set_string("title", t))
    None -> update.set(base_update, update.set_null("title"))
  }

  let with_thumbnail = case thumbnail_url {
    Some(u) -> update.set(with_title, update.set_string("thumbnail_url", u))
    None -> update.set(with_title, update.set_null("thumbnail_url"))
  }

  let query =
    case duration_seconds {
      Some(d) ->
        update.set(with_thumbnail, update.set_int("duration_seconds", d))
      None -> update.set(with_thumbnail, update.set_null("duration_seconds"))
    }
    |> update.to_query()

  db.run_write(conn, query, decode.dynamic)
  |> result.replace(Nil)
}

/// Reset zombie jobs (jobs stuck in 'downloading' state) back to 'pending'
/// This is run on application startup to recover from crashes
pub fn reset_zombies(conn: Db, updated_at: Int) -> Result(Int, DbError) {
  // First count how many zombies we have
  let count_query =
    select.new()
    |> select.from_table("video_jobs")
    |> select.select_cols(["COUNT(*)"])
    |> select.where(where.eq(where.col("status"), where.string("downloading")))
    |> select.to_query()

  use count_rows <- result.try(db.run_read(
    conn,
    count_query,
    decode.at([0], decode.int),
  ))

  let zombie_count = case list.first(count_rows) {
    Ok(n) -> n
    Error(_) -> 0
  }

  // Then update them
  let update_query =
    update.new()
    |> update.table("video_jobs")
    |> update.set(update.set_string("status", "pending"))
    |> update.set(update.set_int("progress", 0))
    |> update.set(update.set_int("updated_at", updated_at))
    |> update.where(where.eq(where.col("status"), where.string("downloading")))
    |> update.to_query()

  use _ <- result.try(db.run_write(conn, update_query, decode.dynamic))

  Ok(zombie_count)
}

/// Get a single job by ID
pub fn get_job(conn: Db, job_id: JobId) -> Result(Option(VideoJob), DbError) {
  let id_str = job_id_to_string(job_id)

  let query =
    select.new()
    |> select.from_table("video_jobs")
    |> select.select_cols([
      "id", "url", "status", "progress", "path", "error_message", "title",
      "thumbnail_url", "duration_seconds", "format_code", "created_at",
      "updated_at",
    ])
    |> select.where(where.eq(where.col("id"), where.string(id_str)))
    |> select.to_query()

  use rows <- result.try(db.run_read(conn, query, video_job_decoder()))

  Ok(case list.first(rows) {
    Ok(job) -> option.Some(job)
    Error(_) -> option.None
  })
}

/// Decoder for VideoJob from database row
fn video_job_decoder() -> decode.Decoder(VideoJob) {
  use id <- decode.then(decode.at([0], decode.string))
  use url <- decode.then(decode.at([1], decode.string))
  use status <- decode.then(decode.at([2], decode.string))
  use progress <- decode.then(decode.at([3], decode.int))
  use path <- decode.then(decode.at([4], decode.optional(decode.string)))
  use error_message <- decode.then(decode.at(
    [5],
    decode.optional(decode.string),
  ))
  use title <- decode.then(decode.at([6], decode.optional(decode.string)))
  use thumbnail_url <- decode.then(decode.at(
    [7],
    decode.optional(decode.string),
  ))
  use duration_seconds <- decode.then(decode.at(
    [8],
    decode.optional(decode.int),
  ))
  use format_code <- decode.then(decode.at([9], decode.optional(decode.string)))
  use created_at <- decode.then(decode.at([10], decode.int))
  use updated_at <- decode.then(decode.at([11], decode.int))

  let video_status = case status, error_message {
    "downloading", _ -> core_types.Downloading(progress)
    "completed", _ -> core_types.Completed
    "failed", Some(msg) -> core_types.Failed(msg)
    "failed", None -> core_types.Failed("Unknown error")
    _, _ -> core_types.Pending
  }

  decode.success(core_types.VideoJob(
    id: new_job_id(id),
    url: url,
    status: video_status,
    path: path,
    title: title,
    thumbnail_url: thumbnail_url,
    duration_seconds: duration_seconds,
    format_code: format_code,
    created_at: created_at,
    updated_at: updated_at,
  ))
}

// ============================================================================
// Download Configuration (singleton)
// ============================================================================

/// Get download configuration from database (singleton row)
/// Falls back to default config if table doesn't exist or is empty
pub fn get_download_config(conn: Db) -> Result(ytdlp.DownloadConfig, DbError) {
  let query =
    select.new()
    |> select.from_table("download_config")
    |> select.select_cols([
      "output_directory", "format", "max_filesize", "audio_only", "audio_format",
      "allow_playlist", "download_timeout_minutes", "rate_limit_delay_ms",
      "bandwidth_limit", "use_channel_folders", "max_concurrency",
    ])
    |> select.where(where.eq(where.col("id"), where.int(1)))
    |> select.to_query()

  case db.run_read(conn, query, download_config_decoder()) {
    Ok(rows) ->
      case list.first(rows) {
        Ok(config) -> Ok(config)
        Error(_) -> Ok(ytdlp.default_config())
      }
    // Table might not exist yet (pre-migration), return defaults
    Error(_) -> Ok(ytdlp.default_config())
  }
}

/// Update download configuration in database
pub fn update_download_config(
  conn: Db,
  config: ytdlp.DownloadConfig,
  updated_at: Int,
) -> Result(Nil, DbError) {
  let audio_only_int = case config.audio_only {
    True -> 1
    False -> 0
  }

  let allow_playlist_int = case config.allow_playlist {
    True -> 1
    False -> 0
  }

  let use_channel_folders_int = case config.use_channel_folders {
    True -> 1
    False -> 0
  }

  let query =
    update.new()
    |> update.table("download_config")
    |> update.set(update.set_string("output_directory", config.output_directory))
    |> update.set(update.set_string("format", config.format))
    |> update.set(update.set_string("max_filesize", config.max_filesize))
    |> update.set(update.set_int("audio_only", audio_only_int))
    |> update.set(update.set_string(
      "audio_format",
      ytdlp.audio_format_to_string(config.audio_format),
    ))
    |> update.set(update.set_int("allow_playlist", allow_playlist_int))
    |> update.set(update.set_int(
      "download_timeout_minutes",
      config.download_timeout_ms / 60_000,
    ))
    |> update.set(update.set_int(
      "rate_limit_delay_ms",
      config.rate_limit_delay_ms,
    ))
    |> update.set(update.set_string("bandwidth_limit", config.bandwidth_limit))
    |> update.set(update.set_int("use_channel_folders", use_channel_folders_int))
    |> update.set(update.set_int("updated_at", updated_at))
    |> update.where(where.eq(where.col("id"), where.int(1)))
    |> update.to_query()

  db.run_write(conn, query, decode.dynamic)
  |> result.replace(Nil)
}

/// Decoder for DownloadConfig from database row
fn download_config_decoder() -> decode.Decoder(ytdlp.DownloadConfig) {
  use output_directory <- decode.then(decode.at([0], decode.string))
  use format <- decode.then(decode.at([1], decode.string))
  use max_filesize <- decode.then(decode.at([2], decode.string))
  use audio_only <- decode.then(decode.at([3], decode.int))
  use audio_format <- decode.then(decode.at([4], decode.string))
  use allow_playlist <- decode.then(decode.at([5], decode.int))
  use download_timeout_minutes <- decode.then(decode.at([6], decode.int))
  use rate_limit_delay_ms <- decode.then(decode.at([7], decode.int))
  use bandwidth_limit <- decode.then(decode.at([8], decode.string))
  use use_channel_folders <- decode.then(decode.at([9], decode.int))
  // max_concurrency is at index 10 but not stored in DownloadConfig

  decode.success(ytdlp.DownloadConfig(
    output_directory: output_directory,
    format: format,
    max_filesize: max_filesize,
    audio_only: audio_only == 1,
    audio_format: ytdlp.string_to_audio_format(audio_format),
    allow_playlist: allow_playlist == 1,
    download_timeout_ms: download_timeout_minutes * 60_000,
    rate_limit_delay_ms: rate_limit_delay_ms,
    bandwidth_limit: bandwidth_limit,
    use_channel_folders: use_channel_folders == 1,
  ))
}
