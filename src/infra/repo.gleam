/// Repository layer for video jobs
///
/// Data access objects (DAOs) for CRUD operations on video_jobs table.
/// All functions use prepared statements to prevent SQL injection.
import domain/types.{type JobId, type VideoJob, type VideoStatus}
import gleam/dynamic/decode
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import infra/db.{type Db, type DbError}
import sqlight

/// Insert a new video job into the database
pub fn insert_job(
  conn: Db,
  job_id: JobId,
  url: String,
  created_at: Int,
) -> Result(Nil, DbError) {
  let sql =
    "
    INSERT INTO video_jobs (id, url, status, progress, path, error_message, title, thumbnail_url, duration_seconds, format_code, created_at, updated_at)
    VALUES (?, ?, 'pending', 0, NULL, NULL, NULL, NULL, NULL, NULL, ?, ?)
  "

  let id_str = types.job_id_to_string(job_id)

  db.query(
    conn,
    sql,
    [
      sqlight.text(id_str),
      sqlight.text(url),
      sqlight.int(created_at),
      sqlight.int(created_at),
    ],
    decode.at([0], decode.int),
  )
  |> result.replace(Nil)
}

/// List all jobs with optional status filter
pub fn list_jobs(
  conn: Db,
  status_filter: Option(String),
) -> Result(List(VideoJob), DbError) {
  let sql = case status_filter {
    None ->
      "SELECT id, url, status, progress, path, error_message, title, thumbnail_url, duration_seconds, format_code, created_at, updated_at
       FROM video_jobs ORDER BY created_at DESC"
    Some(_) ->
      "SELECT id, url, status, progress, path, error_message, title, thumbnail_url, duration_seconds, format_code, created_at, updated_at
       FROM video_jobs WHERE status = ? ORDER BY created_at DESC"
  }

  let args = case status_filter {
    None -> []
    Some(status) -> [sqlight.text(status)]
  }

  db.query(conn, sql, args, video_job_decoder())
}

/// Update job status
pub fn update_status(
  conn: Db,
  job_id: JobId,
  status: VideoStatus,
  updated_at: Int,
) -> Result(Nil, DbError) {
  let status_str = types.status_to_string(status)
  let id_str = types.job_id_to_string(job_id)

  let #(sql, args) = case status {
    types.Downloading(progress) -> #(
      "UPDATE video_jobs SET status = ?, progress = ?, updated_at = ? WHERE id = ?",
      [
        sqlight.text(status_str),
        sqlight.int(progress),
        sqlight.int(updated_at),
        sqlight.text(id_str),
      ],
    )
    types.Completed -> #(
      "UPDATE video_jobs SET status = ?, progress = 100, updated_at = ? WHERE id = ?",
      [sqlight.text(status_str), sqlight.int(updated_at), sqlight.text(id_str)],
    )
    types.Failed(reason) -> #(
      "UPDATE video_jobs SET status = ?, error_message = ?, updated_at = ? WHERE id = ?",
      [
        sqlight.text(status_str),
        sqlight.text(reason),
        sqlight.int(updated_at),
        sqlight.text(id_str),
      ],
    )
    types.Pending -> #(
      "UPDATE video_jobs SET status = ?, progress = 0, updated_at = ? WHERE id = ?",
      [sqlight.text(status_str), sqlight.int(updated_at), sqlight.text(id_str)],
    )
  }

  db.query(conn, sql, args, decode.at([0], decode.int))
  |> result.replace(Nil)
}

/// Update job path when download completes
pub fn update_path(
  conn: Db,
  job_id: JobId,
  path: String,
  updated_at: Int,
) -> Result(Nil, DbError) {
  let sql = "UPDATE video_jobs SET path = ?, updated_at = ? WHERE id = ?"
  let id_str = types.job_id_to_string(job_id)

  db.query(
    conn,
    sql,
    [sqlight.text(path), sqlight.int(updated_at), sqlight.text(id_str)],
    decode.at([0], decode.int),
  )
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
  let sql =
    "UPDATE video_jobs SET title = ?, thumbnail_url = ?, duration_seconds = ?, updated_at = ? WHERE id = ?"
  let id_str = types.job_id_to_string(job_id)

  let title_value = case title {
    Some(t) -> sqlight.text(t)
    None -> sqlight.null()
  }
  let thumbnail_value = case thumbnail_url {
    Some(u) -> sqlight.text(u)
    None -> sqlight.null()
  }
  let duration_value = case duration_seconds {
    Some(d) -> sqlight.int(d)
    None -> sqlight.null()
  }

  db.query(
    conn,
    sql,
    [
      title_value,
      thumbnail_value,
      duration_value,
      sqlight.int(updated_at),
      sqlight.text(id_str),
    ],
    decode.at([0], decode.int),
  )
  |> result.replace(Nil)
}

/// Reset zombie jobs (jobs stuck in 'downloading' state) back to 'pending'
/// This is run on application startup to recover from crashes
pub fn reset_zombies(conn: Db, updated_at: Int) -> Result(Int, DbError) {
  // First count how many zombies we have
  let count_sql = "SELECT COUNT(*) FROM video_jobs WHERE status = 'downloading'"
  use count_rows <- result.try(db.query(
    conn,
    count_sql,
    [],
    decode.at([0], decode.int),
  ))
  let zombie_count = case list.first(count_rows) {
    Ok(n) -> n
    Error(_) -> 0
  }

  // Then update them (using query to support parameters, ignore empty result)
  let update_sql =
    "UPDATE video_jobs SET status = 'pending', progress = 0, updated_at = ?
     WHERE status = 'downloading'"

  use _ <- result.try(db.query(
    conn,
    update_sql,
    [sqlight.int(updated_at)],
    decode.at([0], decode.int),
  ))

  Ok(zombie_count)
}

/// Decoder for VideoJob from database row
fn video_job_decoder() -> decode.Decoder(VideoJob) {
  use id <- decode.then(decode.at([0], decode.string))
  use url <- decode.then(decode.at([1], decode.string))
  use status <- decode.then(decode.at([2], decode.string))
  use progress <- decode.then(decode.at([3], decode.int))
  use path <- decode.then(decode.at([4], decode.optional(decode.string)))
  use _error <- decode.then(decode.at([5], decode.optional(decode.string)))
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

  let video_status = case status, progress {
    "downloading", p -> types.Downloading(p)
    "completed", _ -> types.Completed
    "failed", _ -> types.Failed("Download failed")
    _, _ -> types.Pending
  }

  decode.success(types.VideoJob(
    id: types.new_job_id(id),
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
