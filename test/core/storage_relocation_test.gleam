import cake/select
import cake/where
import core/storage_relocation
import gleam/dynamic/decode
import gleam/list
import gleam/result
import gleeunit
import gleeunit/should
import infra/db
import simplifile

pub fn main() {
  gleeunit.main()
}

pub fn relocation_rewrites_only_paths_inside_old_root_test() {
  let fixture = "/tmp/video_puller_storage_relocation"
  let _ = simplifile.delete_all([fixture])
  let old = fixture <> "/old volume/YouTube"
  let new = fixture <> "/new volume/YouTube"
  simplifile.create_directory_all(old) |> should.be_ok
  simplifile.create_directory_all(new) |> should.be_ok
  simplifile.write(old <> "/video_eater.db", "source placeholder")
  |> should.be_ok
  simplifile.create_directory_all(new <> "/ytdl-sub") |> should.be_ok
  simplifile.write(new <> "/ytdl-sub/config.yaml", "stale") |> should.be_ok
  simplifile.write(new <> "/ytdl-sub/subscriptions.yaml.jobs", "stale")
  |> should.be_ok
  simplifile.create_directory_all(new <> "/ytdl-sub/download-history")
  |> should.be_ok
  let archive = new <> "/ytdl-sub/download-history/archive.txt"
  simplifile.write(archive, "youtube completed-video\n") |> should.be_ok
  let path = new <> "/video_eater.db"
  let assert Ok(conn) = db.connect(path)
  create_fixture_schema(conn)
  db.exec_raw(
    conn,
    "INSERT INTO video_jobs (id, path) VALUES ('one', '"
      <> old
      <> "/library/Show/a.mp4'), ('two', '"
      <> old
      <> "-backup/a.mp4'), ('three', '/other/a.mp4')",
  )
  |> should.be_ok
  db.exec_raw(
    conn,
    "INSERT INTO download_config (id, output_directory) VALUES (1, '"
      <> old
      <> "/downloads')",
  )
  |> should.be_ok
  db.exec_raw(
    conn,
    "INSERT INTO subscription_config (id, poll_interval_minutes) VALUES (1, 360)",
  )
  |> should.be_ok
  db.exec_raw(
    conn,
    "INSERT INTO seen_videos (video_id, title) VALUES ('v1234567890', 'Saved title')",
  )
  |> should.be_ok
  db.close(conn) |> should.be_ok

  storage_relocation.relocate(old, new) |> should.be_ok

  let assert Ok(conn) = db.connect(path)
  read_path(conn, "one")
  |> should.equal(Ok(new <> "/library/Show/a.mp4"))
  read_path(conn, "two") |> should.equal(Ok(old <> "-backup/a.mp4"))
  read_path(conn, "three") |> should.equal(Ok("/other/a.mp4"))
  read_output_directory(conn) |> should.equal(Ok(new <> "/downloads"))
  read_integer(conn, "subscription_config", "poll_interval_minutes")
  |> should.equal(Ok(360))
  read_seen_title(conn) |> should.equal(Ok("Saved title"))
  db.close(conn) |> should.be_ok
  simplifile.is_file(new <> "/ytdl-sub/config.yaml")
  |> should.equal(Ok(False))
  simplifile.is_file(new <> "/ytdl-sub/subscriptions.yaml.jobs")
  |> should.equal(Ok(False))
  simplifile.read(archive) |> should.equal(Ok("youtube completed-video\n"))
  storage_relocation.relocate(old, new) |> should.be_ok
  simplifile.read(archive) |> should.equal(Ok("youtube completed-video\n"))
  let assert Ok(conn) = db.connect(path)
  read_seen_title(conn) |> should.equal(Ok("Saved title"))
  db.close(conn) |> should.be_ok
  let _ = simplifile.delete_all([fixture])
}

pub fn relocation_rejects_noncanonical_or_overlapping_roots_test() {
  storage_relocation.validate_root("relative/path")
  |> should.be_error
  storage_relocation.validate_root("/tmp/../outside") |> should.be_error
  storage_relocation.validate_root("/tmp//duplicate") |> should.be_error
  storage_relocation.relocate("/tmp/parent", "/tmp/parent/child")
  |> should.be_error
}

pub fn relocation_refuses_missing_copied_database_test() {
  let fixture = "/tmp/video_puller_storage_relocation_missing"
  let _ = simplifile.delete_all([fixture])
  simplifile.create_directory_all(fixture <> "/old") |> should.be_ok
  simplifile.create_directory_all(fixture <> "/new") |> should.be_ok
  simplifile.write(fixture <> "/old/video_eater.db", "source placeholder")
  |> should.be_ok
  storage_relocation.relocate(fixture <> "/old", fixture <> "/new")
  |> should.be_error
  let _ = simplifile.delete_all([fixture])
}

pub fn relocation_rejects_a_hard_link_alias_before_opening_database_test() {
  let fixture = "/tmp/video_puller_storage_relocation_hardlink"
  let _ = simplifile.delete_all([fixture])
  let old = fixture <> "/old"
  let new = fixture <> "/new"
  simplifile.create_directory_all(old) |> should.be_ok
  simplifile.create_directory_all(new) |> should.be_ok
  let database = new <> "/video_eater.db"
  let assert Ok(conn) = db.connect(database)
  create_fixture_schema(conn)
  db.exec_raw(
    conn,
    "INSERT INTO video_jobs (id, path) VALUES ('kept', '/old/library/x.mp4')",
  )
  |> should.be_ok
  db.close(conn) |> should.be_ok
  simplifile.create_link(database, old <> "/video_eater.db") |> should.be_ok

  storage_relocation.relocate(old, new) |> should.be_error

  let assert Ok(conn) = db.connect(database)
  read_path(conn, "kept") |> should.equal(Ok("/old/library/x.mp4"))
  db.close(conn) |> should.be_ok
  let _ = simplifile.delete_all([fixture])
}

pub fn relocation_rejects_symlink_roots_and_database_paths_test() {
  let fixture = "/tmp/video_puller_storage_relocation_symlink"
  let _ = simplifile.delete_all([fixture])
  let actual_old = fixture <> "/actual-old"
  let old_link = fixture <> "/old-link"
  let new = fixture <> "/new"
  simplifile.create_directory_all(actual_old) |> should.be_ok
  simplifile.create_directory_all(new) |> should.be_ok
  simplifile.write(actual_old <> "/video_eater.db", "placeholder")
  |> should.be_ok
  let db_path = new <> "/video_eater.db"
  let assert Ok(conn) = db.connect(db_path)
  create_fixture_schema(conn)
  db.close(conn) |> should.be_ok
  simplifile.create_symlink(actual_old, old_link) |> should.be_ok
  storage_relocation.relocate(old_link, new) |> should.be_error

  let old = fixture <> "/old"
  simplifile.create_directory_all(old) |> should.be_ok
  simplifile.write(old <> "/video_eater.db", "placeholder") |> should.be_ok
  let external_db = fixture <> "/external.db"
  let assert Ok(external_conn) = db.connect(external_db)
  create_fixture_schema(external_conn)
  db.close(external_conn) |> should.be_ok
  simplifile.delete(db_path) |> should.be_ok
  simplifile.create_symlink(external_db, db_path) |> should.be_ok
  storage_relocation.relocate(old, new) |> should.be_error
  simplifile.is_file(external_db) |> should.equal(Ok(True))
  let _ = simplifile.delete_all([fixture])
}

pub fn relocation_rolls_back_when_destination_schema_is_incomplete_test() {
  let fixture = "/tmp/video_puller_storage_relocation_rollback"
  let _ = simplifile.delete_all([fixture])
  let old = fixture <> "/old"
  let new = fixture <> "/new"
  simplifile.create_directory_all(old) |> should.be_ok
  simplifile.create_directory_all(new) |> should.be_ok
  simplifile.write(old <> "/video_eater.db", "source placeholder")
  |> should.be_ok
  let path = new <> "/video_eater.db"
  let assert Ok(conn) = db.connect(path)
  db.exec_raw(conn, "CREATE TABLE video_jobs (id TEXT PRIMARY KEY, path TEXT)")
  |> should.be_ok
  db.exec_raw(
    conn,
    "INSERT INTO video_jobs (id, path) VALUES ('row', '"
      <> old
      <> "/movie.mp4')",
  )
  |> should.be_ok
  db.close(conn) |> should.be_ok

  storage_relocation.relocate(old, new) |> should.be_error

  let assert Ok(conn) = db.connect(path)
  read_path(conn, "row") |> should.equal(Ok(old <> "/movie.mp4"))
  db.close(conn) |> should.be_ok
  let _ = simplifile.delete_all([fixture])
}

fn create_fixture_schema(conn: db.Db) -> Nil {
  db.exec_raw(conn, "CREATE TABLE video_jobs (id TEXT PRIMARY KEY, path TEXT)")
  |> should.be_ok
  db.exec_raw(
    conn,
    "CREATE TABLE download_config (id INTEGER PRIMARY KEY, output_directory TEXT)",
  )
  |> should.be_ok
  db.exec_raw(
    conn,
    "CREATE TABLE subscription_config (id INTEGER PRIMARY KEY, poll_interval_minutes INTEGER)",
  )
  |> should.be_ok
  db.exec_raw(
    conn,
    "CREATE TABLE seen_videos (video_id TEXT PRIMARY KEY, title TEXT)",
  )
  |> should.be_ok
}

fn read_path(conn: db.Db, id: String) -> Result(String, db.DbError) {
  let query =
    select.new()
    |> select.from_table("video_jobs")
    |> select.select_cols(["path"])
    |> select.where(where.eq(where.col("id"), where.string(id)))
    |> select.to_query()
  db.run_read(conn, query, decode.at([0], decode.string))
  |> result.map(fn(paths) { list.first(paths) |> result.unwrap("") })
}

fn read_output_directory(conn: db.Db) -> Result(String, db.DbError) {
  let query =
    select.new()
    |> select.from_table("download_config")
    |> select.select_cols(["output_directory"])
    |> select.to_query()
  db.run_read(conn, query, decode.at([0], decode.string))
  |> result.map(fn(values) { list.first(values) |> result.unwrap("") })
}

fn read_integer(
  conn: db.Db,
  table: String,
  column: String,
) -> Result(Int, db.DbError) {
  let query =
    select.new()
    |> select.from_table(table)
    |> select.select_cols([column])
    |> select.to_query()
  db.run_read(conn, query, decode.at([0], decode.int))
  |> result.map(fn(values) { list.first(values) |> result.unwrap(0) })
}

fn read_seen_title(conn: db.Db) -> Result(String, db.DbError) {
  let query =
    select.new()
    |> select.from_table("seen_videos")
    |> select.select_cols(["title"])
    |> select.where(where.eq(where.col("video_id"), where.string("v1234567890")))
    |> select.to_query()
  db.run_read(conn, query, decode.at([0], decode.string))
  |> result.map(fn(values) { list.first(values) |> result.unwrap("") })
}
