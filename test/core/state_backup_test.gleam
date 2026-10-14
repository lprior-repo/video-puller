import core/state_backup
import gleam/dynamic/decode
import gleam/list
import gleeunit
import gleeunit/should
import infra/db
import simplifile
import sqlight

pub fn main() {
  gleeunit.main()
}

pub fn vacuum_into_captures_committed_wal_row_and_subscription_files_test() {
  let data_root = temporary_root() <> "/state_backup_wal_data"
  let backup_root = temporary_root() <> "/state_backup_wal_backups"
  let database_path = data_root <> "/app.sqlite"
  clean(data_root, backup_root)
  simplifile.create_directory_all(data_root) |> should.be_ok()
  simplifile.create_directory_all(data_root <> "/ytdl-sub/download-history")
  |> should.be_ok()
  simplifile.write(
    data_root <> "/ytdl-sub/download-history/archive.txt",
    "youtube abcdefghijk\n",
  )
  |> should.be_ok()
  simplifile.write(data_root <> "/ytdl-sub/channels.txt", "@channel\n")
  |> should.be_ok()
  simplifile.write(data_root <> "/ytdl-sub/desired-channels.txt", "@desired\n")
  |> should.be_ok()
  simplifile.write(data_root <> "/ytdl-sub/channel_ids.txt", "@channel\tUC1\n")
  |> should.be_ok()

  let connection = db.init_db(database_path) |> should.be_ok()
  db.exec_raw(connection, "CREATE TABLE backup_probe (value TEXT NOT NULL)")
  |> should.be_ok()
  db.exec_raw(connection, "INSERT INTO backup_probe VALUES ('still in wal')")
  |> should.be_ok()
  simplifile.is_file(database_path <> "-wal") |> should.equal(Ok(True))

  let snapshot =
    state_backup.run_once(connection, data_root, backup_root, 1_800_000_000)
    |> should.be_ok()
  let snapshot_db = db.connect(snapshot <> "/database.sqlite") |> should.be_ok()
  sqlight.query(
    "SELECT value FROM backup_probe",
    snapshot_db,
    [],
    decode.at([0], decode.string),
  )
  |> should.equal(Ok(["still in wal"]))
  simplifile.read(snapshot <> "/archive.txt")
  |> should.equal(Ok("youtube abcdefghijk\n"))
  simplifile.read(snapshot <> "/channels.txt") |> should.equal(Ok("@channel\n"))
  simplifile.read(snapshot <> "/desired-channels.txt")
  |> should.equal(Ok("@desired\n"))
  simplifile.read(snapshot <> "/channel_ids.txt")
  |> should.equal(Ok("@channel\tUC1\n"))

  db.close(snapshot_db) |> should.be_ok()
  db.close(connection) |> should.be_ok()
  clean(data_root, backup_root)
}

pub fn backup_rejects_destination_inside_data_root_test() {
  let data_root = temporary_root() <> "/state_backup_inside_data"
  clean(data_root, data_root <> "/backups")
  simplifile.create_directory_all(data_root) |> should.be_ok()
  let connection = db.init_db(data_root <> "/app.sqlite") |> should.be_ok()
  state_backup.run_once(
    connection,
    data_root,
    data_root <> "/backups",
    1_800_000_000,
  )
  |> should.be_error()
  simplifile.is_directory(data_root <> "/backups") |> should.equal(Ok(False))
  db.close(connection) |> should.be_ok()
  clean(data_root, data_root <> "/backups")
}

pub fn backup_retention_keeps_only_three_completed_daily_snapshots_test() {
  let data_root = temporary_root() <> "/state_backup_retention_data"
  let backup_root = temporary_root() <> "/state_backup_retention_backups"
  clean(data_root, backup_root)
  simplifile.create_directory_all(data_root) |> should.be_ok()
  let connection = db.init_db(data_root <> "/app.sqlite") |> should.be_ok()
  db.exec_raw(connection, "CREATE TABLE backup_probe (value TEXT)")
  |> should.be_ok()

  let first = 1_800_000_000
  let _ =
    state_backup.run_once(connection, data_root, backup_root, first)
    |> should.be_ok()
  let _ =
    state_backup.run_once(connection, data_root, backup_root, first + 86_400)
    |> should.be_ok()
  let _ =
    state_backup.run_once(
      connection,
      data_root,
      backup_root,
      first + 2 * 86_400,
    )
    |> should.be_ok()
  let _ =
    state_backup.run_once(
      connection,
      data_root,
      backup_root,
      first + 3 * 86_400,
    )
    |> should.be_ok()
  let assert Ok(names) = simplifile.read_directory(backup_root)
  names |> list.length |> should.equal(3)

  db.close(connection) |> should.be_ok()
  clean(data_root, backup_root)
}

pub fn malformed_history_cannot_publish_a_completed_backup_test() {
  let data_root = temporary_root() <> "/state_backup_bad_history_data"
  let backup_root = temporary_root() <> "/state_backup_bad_history_backups"
  clean(data_root, backup_root)
  simplifile.create_directory_all(data_root <> "/ytdl-sub/download-history")
  |> should.be_ok()
  simplifile.write(
    data_root <> "/ytdl-sub/download-history/archive.txt",
    "broken entry\n",
  )
  |> should.be_ok()
  let conn = db.init_db(data_root <> "/app.sqlite") |> should.be_ok()
  db.exec_raw(conn, "CREATE TABLE probe (value TEXT)") |> should.be_ok()
  state_backup.run_once(conn, data_root, backup_root, 1_800_000_000)
  |> should.be_error()
  simplifile.read_directory(backup_root) |> should.equal(Ok([]))
  db.close(conn) |> should.be_ok()
  clean(data_root, backup_root)
}

pub fn retention_preserves_an_old_snapshot_with_unfamiliar_files_test() {
  let data_root = temporary_root() <> "/state_backup_unknown_data"
  let backup_root = temporary_root() <> "/state_backup_unknown_backups"
  clean(data_root, backup_root)
  simplifile.create_directory_all(data_root) |> should.be_ok()
  let conn = db.init_db(data_root <> "/app.sqlite") |> should.be_ok()
  db.exec_raw(conn, "CREATE TABLE probe (value TEXT)") |> should.be_ok()
  let now = 1_800_000_000
  let oldest =
    state_backup.run_once(conn, data_root, backup_root, now) |> should.be_ok()
  simplifile.write(oldest <> "/keep-personal.txt", "keep") |> should.be_ok()
  state_backup.run_once(conn, data_root, backup_root, now + 86_400)
  |> should.be_ok()
  state_backup.run_once(conn, data_root, backup_root, now + 2 * 86_400)
  |> should.be_ok()
  state_backup.run_once(conn, data_root, backup_root, now + 3 * 86_400)
  |> should.be_error()
  simplifile.read(oldest <> "/keep-personal.txt") |> should.equal(Ok("keep"))
  simplifile.is_file(oldest <> "/database.sqlite") |> should.equal(Ok(True))
  db.close(conn) |> should.be_ok()
  clean(data_root, backup_root)
}

fn clean(data_root: String, backup_root: String) -> Nil {
  let _ = simplifile.delete_all([data_root, backup_root])
  Nil
}

fn temporary_root() -> String {
  case simplifile.is_directory("/private/tmp") {
    Ok(True) -> "/private/tmp"
    _ -> "/tmp"
  }
}
