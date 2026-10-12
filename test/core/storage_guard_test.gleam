/// Storage identity checks are independent of the free-space reserve so
/// cleanup may recover a verified drive when downloads are paused.
import core/storage_guard
import engine/shell.{ShellResult}
import envoy
import gleam/list
import gleam/option.{type Option, None, Some}
import gleeunit
import gleeunit/should
import simplifile

pub fn main() {
  gleeunit.main()
}

pub fn verified_volume_allows_cleanup_but_low_space_blocks_downloads_test() {
  let root = "/tmp/test_storage_guard_low_space"
  let volume = "/Volumes/Media"
  let uuid = "11111111-2222-3333-4444-555555555555"
  let env = prepare_fixture(root, volume, uuid, uuid, False)

  let cleanup_check = storage_guard.check_volume()
  let download_check = storage_guard.check()

  restore_env(env)
  let _ = simplifile.delete_all([root])

  cleanup_check |> should.equal(Ok(Nil))
  download_check |> should.be_error()
}

pub fn changed_volume_identity_blocks_cleanup_and_downloads_test() {
  let root = "/tmp/test_storage_guard_wrong_uuid"
  let volume = "/Volumes/Media"
  let configured_uuid = "11111111-2222-3333-4444-555555555555"
  let mounted_uuid = "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"
  let env = prepare_fixture(root, volume, configured_uuid, mounted_uuid, True)

  let cleanup_check = storage_guard.check_volume()
  let download_check = storage_guard.check()

  restore_env(env)
  let marker_was_created = simplifile.is_file(root <> "/df-invoked")
  let _ = simplifile.delete_all([root])

  cleanup_check |> should.be_error()
  download_check |> should.be_error()
  marker_was_created |> should.equal(Ok(False))
}

type EnvValue {
  EnvValue(name: String, previous: Option(String))
}

fn prepare_fixture(
  root: String,
  volume: String,
  configured_uuid: String,
  diskutil_uuid: String,
  write_df_marker: Bool,
) -> List(EnvValue) {
  let _ = simplifile.delete_all([root])
  let bin = root <> "/bin"
  let _ = simplifile.create_directory_all(bin)
  let diskutil = bin <> "/diskutil"
  let df = bin <> "/df"
  let marker = root <> "/df-invoked"
  let diskutil_script =
    "#!/bin/sh\ncat <<'PLIST'\n<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<plist><dict><key>MountPoint</key><string>"
    <> volume
    <> "</string><key>VolumeUUID</key><string>"
    <> diskutil_uuid
    <> "</string></dict></plist>\nPLIST\n"
  let df_script = case write_df_marker {
    True -> "#!/bin/sh\nprintf invoked > " <> marker <> "\n"
    False -> "#!/bin/sh\n"
  }
  let df_script =
    df_script
    <> "printf 'Filesystem 1024-blocks Used Available Capacity Mounted on\\n/dev/disk9s1 100000000 50000000 41943040 60%% "
    <> volume
    <> "\\n'\n"
  let _ = simplifile.write(diskutil, diskutil_script)
  let _ = simplifile.write(df, df_script)
  let _ = shell_chmod(diskutil)
  let _ = shell_chmod(df)

  let previous_path = save_env("PATH", bin <> ":" <> env_or_empty("PATH"))
  [
    save_env("STORAGE_VOLUME", volume),
    save_env("STORAGE_VOLUME_UUID", configured_uuid),
    save_env("MIN_FREE_GB", "50"),
    previous_path,
  ]
}

fn shell_chmod(path: String) -> Result(Nil, String) {
  case shell.run("chmod", ["+x", path]) {
    Ok(ShellResult(0, _, _)) -> Ok(Nil)
    _ -> Error("cannot make fake storage command executable")
  }
}

fn env_or_empty(name: String) -> String {
  case envoy.get(name) {
    Ok(value) -> value
    Error(_) -> ""
  }
}

fn save_env(name: String, value: String) -> EnvValue {
  let previous = case envoy.get(name) {
    Ok(value) -> Some(value)
    Error(_) -> None
  }
  let _ = envoy.set(name, value)
  EnvValue(name, previous)
}

fn restore_env(values: List(EnvValue)) -> Nil {
  list.each(values, fn(value) {
    case value.previous {
      Some(previous) -> envoy.set(value.name, previous)
      None -> envoy.unset(value.name)
    }
  })
}
