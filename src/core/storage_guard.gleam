/// Stop the application before an external data disk can fill or disappear.
import engine/shell
import envoy
import gleam/erlang/process
import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/string

pub fn check() -> Result(Nil, String) {
  use _ <- result.try(check_volume())
  case envoy.get("STORAGE_VOLUME") {
    Error(_) | Ok("") -> Ok(Nil)
    Ok(volume) -> {
      use output <- result.try(command("df", ["-k", volume]))
      let row =
        output
        |> string.trim
        |> string.split("\n")
        |> list.reverse
        |> list.first
        |> result.unwrap("")
      let columns =
        row |> string.split(" ") |> list.filter(fn(value) { value != "" })
      let reserve =
        envoy.get("MIN_FREE_GB")
        |> result.unwrap("50")
        |> int.parse
        |> result.unwrap(50)
      case columns {
        [_, _, _, available, ..] -> {
          let free = int.parse(available) |> result.unwrap(0)
          case free >= reserve * 1024 * 1024 {
            True -> Ok(Nil)
            False ->
              Error(
                "External drive is below the free-space reserve; downloads stopped",
              )
          }
        }
        _ -> Error("Cannot check external-drive free space; downloads stopped")
      }
    }
  }
}

/// Verify identity before cleanup even when the free-space reserve is low.
/// Downloads still require the full `check`; deletion can recover space on
/// the verified external disk before a new downloader is allowed to start.
pub fn check_volume() -> Result(Nil, String) {
  case envoy.get("STORAGE_VOLUME") {
    Error(_) | Ok("") -> Ok(Nil)
    Ok(volume) -> {
      let uuid = envoy.get("STORAGE_VOLUME_UUID") |> result.unwrap("")
      use info <- result.try(command("diskutil", ["info", "-plist", volume]))
      case
        plist_value(info, "MountPoint") == volume
        && plist_value(info, "VolumeUUID") == uuid
        && uuid != ""
      {
        True -> Ok(Nil)
        False ->
          Error(
            "The configured external drive is missing or has changed; downloads stopped",
          )
      }
    }
  }
}

/// Runs as a linked BEAM process; launchd retries after storage is available.
pub fn start() -> Nil {
  let _ = process.spawn(loop)
  Nil
}

fn loop() -> Nil {
  process.sleep(5000)
  case check() {
    Ok(_) -> loop()
    Error(error) -> {
      io.println("STORAGE GUARD: " <> error)
      stop_download_processes()
      halt(75)
    }
  }
}

fn command(name: String, args: List(String)) -> Result(String, String) {
  case shell.run_with_timeout(name, args, 5000) {
    Ok(output) if output.exit_code == 0 -> Ok(output.stdout)
    _ -> Error("Cannot verify external storage; downloads stopped")
  }
}

fn plist_value(content: String, key: String) -> String {
  case string.split(content, "<key>" <> key <> "</key>") {
    [_, value, ..] -> {
      case string.split(value, "<string>") {
        [_, value, ..] ->
          string.split(value, "</string>") |> list.first |> result.unwrap("")
        _ -> ""
      }
    }
    _ -> ""
  }
}

@external(erlang, "shell_ffi", "stop_download_processes")
fn stop_download_processes() -> Nil

@external(erlang, "erlang", "halt")
fn halt(code: Int) -> Nil
