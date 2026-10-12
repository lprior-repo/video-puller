/// Stage an authoritative subscription export through the same Gleam logic
/// used by the web UI. The running coordinator applies it within 30 seconds.
import engine/channel_import
import engine/ytdl_sub
import envoy
import gleam/int
import gleam/io
import gleam/list

pub fn main() -> Nil {
  case envoy.get("CHANNELS_IMPORT_FILE") {
    Error(_) -> {
      io.println("Set CHANNELS_IMPORT_FILE to the exported channel list")
      halt(1)
    }
    Ok(path) -> {
      let layout = ytdl_sub.layout_from_env()
      case channel_import.reconcile_file(path, layout.channels_file) {
        Error(message) -> {
          io.println("Channel import failed: " <> message)
          halt(1)
        }
        Ok(summary) -> {
          io.println("Desired subscriptions: " <> int.to_string(summary.found))
          io.println("Added: " <> int.to_string(summary.added))
          io.println("Removed: " <> int.to_string(list.length(summary.removed)))
          Nil
        }
      }
    }
  }
}

@external(erlang, "erlang", "halt")
fn halt(code: Int) -> Nil
