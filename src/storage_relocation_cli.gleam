/// Retarget a copied Video Puller data tree to its new absolute location.
///
/// Usage: gleam run -m storage_relocation_cli -- OLD_ROOT NEW_ROOT
import core/storage_relocation
import gleam/io
import gleam/list

pub fn main() -> Nil {
  case arguments() {
    [old_root, new_root] ->
      case storage_relocation.relocate(old_root, new_root) {
        Ok(summary) -> {
          io.println("Updated copied database paths:")
          io.println("  source root: " <> summary.old_root)
          io.println("  destination root: " <> summary.new_root)
          io.println("  database: " <> summary.database)
        }
        Error(message) -> fail(message)
      }
    _ -> fail("Usage: storage_relocation_cli OLD_ROOT NEW_ROOT")
  }
}

fn fail(message: String) -> Nil {
  io.println("Storage relocation failed: " <> message)
  halt(1)
}

@external(erlang, "init", "get_plain_arguments")
fn raw_arguments() -> List(List(Int))

fn arguments() -> List(String) {
  list.map(raw_arguments(), argument_to_string)
}

@external(erlang, "unicode", "characters_to_binary")
fn argument_to_string(argument: List(Int)) -> String

@external(erlang, "erlang", "halt")
fn halt(code: Int) -> Nil
