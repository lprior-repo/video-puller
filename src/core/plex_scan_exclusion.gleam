/// Keep a dedicated media data tree out of Plex libraries scanning its parent.
import envoy
import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import simplifile

/// Explicit opt-in: a dedicated Plex library must point at DATA_DIR/library.
pub fn configure(data_root: String) -> Nil {
  case envoy.get("PLEX_PARENT_SCAN_EXCLUSION"), envoy.get("STORAGE_VOLUME") {
    Ok("true"), Ok(volume) -> {
      case ensure(volume, data_root) {
        Ok(_) -> Nil
        Error(message) -> io.println("Plex parent scan exclusion: " <> message)
      }
    }
    _, _ -> Nil
  }
}

/// Reject paths and pattern characters that could exclude unrelated content.
pub fn exclusion_rule(
  volume: String,
  data_root: String,
) -> Result(String, String) {
  let volume = drop_trailing_slashes(volume)
  let data_root = drop_trailing_slashes(data_root)
  let prefix = volume <> "/"
  case
    valid_path(volume)
    && valid_path(data_root)
    && volume != ""
    && string.starts_with(data_root, prefix)
  {
    False ->
      Error("data directory must be a strict child of the storage volume")
    True -> Ok(string.drop_start(data_root, string.length(prefix)) <> "/*")
  }
}

fn valid_path(path: String) -> Bool {
  string.starts_with(path, "/")
  && !list.any(["*", "?", "[", "]", "\n", "\r"], fn(char) {
    string.contains(path, char)
  })
  && list.all(string.split(string.drop_start(path, 1), "/"), fn(part) {
    part != "" && part != "." && part != ".."
  })
}

/// Preserve custom rules and comments; an existing exact rule makes this a no-op.
pub fn ensure(volume: String, data_root: String) -> Result(Nil, String) {
  use rule <- result.try(exclusion_rule(volume, data_root))
  let path = drop_trailing_slashes(volume) <> "/.plexignore"
  use _ <- result.try(case simplifile.is_symlink(path) {
    Ok(True) -> Error("refusing a symlinked .plexignore")
    Ok(False) | Error(simplifile.Enoent) -> Ok(Nil)
    Error(_) -> Error("cannot inspect .plexignore")
  })
  use existing <- result.try(case simplifile.read(path) {
    Ok(content) -> Ok(content)
    Error(simplifile.Enoent) -> Ok("")
    Error(_) -> Error("cannot read .plexignore")
  })
  case
    list.any(string.split(existing, "\n"), fn(line) {
      string.trim(line) == rule
    })
  {
    True -> Ok(Nil)
    False -> {
      let separator = case existing == "" || string.ends_with(existing, "\n") {
        True -> ""
        False -> "\n"
      }
      let temporary =
        path <> ".tmp." <> int.to_string(unique_integer([Positive]))
      use _ <- result.try(
        simplifile.write(temporary, existing <> separator <> rule <> "\n")
        |> result.map_error(fn(_) { "cannot write .plexignore" }),
      )
      case simplifile.rename(temporary, path) {
        Ok(_) -> Ok(Nil)
        Error(_) -> {
          let _ = simplifile.delete(temporary)
          Error("cannot replace .plexignore")
        }
      }
    }
  }
}

type IntegerOption {
  Positive
}

@external(erlang, "erlang", "unique_integer")
fn unique_integer(options: List(IntegerOption)) -> Int

fn drop_trailing_slashes(path: String) -> String {
  case string.ends_with(path, "/") {
    True -> drop_trailing_slashes(string.drop_end(path, 1))
    False -> path
  }
}
