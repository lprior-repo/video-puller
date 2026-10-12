/// Warm the shared compact cache without changing raw video metadata.
import engine/media_info
import engine/ytdl_sub
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{None, Some}

pub fn main() -> Nil {
  let layout = ytdl_sub.layout_from_env()
  let files = ytdl_sub.list_library_media(layout.library_dir)
  let cached =
    list.fold(files, 0, fn(count, path) {
      case media_info.read(path, layout.root <> "/cache/video-metadata") {
        Some(_) -> count + 1
        None -> count
      }
    })
  io.println("Compact metadata cached: " <> int.to_string(cached))
}
