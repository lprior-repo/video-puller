/// Companion matching shared by expired and watched episode cleanup.
/// Filesystem checks and the decision to delete media remain with the caller.
import gleam/list
import gleam/string

/// Match only known companion names, giving an equally or more specific
/// sibling media stem priority. For example, Clip.extended.srt belongs to
/// Clip.extended.mp4, not Clip.mp4; Clip-thumb.jpg can belong to Clip-thumb.mp4.
///
/// Pass stems for ALL other media entries in the directory, including retained
/// media and symlinks. Equal stems (different media extensions) are ambiguous:
/// preserve their shared sidecars while another media entry still exists.
pub fn owned(
  stem: String,
  filename: String,
  other_stems: List(String),
) -> Bool {
  matches(stem, filename)
  && !list.any(other_stems, fn(other) {
    string.length(other) >= string.length(stem) && matches(other, filename)
  })
}

fn matches(stem: String, filename: String) -> Bool {
  list.any(
    [
      ".info.json",
      ".nfo",
      ".jpg",
      ".jpeg",
      ".png",
      ".webp",
      "-thumb.jpg",
      "-thumb.jpeg",
      "-thumb.png",
      "-thumb.webp",
    ],
    fn(suffix) { filename == stem <> suffix },
  )
  || subtitle_sidecar(stem, filename)
}

fn subtitle_sidecar(stem: String, filename: String) -> Bool {
  list.any([".srt", ".vtt", ".ass", ".ssa", ".lrc"], fn(extension) {
    case filename == stem <> extension {
      True -> True
      False -> {
        let prefix = stem <> "."
        case
          string.starts_with(filename, prefix)
          && string.ends_with(filename, extension)
        {
          False -> False
          True -> {
            let track =
              filename
              |> string.drop_start(string.length(prefix))
              |> string.drop_end(string.length(extension))
            valid_subtitle_track(track)
          }
        }
      }
    }
  })
}

fn valid_subtitle_track(track: String) -> Bool {
  case string.length(track) > 0 && string.length(track) <= 20 {
    False -> False
    True ->
      list.all(string.to_graphemes(track), fn(char) {
        case char {
          "-" -> True
          _ ->
            string.length(char) == 1
            && string.contains(
              "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789",
              char,
            )
        }
      })
  }
}
