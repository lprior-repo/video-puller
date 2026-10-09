/// Downloader Actor
///
/// OTP actor that handles individual video download jobs.
/// Receives commands to start downloads and reports progress/completion.
import domain/core_types.{DownloadComplete, DownloadFailed}
import domain/types.{
  type DownloadResult, type JobId, type ManagerMessage, UpdateProgress,
}
import engine/nfo
import engine/parser
import engine/shell
import engine/ytdlp
import gleam/erlang/process.{type Subject}
import gleam/list
import gleam/option.{type Option}
import gleam/otp/actor
import gleam/result
import gleam/string
import simplifile

/// Actor state for the downloader
pub type DownloaderState {
  DownloaderState(config: ytdlp.DownloadConfig, current_job: Option(JobId))
}

/// Messages the downloader actor can receive
pub type DownloaderMessage {
  Download(
    job_id: JobId,
    url: String,
    reply: Subject(DownloadResult),
    progress_subject: Subject(ManagerMessage),
  )
  Shutdown
}

/// Start a new downloader actor
pub fn start(
  config: ytdlp.DownloadConfig,
) -> Result(Subject(DownloaderMessage), actor.StartError) {
  let state = DownloaderState(config: config, current_job: option.None)

  actor.start(
    actor.new(state)
    |> actor.on_message(handle_message),
  )
  |> result.map(fn(started) { started.data })
}

/// Collect lines printed by yt-dlp while the command is running.
///
/// The `after_move:filepath` print hook is emitted after the final media path
/// is known. Keeping all output in a private subject lets the progress callback
/// remain streaming while the completion path can select the reported file.
fn collect_output_lines(
  subject: Subject(String),
  lines: List(String),
) -> List(String) {
  case process.receive(subject, 0) {
    Ok(line) -> collect_output_lines(subject, [line, ..lines])
    Error(_) -> list.reverse(lines)
  }
}

/// Select the path reported by yt-dlp's after-move hook.
fn find_reported_file(lines: List(String)) -> Result(String, Nil) {
  lines
  |> list.map(string.trim)
  |> list.find(fn(path) {
    case simplifile.is_file(path) {
      Ok(True) -> True
      _ -> False
    }
  })
}

/// Generate NFO sidecar file from the .info.json file next to the media file.
fn generate_nfo_sidecar(video_path: String) -> Nil {
  let output_directory = get_parent_directory(video_path)
  let video_basename = get_basename_without_ext(video_path)

  case simplifile.read_directory(output_directory) {
    Ok(files) -> {
      let info_json_opt =
        files
        |> list.find(fn(f) { f == video_basename <> ".info.json" })

      case info_json_opt {
        Ok(info_json_file) -> {
          let info_json_path = output_directory <> "/" <> info_json_file
          // Generate NFO (ignore errors - NFO is nice to have, not critical)
          let _ = nfo.generate_nfo_from_info_json(info_json_path)
          Nil
        }
        Error(_) -> Nil
      }
    }
    Error(_) -> Nil
  }
}

/// Get the directory portion of a path.
fn get_parent_directory(path: String) -> String {
  let parts =
    path
    |> string.split("/")
    |> list.reverse
    |> list.drop(1)
    |> list.reverse

  case string.join(parts, "/") {
    "" -> "."
    directory -> directory
  }
}

/// Get basename of a file path without its final extension.
fn get_basename_without_ext(path: String) -> String {
  let parts = string.split(path, "/")
  let filename = case list.last(parts) {
    Ok(f) -> f
    Error(_) -> path
  }
  let extension_parts = string.split(filename, ".") |> list.reverse
  case extension_parts {
    [_extension, ..rest] -> string.join(list.reverse(rest), ".")
    [] -> filename
  }
}

/// Handle messages sent to the downloader actor
fn handle_message(
  state: DownloaderState,
  message: DownloaderMessage,
) -> actor.Next(DownloaderState, DownloaderMessage) {
  case message {
    Download(job_id, url, reply, progress_subject) -> {
      // Execute the download with streaming progress
      let result =
        execute_download_streaming(job_id, url, state.config, progress_subject)

      // Send result back to caller
      process.send(reply, result)

      // Update state to track current job
      let new_state =
        DownloaderState(config: state.config, current_job: option.Some(job_id))
      actor.continue(new_state)
    }

    Shutdown -> actor.stop()
  }
}

/// Execute a download using yt-dlp with streaming progress updates
fn execute_download_streaming(
  job_id: JobId,
  url: String,
  config: ytdlp.DownloadConfig,
  progress_subject: Subject(ManagerMessage),
) -> DownloadResult {
  // Build command arguments
  case ytdlp.build_download_args(url, job_id, config, option.None) {
    Ok(args) -> {
      let reported_output = process.new_subject()

      // Execute yt-dlp with streaming progress and collect its machine-readable
      // after-move path output. The path is emitted only after the final move,
      // so it also handles channel subfolders and post-processing extensions.
      let shell_result =
        shell.run_streaming("yt-dlp", args, fn(line) {
          process.send(reported_output, line)

          case parser.parse_progress(line) {
            Ok(progress_info) ->
              process.send(
                progress_subject,
                UpdateProgress(job_id, progress_info.percentage),
              )
            Error(_) -> Nil
          }
          Nil
        })

      let output_lines = collect_output_lines(reported_output, [])

      case shell_result {
        Ok(exit_code) -> {
          case find_reported_file(output_lines) {
            Ok(path) -> {
              // The reported path is the same path used for NFO generation and
              // later worker-pool verification. A file landed, so a reported
              // path wins even when yt-dlp exits non-zero for later warnings.
              generate_nfo_sidecar(path)
              DownloadComplete(job_id, path)
            }
            Error(_) ->
              case exit_code {
                0 ->
                  DownloadFailed(
                    job_id,
                    "Download reported success but file not found",
                  )
                _ -> DownloadFailed(job_id, "Download failed with exit code")
              }
          }
        }
        Error(shell.ExecutionError(msg)) ->
          DownloadFailed(job_id, "Execution error: " <> msg)
        Error(shell.InvalidCommand(msg)) ->
          DownloadFailed(job_id, "Invalid command: " <> msg)
      }
    }
    Error(msg) -> DownloadFailed(job_id, "Invalid URL: " <> msg)
  }
}
