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

/// Check if a downloaded file exists for the given job_id
/// Returns the full path if found, or Error if no file exists
fn find_downloaded_file(
  job_id: JobId,
  output_directory: String,
) -> Result(String, Nil) {
  let job_id_str = core_types.job_id_to_string(job_id)

  // Read directory contents
  case simplifile.read_directory(output_directory) {
    Ok(files) -> {
      // Look for files that start with the job_id
      files
      |> list.filter(fn(filename) { string.starts_with(filename, job_id_str) })
      |> list.first
      |> result.map(fn(filename) { output_directory <> "/" <> filename })
    }
    Error(_) -> Error(Nil)
  }
}

/// Generate NFO sidecar file from the .info.json file
/// Searches for the corresponding .info.json file and generates .nfo
fn generate_nfo_sidecar(video_path: String, output_directory: String) -> Nil {
  // Find the .info.json file that matches the video
  // yt-dlp creates {title}.info.json alongside {title}.mp4
  let video_basename = get_basename_without_ext(video_path)

  case simplifile.read_directory(output_directory) {
    Ok(files) -> {
      // Find matching .info.json file
      let info_json_opt =
        files
        |> list.filter(fn(f) {
          string.ends_with(f, ".info.json")
          && string.starts_with(f, video_basename)
        })
        |> list.first

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

/// Get basename of a file path without extension
fn get_basename_without_ext(path: String) -> String {
  let parts = string.split(path, "/")
  let filename = case list.last(parts) {
    Ok(f) -> f
    Error(_) -> path
  }
  // Remove extension
  case string.split(filename, ".") {
    [base, ..] -> base
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
      // Execute yt-dlp command with streaming output
      case
        shell.run_streaming("yt-dlp", args, fn(line) {
          // Parse progress from each line
          case parser.parse_progress(line) {
            Ok(progress_info) -> {
              // Only send update if progress changed
              process.send(
                progress_subject,
                UpdateProgress(job_id, progress_info.percentage),
              )
            }
            Error(_) -> Nil
          }
          Nil
        })
      {
        Ok(exit_code) -> {
          case exit_code {
            0 -> {
              // Exit code 0 - check for downloaded file
              case find_downloaded_file(job_id, config.output_directory) {
                Ok(path) -> {
                  // Generate NFO sidecar file
                  generate_nfo_sidecar(path, config.output_directory)
                  DownloadComplete(job_id, path)
                }
                Error(_) -> {
                  // Exit code 0 but no file found - unexpected
                  DownloadFailed(
                    job_id,
                    "Download reported success but file not found",
                  )
                }
              }
            }
            _ -> {
              // Non-zero exit code - check if file exists anyway
              // yt-dlp returns exit code 1 for warnings even when download succeeds
              case find_downloaded_file(job_id, config.output_directory) {
                Ok(path) -> {
                  // File exists despite non-zero exit code - treat as success
                  // Generate NFO sidecar file
                  generate_nfo_sidecar(path, config.output_directory)
                  DownloadComplete(job_id, path)
                }
                Error(_) -> {
                  // No file found - genuine failure
                  DownloadFailed(job_id, "Download failed with exit code")
                }
              }
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
