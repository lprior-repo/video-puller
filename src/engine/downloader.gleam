/// Downloader Actor
///
/// OTP actor that handles individual video download jobs.
/// Receives commands to start downloads and reports progress/completion.

import domain/types.{type DownloadResult, type JobId}
import engine/parser
import engine/shell
import engine/ytdlp
import gleam/erlang/process.{type Subject}
import gleam/list
import gleam/option.{type Option}
import gleam/otp/actor
import gleam/result
import gleam/string

/// Actor state for the downloader
pub type DownloaderState {
  DownloaderState(config: ytdlp.DownloadConfig, current_job: Option(JobId))
}

/// Messages the downloader actor can receive
pub type DownloaderMessage {
  Download(job_id: JobId, url: String, reply: Subject(DownloadResult))
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

/// Handle messages sent to the downloader actor
fn handle_message(
  message: DownloaderMessage,
  state: DownloaderState,
) -> actor.Next(DownloaderState, DownloaderMessage) {
  case message {
    Download(job_id, url, reply) -> {
      // Execute the download
      let result = execute_download(job_id, url, state.config)

      // Send result back to caller
      process.send(reply, result)

      // Update state to track current job
      let new_state = DownloaderState(
        config: state.config,
        current_job: option.Some(job_id),
      )
      actor.continue(new_state)
    }

    Shutdown -> actor.stop()
  }
}

/// Execute a download using yt-dlp
fn execute_download(
  job_id: JobId,
  url: String,
  config: ytdlp.DownloadConfig,
) -> DownloadResult {
  // Build command arguments
  case ytdlp.build_download_args(url, job_id, config) {
    Ok(args) -> {
      // Execute yt-dlp command
      case shell.run("yt-dlp", args) {
        Ok(result) -> {
          case result.exit_code {
            0 -> {
              // Success - extract path from output
              let path = extract_path_from_output(result.stdout, config)
              types.DownloadComplete(job_id, path)
            }
            _ -> {
              // Failed - extract error from stderr
              let error = parser.extract_error(result.stderr)
              types.DownloadFailed(job_id, error)
            }
          }
        }
        Error(shell.ExecutionError(msg)) ->
          types.DownloadFailed(job_id, "Execution error: " <> msg)
        Error(shell.InvalidCommand(msg)) ->
          types.DownloadFailed(job_id, "Invalid command: " <> msg)
      }
    }
    Error(msg) -> types.DownloadFailed(job_id, "Invalid URL: " <> msg)
  }
}

/// Extract the output file path from yt-dlp output
fn extract_path_from_output(
  output: String,
  config: ytdlp.DownloadConfig,
) -> String {
  // Look for "[download] Destination: " line or similar
  let lines = string.split(output, "\n")

  case
    lines
    |> list.find(fn(line) { string.contains(line, "[download] Destination:") })
  {
    Ok(line) -> {
      // Extract path after "Destination: "
      case string.split(line, "Destination: ") {
        [_, path] -> string.trim(path)
        _ -> config.output_directory <> "/unknown.mp4"
      }
    }
    Error(_) -> config.output_directory <> "/unknown.mp4"
  }
}
