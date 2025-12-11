/// Domain types for the FractalVideoEater system
///
/// This module re-exports core types and defines orchestration-specific types.
/// Core types are defined in domain/core_types to break circular dependencies.
import core/pool_types.{type WorkerPoolSubject}
import domain/core_types
import gleam/erlang/process.{type Subject}

// Re-export core types for backwards compatibility
pub type JobId =
  core_types.JobId

pub type FormatOption =
  core_types.FormatOption

pub type VideoMetadata =
  core_types.VideoMetadata

pub type VideoStatus =
  core_types.VideoStatus

pub type VideoJob =
  core_types.VideoJob

pub type DownloadCommand =
  core_types.DownloadCommand

pub type DownloadResult =
  core_types.DownloadResult

pub type ManagerStats =
  core_types.ManagerStats

pub type DownloaderConfig =
  core_types.DownloaderConfig

/// Manager message for the orchestration layer
pub type ManagerMessage {
  PollJobs
  JobStatusUpdate(job_id: core_types.JobId, status: core_types.VideoStatus)
  UpdateProgress(job_id: core_types.JobId, progress: Int)
  Shutdown
  ForceShutdown
  SetSelf(Subject(ManagerMessage))
  // Worker pool integration - properly typed
  SetWorkerPool(pool: WorkerPoolSubject)
  // Get manager statistics
  GetStats(reply: Subject(core_types.ManagerStats))
}

/// Re-export core type constructors and helper functions
pub const new_job_id = core_types.new_job_id

pub const job_id_to_string = core_types.job_id_to_string

pub const is_terminal_status = core_types.is_terminal_status

pub const status_to_string = core_types.status_to_string

pub const string_to_status = core_types.string_to_status

/// Generate a new random job ID using cryptographically secure random bytes
pub fn new_random_job_id() -> JobId {
  let bytes = crypto_strong_rand_bytes(16)
  let id = bytes_to_hex(bytes)
  core_types.new_job_id(id)
}

@external(erlang, "crypto", "strong_rand_bytes")
fn crypto_strong_rand_bytes(n: Int) -> BitArray

/// Convert bytes to hex string
fn bytes_to_hex(bytes: BitArray) -> String {
  bytes
  |> bit_array_to_list()
  |> list_map(byte_to_hex)
  |> string_join("")
}

@external(erlang, "binary", "bin_to_list")
fn bit_array_to_list(bytes: BitArray) -> List(Int)

fn byte_to_hex(byte: Int) -> String {
  let high = byte / 16
  let low = byte % 16
  hex_char(high) <> hex_char(low)
}

fn hex_char(n: Int) -> String {
  case n {
    0 -> "0"
    1 -> "1"
    2 -> "2"
    3 -> "3"
    4 -> "4"
    5 -> "5"
    6 -> "6"
    7 -> "7"
    8 -> "8"
    9 -> "9"
    10 -> "a"
    11 -> "b"
    12 -> "c"
    13 -> "d"
    14 -> "e"
    15 -> "f"
    _ -> "0"
  }
}

@external(erlang, "lists", "map")
fn list_map(list: List(a), f: fn(a) -> b) -> List(b)

@external(erlang, "erlang", "iolist_to_binary")
fn string_join(list: List(String), sep: String) -> String
