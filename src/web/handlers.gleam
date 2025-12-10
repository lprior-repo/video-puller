/// HTTP Request Handlers
///
/// Contains all the request handlers for the web application.
/// Handlers interact with the database through the context.
import domain/types
import gleam/io
import gleam/list
import gleam/option.{None}
import gleam/result
import gleam/string
import infra/repo
import lustre/attribute.{class}
import lustre/element.{type Element, text}
import lustre/element/html.{a, div, h2, li, p, ul}
import web/middleware.{type Context, html_response, redirect}
import web/templates
import wisp.{type Request, type Response}

/// Dashboard handler - shows list of all jobs
pub fn dashboard(_req: Request, ctx: Context) -> Response {
  case repo.list_jobs(ctx.db, None) {
    Ok(jobs) -> {
      templates.layout("Dashboard", templates.dashboard_page(jobs))
      |> html_response(200, _)
    }
    Error(err) -> {
      io.println("Failed to fetch jobs: " <> string.inspect(err))
      templates.layout(
        "Error",
        templates.error_page(500, "Failed to load jobs"),
      )
      |> html_response(500, _)
    }
  }
}

/// Create a new job from form submission
pub fn create_job(req: Request, ctx: Context) -> Response {
  use form_data <- wisp.require_form(req)

  // Extract URL from form data
  let url =
    form_data.values
    |> list.find(fn(pair) { pair.0 == "url" })
    |> result.map(fn(pair) { pair.1 })

  case url {
    Ok(url_value) -> {
      case validate_video_url(url_value) {
        Ok(validated_url) -> {
          // Generate a new job ID and timestamp
          let job_id = generate_job_id()
          let timestamp = get_timestamp()

          case repo.insert_job(ctx.db, job_id, validated_url, timestamp) {
            Ok(_) -> {
              io.println("Created job: " <> types.job_id_to_string(job_id))
              redirect(to: "/")
            }
            Error(err) -> {
              io.println("Failed to create job: " <> string.inspect(err))
              templates.layout(
                "Error",
                templates.error_page(500, "Failed to create job"),
              )
              |> html_response(500, _)
            }
          }
        }
        Error(reason) -> {
          templates.layout(
            "Invalid URL",
            templates.error_page(400, "Invalid URL: " <> reason),
          )
          |> html_response(400, _)
        }
      }
    }
    Error(_) -> {
      templates.layout(
        "Missing URL",
        templates.error_page(400, "URL is required"),
      )
      |> html_response(400, _)
    }
  }
}

/// Get a specific job by ID
pub fn get_job(_req: Request, _ctx: Context, _id: String) -> Response {
  // For now, just redirect to dashboard since we don't have a detail view
  redirect(to: "/")
}

/// Queue page handler - shows pending jobs only
pub fn queue(_req: Request, ctx: Context) -> Response {
  case repo.list_jobs(ctx.db, option.Some("pending")) {
    Ok(jobs) -> {
      templates.layout("Queue", templates.queue_page(jobs))
      |> html_response(200, _)
    }
    Error(err) -> {
      io.println("Failed to fetch pending jobs: " <> string.inspect(err))
      templates.layout(
        "Error",
        templates.error_page(500, "Failed to load queue"),
      )
      |> html_response(500, _)
    }
  }
}

/// History page handler - shows completed and failed jobs
pub fn history(_req: Request, ctx: Context) -> Response {
  // For now, show all completed and failed jobs
  // TODO: Implement filter from query params when needed
  let filter = "all"

  // Fetch all jobs and filter to completed/failed
  case repo.list_jobs(ctx.db, option.None) {
    Ok(all_jobs) -> {
      // Filter to only show completed or failed jobs
      let history_jobs =
        list.filter(all_jobs, fn(job) {
          case job.status {
            types.Completed | types.Failed(_) -> True
            _ -> False
          }
        })

      templates.layout("History", templates.history_page(history_jobs, filter))
      |> html_response(200, _)
    }
    Error(err) -> {
      io.println("Failed to fetch history: " <> string.inspect(err))
      templates.layout(
        "Error",
        templates.error_page(500, "Failed to load history"),
      )
      |> html_response(500, _)
    }
  }
}

/// About page handler
pub fn about(_req: Request, _ctx: Context) -> Response {
  templates.layout("About", about_content())
  |> html_response(200, _)
}

/// Settings page handler
pub fn settings(_req: Request, _ctx: Context) -> Response {
  templates.layout("Settings", templates.settings_page())
  |> html_response(200, _)
}

/// About page content
fn about_content() -> Element(a) {
  div([class("bg-gray-800 rounded-lg shadow-lg px-6 py-8")], [
    h2([class("text-2xl font-bold mb-4")], [text("About FractalVideoEater")]),
    p([class("text-gray-300 mb-4")], [
      text(
        "FractalVideoEater is a video download management system built with Gleam. It uses yt-dlp under the hood to support downloading from hundreds of video platforms.",
      ),
    ]),
    h2([class("text-xl font-semibold mb-2 mt-6")], [text("Features")]),
    ul([class("list-disc list-inside text-gray-300 mb-4")], [
      li([], [text("Queue-based download management")]),
      li([], [text("Support for 1000+ video sites via yt-dlp")]),
      li([], [text("Progress tracking and status updates")]),
      li([], [text("Automatic retry on failure")]),
      li([], [text("Built with OTP for fault tolerance")]),
    ]),
    h2([class("text-xl font-semibold mb-2 mt-6")], [text("Technology")]),
    p([class("text-gray-300")], [
      text("Built with "),
      a(
        [
          attribute.href("https://gleam.run"),
          attribute.attribute("target", "_blank"),
        ],
        [text("Gleam")],
      ),
      text(", "),
      a(
        [
          attribute.href("https://github.com/gleam-wisp/wisp"),
          attribute.attribute("target", "_blank"),
        ],
        [text("Wisp")],
      ),
      text(", and "),
      a(
        [
          attribute.href("https://github.com/lustre-labs/lustre"),
          attribute.attribute("target", "_blank"),
        ],
        [text("Lustre")],
      ),
      text("."),
    ]),
  ])
}

/// Validate a video URL
fn validate_video_url(url: String) -> Result(String, String) {
  let trimmed = string.trim(url)
  case string.is_empty(trimmed) {
    True -> Error("URL cannot be empty")
    False ->
      case
        string.starts_with(trimmed, "http://")
        || string.starts_with(trimmed, "https://")
      {
        True -> Ok(trimmed)
        False -> Error("URL must start with http:// or https://")
      }
  }
}

/// Generate a unique job ID
fn generate_job_id() -> types.JobId {
  let id = generate_uuid()
  types.new_job_id(id)
}

/// Generate a UUID using Erlang's crypto module
fn generate_uuid() -> String {
  let bytes = crypto_strong_rand_bytes(16)
  bytes_to_hex(bytes)
}

@external(erlang, "crypto", "strong_rand_bytes")
fn crypto_strong_rand_bytes(n: Int) -> BitArray

/// Convert bytes to hex string
fn bytes_to_hex(bytes: BitArray) -> String {
  bytes
  |> binary_to_hex_list
  |> list.map(int_to_hex_char)
  |> string.concat
}

@external(erlang, "binary", "bin_to_list")
fn binary_to_hex_list(bytes: BitArray) -> List(Int)

fn int_to_hex_char(n: Int) -> String {
  let high = n / 16
  let low = n % 16
  hex_digit(high) <> hex_digit(low)
}

fn hex_digit(n: Int) -> String {
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
    _ -> "f"
  }
}

/// Get current Unix timestamp
@external(erlang, "os", "system_time")
fn get_timestamp() -> Int
