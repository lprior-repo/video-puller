/// HTTP Request Handlers
///
/// Contains all the request handlers for the web application.
/// Handlers interact with the database through the context.
import core/subscription_manager
import domain/core_types.{Completed, Failed}
import domain/subscription_types.{type SubscriptionStatus}
import domain/types
import engine/shell
import gleam/erlang/process
import gleam/http/request
import gleam/int
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import infra/db
import infra/repo
import infra/subscription_repo
import lustre/attribute.{class}
import lustre/element.{type Element, text}
import lustre/element/html.{a, div, h2, li, p, ul}
import web/middleware.{type Context, html_response, json_response, redirect}
import web/templates
import wisp.{type Request, type Response}

/// Health endpoint - returns server status with database connectivity check
pub fn health(_req: Request, ctx: Context) -> Response {
  // Check database connectivity by running a simple query
  case check_database_connection(ctx.db) {
    Ok(_) -> {
      let body =
        json.object([
          #("status", json.string("ok")),
          #("database", json.string("connected")),
        ])
        |> json.to_string

      json_response(200, body)
    }
    Error(_) -> {
      let body =
        json.object([
          #("status", json.string("error")),
          #("database", json.string("disconnected")),
        ])
        |> json.to_string

      json_response(503, body)
    }
  }
}

/// Check database connectivity by executing a simple query
fn check_database_connection(conn: db.Db) -> Result(Nil, db.DbError) {
  // Try to execute a simple query to verify database connectivity
  db.exec_raw(conn, "SELECT 1")
}

/// Dashboard handler - shows list of all jobs
pub fn dashboard(_req: Request, ctx: Context) -> Response {
  case repo.list_jobs(ctx.db, None) {
    Ok(jobs) -> {
      templates.layout(
        "Dashboard",
        templates.dashboard_page(jobs, ctx.output_directory),
      )
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

/// Create multiple jobs from batch URL submission (one URL per line)
pub fn create_batch_jobs(req: Request, ctx: Context) -> Response {
  use form_data <- wisp.require_form(req)

  // Extract URLs from form data (textarea content)
  let urls_text =
    form_data.values
    |> list.find(fn(pair) { pair.0 == "urls" })
    |> result.map(fn(pair) { pair.1 })

  case urls_text {
    Ok(text) -> {
      // Split by newlines and filter empty lines
      let urls =
        text
        |> string.split("\n")
        |> list.map(string.trim)
        |> list.filter(fn(line) { !string.is_empty(line) })

      case list.is_empty(urls) {
        True -> {
          templates.layout(
            "No URLs",
            templates.error_page(400, "No valid URLs provided"),
          )
          |> html_response(400, _)
        }
        False -> {
          // Process each URL
          let results =
            list.map(urls, fn(url) {
              case validate_video_url(url) {
                Ok(validated_url) -> {
                  let job_id = generate_job_id()
                  let timestamp = get_timestamp()
                  case
                    repo.insert_job(ctx.db, job_id, validated_url, timestamp)
                  {
                    Ok(_) -> {
                      io.println(
                        "Created job: " <> types.job_id_to_string(job_id),
                      )
                      Ok(validated_url)
                    }
                    Error(_) -> Error("Failed to create job for: " <> url)
                  }
                }
                Error(reason) ->
                  Error("Invalid URL (" <> reason <> "): " <> url)
              }
            })

          // Count successes and failures
          let successes =
            list.filter(results, fn(r) {
              case r {
                Ok(_) -> True
                Error(_) -> False
              }
            })
          let success_count = list.length(successes)
          let total_count = list.length(urls)

          io.println(
            "Batch created: "
            <> int.to_string(success_count)
            <> "/"
            <> int.to_string(total_count)
            <> " jobs",
          )

          // Redirect to queue page to show the new jobs
          redirect(to: "/queue")
        }
      }
    }
    Error(_) -> {
      templates.layout(
        "Missing URLs",
        templates.error_page(400, "URLs are required"),
      )
      |> html_response(400, _)
    }
  }
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
            Completed | Failed(_) -> True
            _ -> False
          }
        })

      templates.layout(
        "History",
        templates.history_page(history_jobs, filter, ctx.output_directory),
      )
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
pub fn settings(_req: Request, ctx: Context) -> Response {
  templates.layout("Settings", templates.settings_page(ctx.output_directory))
  |> html_response(200, _)
}

/// Open downloads folder handler - opens the downloads directory in file manager
pub fn open_folder(req: Request, ctx: Context) -> Response {
  // Get the referer header to redirect back to the same page
  let referer = request.get_header(req, "referer")
  let redirect_to = case referer {
    Ok(url) -> {
      // Extract just the path from the full URL
      case string.split(url, "://") {
        [_, rest] ->
          case string.split(rest, "/") {
            [_, ..path_parts] -> "/" <> string.join(path_parts, "/")
            _ -> "/"
          }
        _ -> "/"
      }
    }
    Error(_) -> "/"
  }

  // Open the downloads folder
  let _ = open_directory(ctx.output_directory)

  // Redirect back to referring page
  redirect(to: redirect_to)
}

/// Open a directory in the file manager based on platform
fn open_directory(directory: String) -> Result(Nil, Nil) {
  // Detect platform using uname
  let platform_result =
    run_command("uname", ["-s"])
    |> result.map(string.lowercase)

  case platform_result {
    Ok(platform) -> {
      case string.contains(platform, "darwin") {
        True -> {
          // macOS: use 'open' to open directory in Finder
          let _ = run_command("open", [directory])
          Ok(Nil)
        }
        False -> {
          // Linux: use xdg-open to open directory
          let _ = run_command("xdg-open", [directory])
          Ok(Nil)
        }
      }
    }
    Error(_) -> {
      // Fallback to Linux behavior if platform detection fails
      let _ = run_command("xdg-open", [directory])
      Ok(Nil)
    }
  }
}

/// Show file in folder handler - opens file manager with file selected
pub fn show_in_folder(req: Request, _ctx: Context) -> Response {
  use form_data <- wisp.require_form(req)

  // Extract file path from form data
  let file_path =
    form_data.values
    |> list.find(fn(pair) { pair.0 == "path" })
    |> result.map(fn(pair) { pair.1 })

  case file_path {
    Ok(path) -> {
      // Detect platform and open file manager
      let _ = open_file_manager(path)
      // Redirect back to history page
      redirect(to: "/history")
    }
    Error(_) -> {
      // If no path provided, just redirect back
      redirect(to: "/history")
    }
  }
}

/// Open file manager with the file selected based on platform
fn open_file_manager(file_path: String) -> Result(Nil, Nil) {
  // Detect platform using uname
  let platform_result =
    run_command("uname", ["-s"])
    |> result.map(string.lowercase)

  case platform_result {
    Ok(platform) -> {
      case string.contains(platform, "darwin") {
        True -> {
          // macOS: use 'open -R' to reveal file in Finder
          let _ = run_command("open", ["-R", file_path])
          Ok(Nil)
        }
        False -> {
          // Linux: use xdg-open on parent directory
          // Extract parent directory from file path
          let parent_dir = get_parent_directory(file_path)
          let _ = run_command("xdg-open", [parent_dir])
          Ok(Nil)
        }
      }
    }
    Error(_) -> {
      // Fallback to Linux behavior if platform detection fails
      let parent_dir = get_parent_directory(file_path)
      let _ = run_command("xdg-open", [parent_dir])
      Ok(Nil)
    }
  }
}

/// Run a shell command safely using the shell module
fn run_command(cmd: String, args: List(String)) -> Result(String, Nil) {
  case shell.run_simple(cmd, args) {
    Ok(output) -> Ok(output)
    Error(_) -> Error(Nil)
  }
}

/// Get parent directory from a file path
pub fn get_parent_directory(path: String) -> String {
  // Split path by / and remove last component
  let parts = string.split(path, "/")
  let parent_parts = case list.reverse(parts) {
    [_last, ..rest] -> list.reverse(rest)
    [] -> []
  }

  case parent_parts {
    [] -> "."
    _ -> string.join(parent_parts, "/")
  }
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
pub fn validate_video_url(url: String) -> Result(String, String) {
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

// =============================================================================
// Subscription Handlers
// =============================================================================

/// Subscriptions page handler - shows subscription settings and recent videos
pub fn subscriptions(_req: Request, ctx: Context) -> Response {
  // Load subscription config
  let config = case subscription_repo.get_config(ctx.db) {
    Ok(c) -> c
    Error(_) -> subscription_types.default_config()
  }

  // Get subscription status if manager is available
  let status = get_subscription_status(ctx)

  // Load recent seen videos
  let recent_videos = case subscription_repo.list_seen_videos(ctx.db, 50, 0) {
    Ok(videos) -> videos
    Error(_) -> []
  }

  templates.layout(
    "Subscriptions",
    templates.subscriptions_page(config, status, recent_videos),
  )
  |> html_response(200, _)
}

/// Update subscription settings from form submission
pub fn update_subscription_config(req: Request, ctx: Context) -> Response {
  use form_data <- wisp.require_form(req)

  // Parse form values
  let enabled = get_form_bool(form_data.values, "enabled")
  let poll_interval =
    get_form_int(form_data.values, "poll_interval_minutes", 60)
  let browser =
    get_form_string(form_data.values, "browser", "firefox")
    |> subscription_types.string_to_browser
  let max_age_days = get_form_int(form_data.values, "max_age_days", 7)
  let min_duration = get_form_int(form_data.values, "min_duration_seconds", 120)
  let max_duration_str =
    get_form_string(form_data.values, "max_duration_seconds", "")
  let max_duration = case int.parse(max_duration_str) {
    Ok(d) if d > 0 -> Some(d)
    _ -> None
  }
  let keyword_filter =
    get_form_string(form_data.values, "keyword_filter", "")
    |> parse_comma_list
  let keyword_exclude =
    get_form_string(form_data.values, "keyword_exclude", "")
    |> parse_comma_list

  // Get existing config to preserve last_poll_at
  let existing = case subscription_repo.get_config(ctx.db) {
    Ok(c) -> c
    Error(_) -> subscription_types.default_config()
  }

  let new_config =
    subscription_types.SubscriptionConfig(
      enabled: enabled,
      poll_interval_minutes: poll_interval,
      browser: browser,
      cookies_path: existing.cookies_path,
      max_age_days: max_age_days,
      min_duration_seconds: min_duration,
      max_duration_seconds: max_duration,
      keyword_filter: keyword_filter,
      keyword_exclude: keyword_exclude,
      last_poll_at: existing.last_poll_at,
    )

  // Save config
  let timestamp = get_timestamp()
  case subscription_repo.update_config(ctx.db, new_config, timestamp) {
    Ok(_) -> {
      // Notify subscription manager of config update
      case ctx.subscription_manager {
        Some(manager) -> {
          process.send(manager, subscription_manager.UpdateConfig(new_config))
        }
        None -> Nil
      }

      io.println("Subscription config updated")
      redirect(to: "/subscriptions")
    }
    Error(err) -> {
      io.println(
        "Failed to update subscription config: " <> string.inspect(err),
      )
      templates.layout(
        "Error",
        templates.error_page(500, "Failed to update settings"),
      )
      |> html_response(500, _)
    }
  }
}

/// Trigger a manual subscription poll
pub fn poll_subscriptions(_req: Request, ctx: Context) -> Response {
  case ctx.subscription_manager {
    Some(manager) -> {
      process.send(manager, subscription_manager.Poll)
      io.println("Manual subscription poll triggered")
      redirect(to: "/subscriptions")
    }
    None -> {
      io.println("Subscription manager not available")
      templates.layout(
        "Error",
        templates.error_page(500, "Subscription manager not running"),
      )
      |> html_response(500, _)
    }
  }
}

/// Get subscription status from manager
fn get_subscription_status(ctx: Context) -> SubscriptionStatus {
  case ctx.subscription_manager {
    Some(manager) -> {
      let reply_subject = process.new_subject()
      process.send(manager, subscription_manager.GetStatus(reply_subject))
      // Wait up to 5 seconds for response
      case process.receive(reply_subject, 5000) {
        Ok(status) -> status
        Error(_) -> default_subscription_status()
      }
    }
    None -> default_subscription_status()
  }
}

/// Default subscription status when manager unavailable
fn default_subscription_status() -> SubscriptionStatus {
  subscription_types.SubscriptionStatus(
    enabled: False,
    last_poll_at: None,
    next_poll_at: None,
    last_result: None,
    is_polling: False,
  )
}

/// Get boolean value from form data
fn get_form_bool(values: List(#(String, String)), key: String) -> Bool {
  case list.find(values, fn(pair) { pair.0 == key }) {
    Ok(#(_, value)) ->
      case value {
        "true" | "1" | "on" | "yes" -> True
        _ -> False
      }
    Error(_) -> False
  }
}

/// Get integer value from form data with default
fn get_form_int(
  values: List(#(String, String)),
  key: String,
  default: Int,
) -> Int {
  case list.find(values, fn(pair) { pair.0 == key }) {
    Ok(#(_, value)) ->
      case int.parse(value) {
        Ok(n) -> n
        Error(_) -> default
      }
    Error(_) -> default
  }
}

/// Get string value from form data with default
fn get_form_string(
  values: List(#(String, String)),
  key: String,
  default: String,
) -> String {
  case list.find(values, fn(pair) { pair.0 == key }) {
    Ok(#(_, value)) -> string.trim(value)
    Error(_) -> default
  }
}

/// Parse comma-separated list into List(String)
fn parse_comma_list(s: String) -> List(String) {
  case string.is_empty(string.trim(s)) {
    True -> []
    False ->
      s
      |> string.split(",")
      |> list.map(string.trim)
      |> list.filter(fn(item) { !string.is_empty(item) })
  }
}
