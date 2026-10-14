/// HTTP Request Handlers
///
/// Contains all the request handlers for the web application.
/// Handlers interact with the database through the context.
import core/subscription_manager
import domain/core_types.{Completed, Failed}
import domain/subscription_types.{
  type SeenVideo, type SubscriptionStatus, SeenVideo,
}
import domain/types
import engine/channel_import.{ImportSummary}
import engine/nfo
import engine/shell
import engine/subscription_progress
import engine/subscription_settings
import engine/ytdl_sub
import engine/ytdlp
import envoy
import gleam/erlang/process
import gleam/http/request
import gleam/int
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import infra/db
import infra/repo
import infra/subscription_repo
import lustre/attribute.{class}
import lustre/element.{type Element, text, to_document_string}
import lustre/element/html.{a, div, h2, li, p, ul}
import simplifile
import web/library_feed
import web/middleware.{type Context, html_response, json_response, redirect}
import web/templates
import wisp.{type Request, type Response}

/// Check the database and scheduler mailbox, so an external watchdog also
/// detects a live VM whose subscription actor has stopped responding.
pub fn health(_req: Request, ctx: Context) -> Response {
  // Check database connectivity by running a simple query
  case check_database_connection(ctx.db) {
    Ok(_) -> {
      let scheduler_ok = scheduler_responsive(ctx)
      let body =
        json.object([
          #(
            "status",
            json.string(case scheduler_ok {
              True -> "ok"
              False -> "error"
            }),
          ),
          #("database", json.string("connected")),
          #(
            "scheduler",
            json.string(case ctx.subscription_manager, scheduler_ok {
              None, _ -> "not_configured"
              Some(_), True -> "responsive"
              Some(_), False -> "unresponsive"
            }),
          ),
        ])
        |> json.to_string

      json_response(
        case scheduler_ok {
          True -> 200
          False -> 503
        },
        body,
      )
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

fn scheduler_responsive(ctx: Context) -> Bool {
  case ctx.subscription_manager {
    None -> True
    Some(manager) ->
      case process.subject_owner(manager) {
        Error(_) -> False
        Ok(_) -> {
          let reply = process.new_subject()
          process.send(manager, subscription_manager.GetStatus(reply))
          case process.receive(reply, 500) {
            Ok(_) -> True
            Error(_) -> False
          }
        }
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
        "Download",
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
          case youtube_subscription_request(ctx, validated_url) {
            Some(channel) -> stage_video_requests(ctx, [channel])
            None -> {
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
          // Validate all URLs first
          let validated =
            list.filter_map(urls, fn(url) {
              case validate_video_url(url) {
                Ok(valid_url) -> Ok(valid_url)
                Error(_) -> Error(Nil)
              }
            })

          let total_count = list.length(urls)
          let valid_count = list.length(validated)

          case valid_count {
            0 -> {
              templates.layout(
                "No Valid URLs",
                templates.error_page(
                  400,
                  "No valid URLs provided (0/"
                    <> int.to_string(total_count)
                    <> " were valid)",
                ),
              )
              |> html_response(400, _)
            }
            _ -> {
              let youtube =
                list.filter_map(validated, fn(url) {
                  youtube_subscription_request(ctx, url)
                  |> option.to_result(Nil)
                })
              let other =
                list.filter(validated, fn(url) {
                  youtube_subscription_request(ctx, url) == None
                })
              case persist_video_requests(ctx, youtube) {
                Error(error) -> channel_import_error(error)
                Ok(_) -> {
                  // Use batch insert for efficiency (handles 500+ URLs efficiently)
                  let timestamp = get_timestamp()
                  case repo.insert_jobs_batch(ctx.db, other, timestamp) {
                    Ok(inserted_count) -> {
                      io.println(
                        "Batch created: "
                        <> int.to_string(inserted_count)
                        <> " jobs (from "
                        <> int.to_string(valid_count)
                        <> " valid URLs out of "
                        <> int.to_string(total_count)
                        <> " total)",
                      )
                      // Redirect to queue page to show the new jobs
                      redirect(to: case youtube {
                        [] -> "/queue"
                        _ -> "/subscriptions"
                      })
                    }
                    Error(err) -> {
                      io.println("Batch insert failed: " <> string.inspect(err))
                      templates.layout(
                        "Error",
                        templates.error_page(500, "Failed to create batch jobs"),
                      )
                      |> html_response(500, _)
                    }
                  }
                }
              }
            }
          }
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

/// One coordinator owns YouTube work, so manual requests cannot race the
/// subscription pool or bypass its metadata, Plex and idempotency policy.
fn youtube_subscription_request(
  ctx: Context,
  url: String,
) -> Option(ytdl_sub.Channel) {
  case ctx.subscription_manager {
    None -> None
    Some(_) ->
      case channel_import.parse_channels(url) {
        [channel] -> Some(channel)
        _ -> None
      }
  }
}

fn persist_video_requests(
  ctx: Context,
  channels: List(ytdl_sub.Channel),
) -> Result(Nil, String) {
  case channels {
    [] -> Ok(Nil)
    _ -> {
      use summary <- result.try(channel_import.add_desired_channels(
        channels,
        ytdl_sub.layout_from_env().channels_file,
      ))
      case summary.added > 0 {
        True -> reload_subscription_channels(ctx)
        False -> Nil
      }
      Ok(Nil)
    }
  }
}

fn stage_video_requests(
  ctx: Context,
  channels: List(ytdl_sub.Channel),
) -> Response {
  case persist_video_requests(ctx, channels) {
    Ok(_) -> redirect("/subscriptions")
    Error(error) -> channel_import_error(error)
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

/// Settings page handler - load config from database
pub fn settings(_req: Request, ctx: Context) -> Response {
  case repo.get_download_config(ctx.db) {
    Ok(config) -> {
      let config = effective_download_config(config, ctx.output_directory)
      templates.layout(
        "Settings",
        templates.settings_page(config, get_ytdlp_version()),
      )
      |> html_response(200, _)
    }
    Error(_) -> {
      // Fallback to default config if database read fails
      let config =
        effective_download_config(ytdlp.default_config(), ctx.output_directory)
      templates.layout(
        "Settings",
        templates.settings_page(config, get_ytdlp_version()),
      )
      |> html_response(200, _)
    }
  }
}

fn effective_download_config(
  config: ytdlp.DownloadConfig,
  configured_output_directory: String,
) -> ytdlp.DownloadConfig {
  case config.output_directory == "./downloads" {
    True ->
      ytdlp.DownloadConfig(
        ..config,
        output_directory: configured_output_directory,
      )
    False -> config
  }
}

/// Update download settings handler
pub fn update_settings(req: Request, ctx: Context) -> Response {
  use form_data <- wisp.require_form(req)

  // Parse form fields with defaults
  let output_directory =
    get_form_string(form_data.values, "output_directory", ctx.output_directory)
  let format =
    get_form_string(
      form_data.values,
      "format",
      "bestvideo[ext=mp4]+bestaudio[ext=m4a]/best[ext=mp4]/best",
    )
  let max_filesize = get_form_string(form_data.values, "max_filesize", "2G")
  let audio_only = get_form_bool(form_data.values, "audio_only")
  let audio_format =
    ytdlp.string_to_audio_format(get_form_string(
      form_data.values,
      "audio_format",
      "best",
    ))
  let allow_playlist = get_form_bool(form_data.values, "allow_playlist")
  let download_timeout_minutes =
    get_form_int(form_data.values, "download_timeout_minutes", 30)
  let rate_limit_delay_ms =
    get_form_int(form_data.values, "rate_limit_delay_ms", 500)
  let bandwidth_limit = get_form_string(form_data.values, "bandwidth_limit", "")
  let use_channel_folders =
    get_form_bool(form_data.values, "use_channel_folders")

  // Build the config
  let config =
    ytdlp.DownloadConfig(
      output_directory: output_directory,
      format: format,
      max_filesize: max_filesize,
      audio_only: audio_only,
      audio_format: audio_format,
      allow_playlist: allow_playlist,
      download_timeout_ms: download_timeout_minutes * 60_000,
      rate_limit_delay_ms: rate_limit_delay_ms,
      bandwidth_limit: bandwidth_limit,
      use_channel_folders: use_channel_folders,
    )

  // Get current timestamp
  let timestamp = get_timestamp()

  // Save to database
  case repo.update_download_config(ctx.db, config, timestamp) {
    Ok(_) -> {
      io.println("✅ Download settings updated")
      redirect("/settings")
    }
    Error(err) -> {
      io.println("❌ Failed to save settings: " <> string.inspect(err))
      templates.layout(
        "Error",
        templates.error_page(500, "Failed to save settings"),
      )
      |> html_response(500, _)
    }
  }
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
type TimeUnit {
  Second
}

@external(erlang, "erlang", "system_time")
fn get_system_time_seconds(unit: TimeUnit) -> Int

fn get_timestamp() -> Int {
  get_system_time_seconds(Second)
}

// =============================================================================
// Subscription Handlers
// =============================================================================

/// Subscriptions page handler - shows subscription settings and recent videos
pub fn subscriptions(_req: Request, ctx: Context) -> Response {
  let layout = ytdl_sub.layout_from_env()
  // Load subscription config
  let config = case subscription_repo.get_config(ctx.db) {
    Ok(c) -> c
    Error(_) -> subscription_types.default_config()
  }

  // Get subscription status if manager is available
  let status = get_subscription_status(ctx)

  templates.layout(
    "Subscriptions",
    templates.subscriptions_page(
      config,
      status,
      layout.library_dir,
      subscription_settings.load(),
      get_ytdlp_version(),
      plex_library_url(),
      subscription_progress.read_snapshots(layout.root),
      subscription_repo.count_downloaded(ctx.db)
        |> result.unwrap(0),
    ),
  )
  |> html_response(200, _)
}

/// Small live status fragment; it deliberately performs no library or metadata
/// traversal and does not replace the independently loaded video grid.
pub fn subscriptions_live(_req: Request, ctx: Context) -> Response {
  let layout = ytdl_sub.layout_from_env()
  let status = get_subscription_status(ctx)
  templates.subscription_live_fragment(
    status,
    subscription_progress.read_snapshots(layout.root),
    subscription_repo.count_downloaded(ctx.db)
      |> result.unwrap(0),
  )
  |> to_document_string
  |> html_response(200, _)
}

/// Rich video grid endpoint loaded outside the page's navigation response and
/// refreshed only when the completed-video count changes.
pub fn subscriptions_feed(_req: Request, ctx: Context) -> Response {
  let layout = ytdl_sub.layout_from_env()
  let videos = case subscription_repo.list_downloaded_videos(ctx.db, 200) {
    Ok(videos) -> list.map(videos, enrich_seen_video)
    Error(_) -> []
  }
  let library_count =
    subscription_repo.count_downloaded(ctx.db)
    |> result.unwrap(list.length(videos))
  templates.subscription_feed_fragment(
    library_feed.cards(layout, videos),
    library_count,
  )
  |> to_document_string
  |> html_response(200, _)
}

/// Import channels pasted from YouTube or supplied in channels.txt format.
pub fn import_subscription_channels(req: Request, ctx: Context) -> Response {
  use form_data <- wisp.require_form(req)
  let pasted =
    form_data.values
    |> list.find(fn(pair) { pair.0 == "channel_text" })
    |> result.map(fn(pair) { pair.1 })
  case pasted {
    Error(_) ->
      templates.layout(
        "Channel import",
        templates.error_page(400, "Paste a channel list to import."),
      )
      |> html_response(400, _)
    Ok(content) -> {
      let channels_file = ytdl_sub.layout_from_env().channels_file
      case channel_import.reconcile_content(content, channels_file) {
        Ok(ImportSummary(added, removed, found)) -> {
          reload_subscription_channels(ctx)
          templates.layout(
            "Channels imported",
            templates.channel_import_result(added, list.length(removed), found),
          )
          |> html_response(200, _)
        }
        Error(err) ->
          templates.layout("Channel import", templates.error_page(400, err))
          |> html_response(400, _)
      }
    }
  }
}

/// Add one URL to the pending desired channel list without replacing it.
pub fn add_subscription_channel(req: Request, ctx: Context) -> Response {
  use form_data <- wisp.require_form(req)
  let url = form_value(form_data.values, "channel_url")
  let supplied_label = form_value(form_data.values, "channel_label")
  case url {
    Error(_) ->
      channel_import_error("Enter a YouTube channel, playlist, or video URL.")
    Ok(raw_url) ->
      case channel_import.parse_channels(raw_url) {
        [parsed] -> {
          let label = case supplied_label {
            Ok(value) -> {
              let value = string.trim(value)
              case string.is_empty(value) {
                True -> parsed.label
                False -> Some(value)
              }
            }
            _ -> parsed.label
          }
          let channel = ytdl_sub.Channel(label, parsed.url)
          let channels_file = ytdl_sub.layout_from_env().channels_file
          case channel_import.add_desired_channel(channel, channels_file) {
            Ok(ImportSummary(added, removed, found)) -> {
              reload_subscription_channels(ctx)
              templates.layout(
                "Channel added",
                templates.channel_import_result(
                  added,
                  list.length(removed),
                  found,
                ),
              )
              |> html_response(200, _)
            }
            Error(err) -> channel_import_error(err)
          }
        }
        _ ->
          channel_import_error(
            "Enter one valid YouTube channel, playlist, or video URL.",
          )
      }
  }
}

fn form_value(
  values: List(#(String, String)),
  key: String,
) -> Result(String, Nil) {
  values
  |> list.find(fn(pair) { pair.0 == key })
  |> result.map(fn(pair) { pair.1 })
}

fn reload_subscription_channels(ctx: Context) -> Nil {
  case ctx.subscription_manager {
    Some(manager) -> process.send(manager, subscription_manager.ReloadChannels)
    None -> Nil
  }
}

fn channel_import_error(message: String) -> Response {
  templates.layout("Channel import", templates.error_page(400, message))
  |> html_response(400, _)
}

fn enrich_seen_video(video: SeenVideo) -> SeenVideo {
  case string.starts_with(video.url, "file://") {
    False -> video
    True -> {
      let path = string.drop_start(video.url, 7)
      case simplifile.read(path <> ".info.json") {
        Error(_) -> video
        Ok(contents) ->
          case nfo.parse_info_json(contents) {
            Error(_) -> video
            Ok(metadata) ->
              SeenVideo(
                ..video,
                title: metadata.title,
                channel_name: prefer_option(
                  video.channel_name,
                  metadata.channel,
                ),
                duration_seconds: prefer_option(
                  video.duration_seconds,
                  metadata.duration,
                ),
                thumbnail_url: prefer_option(
                  video.thumbnail_url,
                  metadata.thumbnail,
                ),
                url: option.unwrap(metadata.webpage_url, video.url),
              )
          }
      }
    }
  }
}

fn prefer_option(current: Option(a), fallback: Option(a)) -> Option(a) {
  case current {
    Some(_) -> current
    None -> fallback
  }
}

/// Update subscription settings from form submission
pub fn update_subscription_config(req: Request, ctx: Context) -> Response {
  use form_data <- wisp.require_form(req)

  // Parse form values
  let enabled = get_form_bool(form_data.values, "enabled")
  let poll_interval =
    get_form_int(form_data.values, "poll_interval_minutes", 360)
  let current_settings = subscription_settings.load()
  let days = get_form_int(form_data.values, "days", current_settings.days)
  let workers =
    get_form_int(form_data.values, "workers", current_settings.workers)
  let skip_restricted = get_form_bool(form_data.values, "skip_restricted")

  // Get existing config to preserve last_poll_at
  let existing = case subscription_repo.get_config(ctx.db) {
    Ok(c) -> c
    Error(_) -> subscription_types.default_config()
  }

  let new_config =
    subscription_types.SubscriptionConfig(
      enabled: enabled,
      poll_interval_minutes: poll_interval,
      last_poll_at: existing.last_poll_at,
    )

  // Save config
  let timestamp = get_timestamp()
  case subscription_settings.save(workers, days, skip_restricted) {
    Error(err) -> {
      io.println("Failed to update subscription runtime settings: " <> err)
      templates.layout(
        "Error",
        templates.error_page(500, "Failed to update settings: " <> err),
      )
      |> html_response(500, _)
    }
    Ok(_) ->
      case subscription_repo.update_config(ctx.db, new_config, timestamp) {
        Ok(_) -> {
          // Notify subscription manager of config update
          case ctx.subscription_manager {
            Some(manager) -> {
              process.send(
                manager,
                subscription_manager.UpdateConfig(new_config),
              )
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
}

fn get_ytdlp_version() -> String {
  case shell.run("yt-dlp", ["--version"]) {
    Ok(shell.ShellResult(code, output, _)) if code == 0 -> {
      let version = string.trim(output)
      case string.is_empty(version) {
        True -> "unavailable"
        False -> version
      }
    }
    _ -> "unavailable"
  }
}

fn plex_library_url() -> Option(String) {
  case envoy.get("PLEX_URL") {
    Ok(base_url) ->
      plex_library_url_for(
        base_url,
        optional_env("PLEX_MACHINE_ID"),
        optional_env("PLEX_SECTION_ID"),
      )
    Error(_) -> None
  }
}

/// Build the native Plex library route when its machine id is configured.
/// Without one, link to Plex Web itself instead of emitting an invalid route.
pub fn plex_library_url_for(
  raw_url: String,
  machine_id: Option(String),
  section_id: Option(String),
) -> Option(String) {
  let base = trim_trailing_slash(string.trim(raw_url))
  case string.is_empty(base) {
    True -> None
    False ->
      case machine_id, section_id {
        Some(machine), Some(section) -> {
          let machine = string.trim(machine)
          let section = string.trim(section)
          case string.is_empty(machine) || string.is_empty(section) {
            True -> Some(base <> "/web")
            False ->
              Some(
                base
                <> "/web/index.html#!/media/"
                <> machine
                <> "/com.plexapp.plugins.library?source="
                <> section,
              )
          }
        }
        _, _ -> Some(base <> "/web")
      }
  }
}

fn optional_env(name: String) -> Option(String) {
  case envoy.get(name) {
    Ok(value) -> Some(value)
    Error(_) -> None
  }
}

fn trim_trailing_slash(path: String) -> String {
  case string.ends_with(path, "/") {
    True -> trim_trailing_slash(string.drop_end(path, 1))
    False -> path
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
