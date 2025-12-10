/// HTML Templates using Lustre SSR
///
/// Server-side rendered HTML templates for the video puller UI.
/// Uses Lustre's element API for type-safe HTML generation.
import domain/core_types.{
  type VideoStatus, Completed, Downloading, Failed, Pending, job_id_to_string,
}
import domain/subscription_types.{
  type PollResult, type SeenVideo, type SubscriptionConfig,
  type SubscriptionStatus,
}
import domain/types.{type VideoJob}
import engine/video_filter
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import lustre/attribute.{attribute, class, href, name, placeholder, type_}
import lustre/element.{type Element, text}
import lustre/element/html.{
  a, body, button, details, div, form, h1, h2, head, html, input, label, li,
  link, main, meta, nav, option, p, select, span, style, summary, title, ul,
}

/// Base HTML layout wrapper
pub fn layout(page_title: String, content: Element(a)) -> String {
  html([attribute("lang", "en")], [
    head([], [
      meta([attribute("charset", "UTF-8")]),
      meta([
        name("viewport"),
        attribute("content", "width=device-width, initial-scale=1.0"),
      ]),
      title([], "GleanDown - " <> page_title),
      link([
        attribute("rel", "stylesheet"),
        href("/static/styles.css"),
      ]),
      style([], inline_styles()),
    ]),
    body([class("min-h-screen bg-gray-900 text-gray-100 flex-layout")], [
      sidebar(),
      main([class("main-content flex-grow px-4 py-8")], [content]),
    ]),
  ])
  |> element.to_document_string
}

/// Inline CSS styles for the application
fn inline_styles() -> String {
  "
  * { box-sizing: border-box; margin: 0; padding: 0; }
  body { font-family: system-ui, -apple-system, sans-serif; line-height: 1.5; }
  .min-h-screen { min-height: 100vh; }
  .flex-layout { display: flex; }
  .flex-grow { flex-grow: 1; }
  .bg-gray-900 { background-color: #111827; }
  .bg-gray-800 { background-color: #1f2937; }
  .bg-gray-700 { background-color: #374151; }
  .text-gray-100 { color: #f3f4f6; }
  .text-gray-300 { color: #d1d5db; }
  .text-gray-400 { color: #9ca3af; }
  .container { max-width: 1200px; margin: 0 auto; }
  .mx-auto { margin-left: auto; margin-right: auto; }
  .px-4 { padding-left: 1rem; padding-right: 1rem; }
  .py-8 { padding-top: 2rem; padding-bottom: 2rem; }
  .py-4 { padding-top: 1rem; padding-bottom: 1rem; }
  .px-6 { padding-left: 1.5rem; padding-right: 1.5rem; }
  .py-2 { padding-top: 0.5rem; padding-bottom: 0.5rem; }
  .mb-4 { margin-bottom: 1rem; }
  .mb-6 { margin-bottom: 1.5rem; }
  .mb-8 { margin-bottom: 2rem; }
  .mt-4 { margin-top: 1rem; }
  .flex { display: flex; }
  .items-center { align-items: center; }
  .justify-between { justify-content: space-between; }
  .gap-2 { gap: 0.5rem; }
  .gap-4 { gap: 1rem; }
  .grid { display: grid; }
  .grid-cols-1 { grid-template-columns: repeat(1, 1fr); }
  .text-xl { font-size: 1.25rem; }
  .text-2xl { font-size: 1.5rem; }
  .text-sm { font-size: 0.875rem; }
  .font-bold { font-weight: 700; }
  .font-semibold { font-weight: 600; }
  .rounded { border-radius: 0.25rem; }
  .rounded-lg { border-radius: 0.5rem; }
  .shadow { box-shadow: 0 1px 3px rgba(0,0,0,0.1); }
  .shadow-lg { box-shadow: 0 10px 15px rgba(0,0,0,0.1); }
  .w-full { width: 100%; }
  .border { border-width: 1px; }
  .border-gray-600 { border-color: #4b5563; }
  .bg-blue-600 { background-color: #2563eb; }
  .bg-blue-700 { background-color: #1d4ed8; }
  .bg-green-600 { background-color: #16a34a; }
  .bg-yellow-600 { background-color: #ca8a04; }
  .bg-red-600 { background-color: #dc2626; }
  .text-white { color: #ffffff; }
  .hover\\:bg-blue-700:hover { background-color: #1d4ed8; }
  .cursor-pointer { cursor: pointer; }
  .list-none { list-style: none; }
  a { color: #60a5fa; text-decoration: none; }
  a:hover { text-decoration: underline; }
  .status-badge { padding: 0.25rem 0.75rem; border-radius: 9999px; font-size: 0.75rem; font-weight: 600; }
  .status-pending { background-color: #4b5563; color: #f3f4f6; }
  .status-downloading { background-color: #2563eb; color: #ffffff; }
  .status-completed { background-color: #16a34a; color: #ffffff; }
  .status-failed { background-color: #dc2626; color: #ffffff; }
  .progress-bar { height: 0.5rem; background-color: #374151; border-radius: 9999px; overflow: hidden; }
  .progress-fill { height: 100%; background-color: #2563eb; transition: width 0.3s; }

  /* Sidebar Navigation */
  .nav-sidebar {
    width: 15%;
    background-color: #08080C;
    padding: 1.25rem 0;
    display: flex;
    flex-direction: column;
    border-right: 1px solid rgba(255, 0, 160, 0.1);
  }

  .nav-logo {
    font-size: 1.5rem;
    font-weight: 700;
    padding: 0 1.25rem 1.875rem;
    color: #f3f4f6;
  }

  .nav-item {
    padding: 0.75rem 1.25rem;
    display: flex;
    align-items: center;
    cursor: pointer;
    transition: background-color 0.2s;
    font-weight: 500;
    color: #f3f4f6;
    text-decoration: none;
  }

  .nav-item:hover {
    background-color: rgba(255, 0, 160, 0.1);
    text-decoration: none;
  }

  .nav-item.active {
    color: #FF00A0;
    background-color: rgba(255, 0, 160, 0.15);
    border-left: 3px solid #FF00A0;
  }

  .main-content {
    overflow-y: auto;
  }

  /* Neon Pink Download Card Styles */
  .download-card {
    background-color: #121215;
    padding: 20px;
    margin-bottom: 15px;
    border-left: 5px solid #FF00A0;
  }

  .download-card-queued {
    border-left-color: #AAAAAA;
  }

  .download-card-failed {
    border-left-color: #dc2626;
  }

  .download-card-completed {
    border-left-color: #16a34a;
  }

  .card-header {
    display: flex;
    justify-content: space-between;
    align-items: center;
  }

  .card-title {
    font-size: 1.4em;
    font-weight: bold;
  }

  .status-text-downloading {
    color: #FF00A0;
  }

  .status-text-queued {
    color: #AAAAAA;
  }

  .status-text-completed {
    color: #16a34a;
  }

  .status-text-failed {
    color: #dc2626;
  }

  .thick-progress-container {
    height: 25px;
    background-color: #08080C;
    border-radius: 4px;
    margin: 15px 0;
    position: relative;
  }

  .thick-progress-fill {
    height: 100%;
    background: linear-gradient(to right, #FF00A0, #8A00FF);
    border-radius: 4px;
    box-shadow: inset 0 0 10px rgba(255, 0, 160, 0.7);
    transition: width 0.3s;
  }

  .thick-progress-fill-gray {
    height: 100%;
    background: #222;
    border-radius: 4px;
  }

  .percentage-display {
    position: absolute;
    top: 50%;
    right: 20px;
    transform: translateY(-50%);
    font-size: 3em;
    font-weight: 900;
    color: #FF00A0;
    text-shadow: 0 0 10px #FF00A0;
  }

  .percentage-display-gray {
    color: #AAAAAA;
    text-shadow: none;
  }

  .speed-data {
    color: #00FF80;
    font-family: monospace;
    font-size: 1em;
    margin-top: 10px;
  }

  .speed-data-gray {
    color: #888;
  }

  /* Glean It Headline and Form */
  .card {
    background-color: #121215;
    padding: 30px;
    margin-bottom: 25px;
    border-radius: 4px;
    box-shadow: 0 0 10px rgba(255, 0, 160, 0.05);
  }

  .headline-section h1 {
    font-size: 3em;
    font-weight: 900;
    margin-bottom: 5px;
    color: #F0F0F0;
  }

  .glean-word {
    color: #FF00A0;
    text-shadow: 0 0 8px #FF00A0;
  }

  .url-input {
    width: 100%;
    padding: 15px 20px;
    font-size: 1.1em;
    background-color: #000000;
    color: #F0F0F0;
    border: 2px solid #222;
    border-radius: 4px;
    margin-top: 20px;
    transition: border-color 0.3s, box-shadow 0.3s;
  }

  .url-input:focus {
    outline: none;
    border-color: #FF00A0;
    box-shadow: 0 0 15px #FF00A0, 0 0 25px rgba(255, 0, 160, 0.5);
  }

  .cta-button {
    background-color: #FF00A0;
    color: #000000;
    font-weight: 900;
    font-size: 1.5em;
    padding: 15px 40px;
    border: none;
    cursor: pointer;
    text-transform: uppercase;
    border-radius: 0;
    margin-top: 20px;
    transition: background-color 0.2s, transform 0.1s;
    box-shadow: 0 0 15px #FF00A0;
  }

  .cta-button:hover {
    background-color: #FF33C7;
    transform: translateY(-2px);
  }

  .advanced-options {
    margin-top: 15px;
    font-size: 0.9em;
    color: #888;
  }

  .advanced-options summary {
    cursor: pointer;
    color: #FF00A0;
    font-weight: 500;
    padding: 8px 0;
    list-style: none;
  }

  .advanced-options summary::-webkit-details-marker {
    display: none;
  }

  .advanced-options summary::before {
    content: '▶ ';
    font-size: 0.8em;
  }

  .advanced-options[open] summary::before {
    content: '▼ ';
  }

  .advanced-options-content {
    padding: 15px;
    background: #0a0a0f;
    border: 1px solid #222;
    border-radius: 4px;
    margin-top: 10px;
  }

  .option-row {
    display: flex;
    justify-content: space-between;
    align-items: center;
    padding: 10px 0;
    border-bottom: 1px solid #1a1a1f;
  }

  .option-row:last-child {
    border-bottom: none;
  }

  .option-label {
    color: #ccc;
    font-weight: 500;
  }

  .option-select {
    padding: 8px 12px;
    background: #000;
    color: #fff;
    border: 1px solid #333;
    border-radius: 4px;
    min-width: 150px;
  }

  .option-select:focus {
    outline: none;
    border-color: #FF00A0;
  }

  .option-checkbox {
    width: 18px;
    height: 18px;
    accent-color: #FF00A0;
  }

  /* Settings Page Styles */
  .settings-section {
    background-color: #121215;
    padding: 25px;
    margin-bottom: 20px;
    border-radius: 4px;
    border-left: 3px solid #FF00A0;
  }

  .settings-section h3 {
    font-size: 1.3em;
    font-weight: 700;
    margin-bottom: 15px;
    color: #FF00A0;
  }

  .setting-row {
    display: flex;
    justify-content: space-between;
    align-items: center;
    padding: 12px 0;
    border-bottom: 1px solid #222;
  }

  .setting-row:last-child {
    border-bottom: none;
  }

  .setting-label {
    font-weight: 500;
    color: #F0F0F0;
  }

  .setting-description {
    font-size: 0.85em;
    color: #888;
    margin-top: 4px;
  }

  .setting-input {
    padding: 10px 15px;
    background-color: #000000;
    color: #F0F0F0;
    border: 2px solid #222;
    border-radius: 4px;
    font-size: 1em;
    min-width: 200px;
    transition: border-color 0.3s;
  }

  .setting-input:focus {
    outline: none;
    border-color: #FF00A0;
  }

  .setting-input:disabled {
    background-color: #0a0a0a;
    color: #666;
    cursor: not-allowed;
  }

  .setting-select {
    padding: 10px 15px;
    background-color: #000000;
    color: #F0F0F0;
    border: 2px solid #222;
    border-radius: 4px;
    font-size: 1em;
    min-width: 200px;
    cursor: pointer;
  }

  .setting-select:focus {
    outline: none;
    border-color: #FF00A0;
  }

  .theme-option {
    display: flex;
    align-items: center;
    gap: 10px;
    padding: 8px 12px;
    cursor: pointer;
    border-radius: 4px;
    transition: background-color 0.2s;
  }

  .theme-option:hover {
    background-color: rgba(255, 0, 160, 0.1);
  }

  .theme-swatch {
    width: 30px;
    height: 30px;
    border-radius: 4px;
    border: 2px solid #333;
  }

  .theme-swatch-neon {
    background: linear-gradient(135deg, #FF00A0, #8A00FF);
  }

  .theme-swatch-dark {
    background: linear-gradient(135deg, #333, #111);
  }

  .theme-swatch-ocean {
    background: linear-gradient(135deg, #0077B6, #00B4D8);
  }

  .settings-note {
    background-color: rgba(255, 0, 160, 0.1);
    padding: 15px;
    border-radius: 4px;
    font-size: 0.9em;
    color: #FF00A0;
    margin-top: 20px;
  }

  /* Download Directory Info Banner */
  .download-dir-banner {
    background-color: #1a1a1f;
    border: 1px solid #2a2a32;
    border-radius: 4px;
    padding: 12px 20px;
    margin-bottom: 20px;
    font-size: 0.9em;
    color: #999;
    display: flex;
    justify-content: space-between;
    align-items: center;
    gap: 16px;
  }

  .download-dir-banner .label {
    color: #666;
    font-weight: 500;
  }

  .download-dir-banner .path {
    color: #ccc;
    font-family: monospace;
    font-size: 0.95em;
  }

  .inline {
    display: inline;
  }
  "
}

/// Sidebar navigation component
fn sidebar() -> Element(a) {
  nav([class("nav-sidebar")], [
    div([class("nav-logo")], [text("GleanDown")]),
    a([href("/"), class("nav-item active")], [text("Glean It")]),
    a([href("/queue"), class("nav-item")], [text("Queue")]),
    a([href("/subscriptions"), class("nav-item")], [text("Subscriptions")]),
    a([href("/history"), class("nav-item")], [text("History")]),
    a([href("/settings"), class("nav-item")], [text("Settings")]),
  ])
}

/// Open folder button component
pub fn open_folder_button() -> Element(a) {
  form(
    [
      attribute("action", "/open-folder"),
      attribute("method", "POST"),
      class("inline"),
    ],
    [
      button(
        [
          type_("submit"),
          class("px-4 py-2 font-semibold text-sm rounded"),
          attribute(
            "style",
            "background-color: #FF00A0; color: #000000; box-shadow: 0 0 10px #FF00A0;",
          ),
        ],
        [text("Open Folder")],
      ),
    ],
  )
}

/// Download directory info banner with open folder button
pub fn download_dir_info(output_directory: String) -> Element(a) {
  div([class("download-dir-banner")], [
    div([class("flex items-center gap-2")], [
      span([class("label")], [text("Downloads saved to:")]),
      span([class("path")], [text(output_directory)]),
    ]),
    open_folder_button(),
  ])
}

/// Dashboard page content
pub fn dashboard_page(
  jobs: List(VideoJob),
  output_directory: String,
) -> Element(a) {
  div([], [
    download_dir_info(output_directory),
    glean_it_form(),
    job_list(jobs),
  ])
}

/// Queue page content - shows pending jobs only
pub fn queue_page(jobs: List(VideoJob)) -> Element(a) {
  div([], [
    h2([class("text-2xl font-bold mb-6")], [text("Download Queue")]),
    queue_list(jobs),
  ])
}

/// History page content - shows completed and failed jobs with filters
pub fn history_page(
  jobs: List(VideoJob),
  filter: String,
  output_directory: String,
) -> Element(a) {
  div([], [
    h2([class("text-2xl font-bold mb-6")], [text("Download History")]),
    download_dir_info(output_directory),
    filter_tabs(filter),
    history_list(jobs),
  ])
}

/// Glean It form with neon pink design
fn glean_it_form() -> Element(a) {
  div([class("card headline-section")], [
    h1([], [
      span([class("glean-word")], [text("GLEAN:")]),
      text(" ZERO-FRICTION VIDEO DELIVERY"),
    ]),
    form(
      [
        attribute("action", "/jobs"),
        attribute("method", "POST"),
      ],
      [
        input([
          type_("text"),
          name("url"),
          placeholder("Paste Video URL Here"),
          class("url-input"),
          attribute("required", ""),
        ]),
        advanced_options_section(),
        button(
          [
            type_("submit"),
            class("cta-button"),
          ],
          [text("DOWNLOAD NOW")],
        ),
      ],
    ),
  ])
}

/// Advanced options expandable section
pub fn advanced_options_section() -> Element(a) {
  details([class("advanced-options")], [
    summary([], [text("Advanced Options")]),
    div([class("advanced-options-content")], [
      // Quality selection
      div([class("option-row")], [
        label([class("option-label")], [text("Video Quality")]),
        select([name("quality"), class("option-select")], [
          option([attribute("value", "best")], "Best Available"),
          option([attribute("value", "1080")], "1080p"),
          option([attribute("value", "720")], "720p"),
          option([attribute("value", "480")], "480p"),
          option([attribute("value", "360")], "360p"),
        ]),
      ]),
      // Audio only option
      div([class("option-row")], [
        label([class("option-label")], [text("Audio Only")]),
        input([
          type_("checkbox"),
          name("audio_only"),
          class("option-checkbox"),
          attribute("value", "true"),
        ]),
      ]),
      // Playlist handling
      div([class("option-row")], [
        label([class("option-label")], [text("Download Playlist")]),
        select([name("playlist"), class("option-select")], [
          option([attribute("value", "single")], "Single Video Only"),
          option([attribute("value", "full")], "Full Playlist"),
        ]),
      ]),
    ]),
  ])
}

/// List of video jobs
fn job_list(jobs: List(VideoJob)) -> Element(a) {
  case list.is_empty(jobs) {
    True -> empty_state()
    False ->
      ul([class("list-none grid grid-cols-1 gap-4")], list.map(jobs, job_card))
  }
}

/// Empty state when no jobs exist
fn empty_state() -> Element(a) {
  div([class("bg-gray-800 rounded-lg shadow-lg px-6 py-8 text-center")], [
    p([class("text-gray-400 text-xl mb-4")], [text("No downloads yet")]),
    p([class("text-gray-400")], [
      text("Add a video URL above to start downloading"),
    ]),
  ])
}

/// List of pending jobs in queue
fn queue_list(jobs: List(VideoJob)) -> Element(a) {
  case list.is_empty(jobs) {
    True -> empty_queue_state()
    False ->
      ul([class("list-none grid grid-cols-1 gap-4")], list.map(jobs, job_card))
  }
}

/// Empty state when queue is empty
fn empty_queue_state() -> Element(a) {
  div([class("bg-gray-800 rounded-lg shadow-lg px-6 py-8 text-center")], [
    p([class("text-gray-400 text-xl mb-4")], [text("Queue is empty")]),
    p([class("text-gray-400")], [
      text("All pending downloads have been processed"),
    ]),
  ])
}

/// Filter tabs for history page
fn filter_tabs(current_filter: String) -> Element(a) {
  div([class("flex gap-2 mb-6")], [
    filter_tab("All", "all", current_filter),
    filter_tab("Completed", "completed", current_filter),
    filter_tab("Failed", "failed", current_filter),
  ])
}

/// Individual filter tab
fn filter_tab(
  label: String,
  filter_value: String,
  current_filter: String,
) -> Element(a) {
  let is_active = current_filter == filter_value
  let tab_class = case is_active {
    True -> "px-4 py-2 rounded bg-blue-600 text-white font-semibold"
    False -> "px-4 py-2 rounded bg-gray-700 text-gray-300 hover:bg-gray-600"
  }

  a([href("/history?filter=" <> filter_value), class(tab_class)], [text(label)])
}

/// List of history jobs
fn history_list(jobs: List(VideoJob)) -> Element(a) {
  case list.is_empty(jobs) {
    True -> empty_history_state()
    False ->
      ul(
        [class("list-none grid grid-cols-1 gap-4")],
        list.map(jobs, history_card),
      )
  }
}

/// Empty state when history is empty
fn empty_history_state() -> Element(a) {
  div([class("bg-gray-800 rounded-lg shadow-lg px-6 py-8 text-center")], [
    p([class("text-gray-400 text-xl mb-4")], [text("No download history")]),
    p([class("text-gray-400")], [
      text("Completed and failed downloads will appear here"),
    ]),
  ])
}

/// Individual history card component
fn history_card(job: VideoJob) -> Element(a) {
  li([class("bg-gray-800 rounded-lg shadow px-6 py-4")], [
    div([class("flex items-center justify-between mb-4")], [
      div([], [
        p([class("font-semibold")], [text(truncate_url(job.url, 60))]),
        p([class("text-sm text-gray-400")], [
          text("ID: " <> job_id_to_string(job.id)),
        ]),
        case job.path {
          option.Some(path) ->
            div([class("mt-2")], [
              p([class("text-sm text-gray-400")], [text("File: " <> path)]),
              show_in_folder_button(path),
            ])
          option.None -> div([], [])
        },
      ]),
      div([class("flex items-center gap-4")], [
        status_badge(job.status),
        case job.status {
          Failed(_) -> retry_button(job.url)
          _ -> div([], [])
        },
      ]),
    ]),
    case job.status {
      Failed(reason) ->
        p([class("text-sm text-red-400 mt-2")], [text("Error: " <> reason)])
      _ -> div([], [])
    },
  ])
}

/// Re-download button for failed jobs
fn retry_button(url: String) -> Element(a) {
  form(
    [attribute("action", "/jobs"), attribute("method", "POST"), class("inline")],
    [
      input([type_("hidden"), name("url"), attribute("value", url)]),
      button(
        [
          type_("submit"),
          class(
            "px-4 py-2 bg-yellow-600 text-white rounded text-sm font-semibold hover:bg-yellow-700",
          ),
        ],
        [text("Retry")],
      ),
    ],
  )
}

/// Show in folder button for completed downloads
pub fn show_in_folder_button(file_path: String) -> Element(a) {
  form(
    [
      attribute("action", "/show-in-folder"),
      attribute("method", "POST"),
      class("inline mt-2"),
    ],
    [
      input([type_("hidden"), name("path"), attribute("value", file_path)]),
      button(
        [
          type_("submit"),
          class(
            "px-3 py-1 bg-gray-700 text-gray-300 border border-gray-600 rounded text-sm font-semibold hover:bg-gray-600",
          ),
        ],
        [text("Show in Folder")],
      ),
    ],
  )
}

/// Individual job card component
fn job_card(job: VideoJob) -> Element(a) {
  li([class("bg-gray-800 rounded-lg shadow px-6 py-4")], [
    div([class("flex items-center justify-between mb-4")], [
      div([], [
        p([class("font-semibold")], [text(truncate_url(job.url, 60))]),
        p([class("text-sm text-gray-400")], [
          text("ID: " <> job_id_to_string(job.id)),
        ]),
      ]),
      status_badge(job.status),
    ]),
    progress_section(job.status),
  ])
}

/// Truncate a URL for display
fn truncate_url(url: String, max_len: Int) -> String {
  case max_len {
    n if n < 0 -> url
    _ -> {
      let len = string_length(url)
      case len > max_len {
        True -> string_slice(url, 0, max_len - 3) <> "..."
        False -> url
      }
    }
  }
}

@external(erlang, "string", "length")
fn string_length(s: String) -> Int

@external(erlang, "string", "slice")
fn string_slice(s: String, start: Int, length: Int) -> String

/// Status badge component
fn status_badge(status: VideoStatus) -> Element(a) {
  let #(status_text, badge_class) = case status {
    Pending -> #("Pending", "status-badge status-pending")
    Downloading(progress) -> #(
      "Downloading " <> int.to_string(progress) <> "%",
      "status-badge status-downloading",
    )
    Completed -> #("Completed", "status-badge status-completed")
    Failed(_reason) -> #("Failed", "status-badge status-failed")
  }

  span([class(badge_class)], [text(status_text)])
}

/// Progress bar section based on status
fn progress_section(status: VideoStatus) -> Element(a) {
  case status {
    Downloading(progress) ->
      div([class("progress-bar")], [
        div(
          [
            class("progress-fill"),
            attribute("style", "width: " <> int.to_string(progress) <> "%"),
          ],
          [],
        ),
      ])
    Failed(reason) ->
      p([class("text-sm text-red-400 mt-2")], [text("Error: " <> reason)])
    _ -> div([], [])
  }
}

/// Settings page content
pub fn settings_page(output_directory: String) -> Element(a) {
  div([], [
    h2([class("text-2xl font-bold mb-6")], [text("Settings")]),
    // Download Settings Section
    div([class("settings-section")], [
      html.h3([], [text("Download Settings")]),
      div([class("setting-row")], [
        div([], [
          div([class("setting-label")], [text("Output Directory")]),
          div([class("setting-description")], [
            text("Where downloaded videos will be saved"),
          ]),
        ]),
        input([
          type_("text"),
          class("setting-input"),
          attribute("value", output_directory),
          attribute("disabled", ""),
        ]),
      ]),
      div([class("setting-row")], [
        div([], [
          div([class("setting-label")], [text("Max Concurrent Downloads")]),
          div([class("setting-description")], [
            text("Number of simultaneous downloads allowed"),
          ]),
        ]),
        input([
          type_("number"),
          class("setting-input"),
          attribute("value", "3"),
          attribute("min", "1"),
          attribute("max", "10"),
          attribute("disabled", ""),
        ]),
      ]),
      div([class("setting-row")], [
        div([], [
          div([class("setting-label")], [text("Default Quality")]),
          div([class("setting-description")], [
            text("Preferred video quality for downloads"),
          ]),
        ]),
        html.select([class("setting-select"), attribute("disabled", "")], [
          html.option(
            [attribute("value", "best"), attribute("selected", "")],
            "Best Available",
          ),
          html.option([attribute("value", "1080p")], "1080p"),
          html.option([attribute("value", "720p")], "720p"),
          html.option([attribute("value", "480p")], "480p"),
          html.option([attribute("value", "audio")], "Audio Only"),
        ]),
      ]),
    ]),
    // Appearance Section
    div([class("settings-section")], [
      html.h3([], [text("Appearance")]),
      div([class("setting-row")], [
        div([], [
          div([class("setting-label")], [text("Theme")]),
          div([class("setting-description")], [text("Choose your color scheme")]),
        ]),
        div([], [
          div([class("theme-option")], [
            div([class("theme-swatch theme-swatch-neon")], []),
            span([], [text("Neon Pink (Active)")]),
          ]),
          div([class("theme-option")], [
            div([class("theme-swatch theme-swatch-dark")], []),
            span([], [text("Dark Mode")]),
          ]),
          div([class("theme-option")], [
            div([class("theme-swatch theme-swatch-ocean")], []),
            span([], [text("Ocean Blue")]),
          ]),
        ]),
      ]),
    ]),
    // System Info Section
    div([class("settings-section")], [
      html.h3([], [text("System Information")]),
      div([class("setting-row")], [
        div([class("setting-label")], [text("Version")]),
        span([class("text-gray-400")], [text("1.0.0")]),
      ]),
      div([class("setting-row")], [
        div([class("setting-label")], [text("yt-dlp Version")]),
        span([class("text-gray-400")], [text("2024.11.18")]),
      ]),
      div([class("setting-row")], [
        div([class("setting-label")], [text("Database")]),
        span([class("text-gray-400")], [text("SQLite 3.x")]),
      ]),
    ]),
    // Note about settings
    div([class("settings-note")], [
      text(
        "Settings are currently read-only. Configuration is managed via environment variables and the config file.",
      ),
    ]),
  ])
}

/// Error page for 404 and other errors
pub fn error_page(code: Int, message: String) -> Element(a) {
  div([class("text-center py-8")], [
    h2([class("text-2xl font-bold mb-4")], [
      text(int.to_string(code) <> " - " <> message),
    ]),
    p([class("text-gray-400 mb-6")], [text("Something went wrong.")]),
    a([href("/"), class("text-blue-400")], [text("← Back to Dashboard")]),
  ])
}

/// Neon Pink Download Card Component
/// Maps VideoJob status to display with neon pink styling
pub fn download_card(job: VideoJob) -> Element(a) {
  let #(card_class, status_text, status_class) = case job.status {
    Pending -> #(
      "download-card download-card-queued",
      "Queued",
      "status-text-queued",
    )
    Downloading(_) -> #(
      "download-card",
      "Downloading...",
      "status-text-downloading",
    )
    Completed -> #(
      "download-card download-card-completed",
      "Complete",
      "status-text-completed",
    )
    Failed(_) -> #(
      "download-card download-card-failed",
      "Failed",
      "status-text-failed",
    )
  }

  div([class(card_class)], [
    div([class("card-header")], [
      span([class("card-title")], [text(truncate_url(job.url, 50))]),
      span([class(status_class)], [text(status_text)]),
    ]),
    thick_progress_bar(job.status),
    speed_status_data(job.status),
  ])
}

/// Thick progress bar with oversized percentage display
fn thick_progress_bar(status: VideoStatus) -> Element(a) {
  case status {
    Downloading(progress) -> {
      let progress_str = int.to_string(progress)
      div([class("thick-progress-container")], [
        div(
          [
            class("thick-progress-fill"),
            attribute("style", "width: " <> progress_str <> "%"),
          ],
          [],
        ),
        span([class("percentage-display")], [text(progress_str <> "%")]),
      ])
    }
    Pending -> {
      div([class("thick-progress-container")], [
        div(
          [class("thick-progress-fill-gray"), attribute("style", "width: 0%")],
          [],
        ),
        span([class("percentage-display percentage-display-gray")], [text("0%")]),
      ])
    }
    Completed -> {
      div([class("thick-progress-container")], [
        div(
          [class("thick-progress-fill"), attribute("style", "width: 100%")],
          [],
        ),
        span([class("percentage-display")], [text("100%")]),
      ])
    }
    Failed(_) -> div([], [])
  }
}

/// Speed and status data in green monospace
fn speed_status_data(status: VideoStatus) -> Element(a) {
  case status {
    Downloading(progress) -> {
      let speed = "Speed: 25.4 MB/s"
      div([class("speed-data")], [
        text(
          "Status: Processing (H.264) | "
          <> speed
          <> " | "
          <> int.to_string(progress)
          <> "%",
        ),
      ])
    }
    Pending -> {
      div([class("speed-data speed-data-gray")], [
        text("Status: Pending Start | Estimated Size: Unknown"),
      ])
    }
    Completed -> {
      div([class("speed-data")], [
        text("Status: Complete | Downloaded Successfully"),
      ])
    }
    Failed(reason) -> {
      div([class("speed-data status-text-failed")], [
        text("Status: Failed | Error: " <> reason),
      ])
    }
  }
}

// =============================================================================
// Subscription Templates
// =============================================================================

/// Subscriptions page content
pub fn subscriptions_page(
  config: SubscriptionConfig,
  status: SubscriptionStatus,
  recent_videos: List(SeenVideo),
) -> Element(a) {
  div([], [
    h2([class("text-2xl font-bold mb-6")], [text("YouTube Subscriptions")]),
    // Status banner
    subscription_status_banner(status),
    // Settings form
    subscription_settings_form(config),
    // Recent discoveries
    subscription_feed_list(recent_videos),
  ])
}

/// Status banner showing current subscription state
fn subscription_status_banner(status: SubscriptionStatus) -> Element(a) {
  let status_text = case status.enabled, status.is_polling {
    True, True -> "Polling in progress..."
    True, False -> "Enabled - Auto-downloading subscriptions"
    False, _ -> "Disabled - Enable to start auto-downloading"
  }

  let status_color = case status.enabled {
    True -> "border-left-color: #16a34a;"
    False -> "border-left-color: #666;"
  }

  div(
    [
      class("settings-section"),
      attribute("style", status_color),
    ],
    [
      div([class("flex justify-between items-center")], [
        div([], [
          html.h3([], [text("Status")]),
          p([class("text-gray-400")], [text(status_text)]),
          case status.last_poll_at {
            Some(ts) ->
              p([class("text-sm text-gray-400 mt-2")], [
                text("Last poll: " <> format_timestamp(ts)),
              ])
            None -> div([], [])
          },
          case status.last_result {
            Some(result) -> poll_result_summary(result)
            None -> div([], [])
          },
        ]),
        // Manual poll button
        form(
          [
            attribute("action", "/subscriptions/poll"),
            attribute("method", "POST"),
          ],
          [
            button(
              [
                type_("submit"),
                class("cta-button"),
                attribute(
                  "style",
                  "font-size: 1em; padding: 10px 20px; margin-top: 0;",
                ),
              ],
              [text("Refresh Now")],
            ),
          ],
        ),
      ]),
    ],
  )
}

/// Poll result summary
fn poll_result_summary(result: PollResult) -> Element(a) {
  div([class("mt-2 text-sm")], [
    span([class("text-gray-400")], [
      text(
        "Last result: "
        <> int.to_string(result.total_found)
        <> " found, "
        <> int.to_string(result.queued_for_download)
        <> " queued, "
        <> int.to_string(result.skipped)
        <> " skipped",
      ),
    ]),
  ])
}

/// Subscription settings form
fn subscription_settings_form(config: SubscriptionConfig) -> Element(a) {
  form(
    [
      attribute("action", "/subscriptions/settings"),
      attribute("method", "POST"),
    ],
    [
      div([class("settings-section")], [
        html.h3([], [text("Subscription Settings")]),
        // Enable toggle
        div([class("setting-row")], [
          div([], [
            div([class("setting-label")], [text("Enable Auto-Download")]),
            div([class("setting-description")], [
              text("Automatically download new videos from your subscriptions"),
            ]),
          ]),
          html.label([class("toggle-switch")], [
            input([
              type_("checkbox"),
              name("enabled"),
              attribute("value", "true"),
              case config.enabled {
                True -> attribute("checked", "")
                False -> attribute("data-unchecked", "")
              },
            ]),
            span([class("toggle-slider")], []),
          ]),
        ]),
        // Poll interval
        div([class("setting-row")], [
          div([], [
            div([class("setting-label")], [text("Check Interval")]),
            div([class("setting-description")], [
              text("How often to check for new videos"),
            ]),
          ]),
          select([name("poll_interval_minutes"), class("setting-input")], [
            option(
              [
                attribute("value", "15"),
                selected_attr(config.poll_interval_minutes == 15),
              ],
              "Every 15 minutes",
            ),
            option(
              [
                attribute("value", "30"),
                selected_attr(config.poll_interval_minutes == 30),
              ],
              "Every 30 minutes",
            ),
            option(
              [
                attribute("value", "60"),
                selected_attr(config.poll_interval_minutes == 60),
              ],
              "Every hour",
            ),
            option(
              [
                attribute("value", "120"),
                selected_attr(config.poll_interval_minutes == 120),
              ],
              "Every 2 hours",
            ),
            option(
              [
                attribute("value", "360"),
                selected_attr(config.poll_interval_minutes == 360),
              ],
              "Every 6 hours",
            ),
            option(
              [
                attribute("value", "720"),
                selected_attr(config.poll_interval_minutes == 720),
              ],
              "Every 12 hours",
            ),
            option(
              [
                attribute("value", "1440"),
                selected_attr(config.poll_interval_minutes == 1440),
              ],
              "Once daily",
            ),
          ]),
        ]),
        // Browser for cookies
        div([class("setting-row")], [
          div([], [
            div([class("setting-label")], [text("Browser for Cookies")]),
            div([class("setting-description")], [
              text(
                "Browser to extract YouTube login cookies from (must be closed)",
              ),
            ]),
          ]),
          select([name("browser"), class("setting-input")], [
            option(
              [
                attribute("value", "firefox"),
                selected_attr(
                  subscription_types.browser_to_string(config.browser)
                  == "firefox",
                ),
              ],
              "Firefox",
            ),
            option(
              [
                attribute("value", "chrome"),
                selected_attr(
                  subscription_types.browser_to_string(config.browser)
                  == "chrome",
                ),
              ],
              "Chrome",
            ),
            option(
              [
                attribute("value", "chromium"),
                selected_attr(
                  subscription_types.browser_to_string(config.browser)
                  == "chromium",
                ),
              ],
              "Chromium",
            ),
            option(
              [
                attribute("value", "edge"),
                selected_attr(
                  subscription_types.browser_to_string(config.browser) == "edge",
                ),
              ],
              "Edge",
            ),
            option(
              [
                attribute("value", "brave"),
                selected_attr(
                  subscription_types.browser_to_string(config.browser)
                  == "brave",
                ),
              ],
              "Brave",
            ),
          ]),
        ]),
      ]),
      // Filters section
      div([class("settings-section")], [
        html.h3([], [text("Filters")]),
        // Max age
        div([class("setting-row")], [
          div([], [
            div([class("setting-label")], [text("Max Video Age (days)")]),
            div([class("setting-description")], [
              text("Only download videos published within this many days"),
            ]),
          ]),
          input([
            type_("number"),
            name("max_age_days"),
            class("setting-input"),
            attribute("value", int.to_string(config.max_age_days)),
            attribute("min", "1"),
            attribute("max", "30"),
          ]),
        ]),
        // Min duration
        div([class("setting-row")], [
          div([], [
            div([class("setting-label")], [text("Minimum Duration (seconds)")]),
            div([class("setting-description")], [
              text("Skip videos shorter than this (120s = 2min skips Shorts)"),
            ]),
          ]),
          input([
            type_("number"),
            name("min_duration_seconds"),
            class("setting-input"),
            attribute("value", int.to_string(config.min_duration_seconds)),
            attribute("min", "0"),
          ]),
        ]),
        // Max duration
        div([class("setting-row")], [
          div([], [
            div([class("setting-label")], [text("Maximum Duration (seconds)")]),
            div([class("setting-description")], [
              text("Skip videos longer than this (0 = no limit)"),
            ]),
          ]),
          input([
            type_("number"),
            name("max_duration_seconds"),
            class("setting-input"),
            attribute("value", case config.max_duration_seconds {
              Some(d) -> int.to_string(d)
              None -> ""
            }),
            attribute("min", "0"),
            placeholder("No limit"),
          ]),
        ]),
        // Keyword filter
        div([class("setting-row")], [
          div([], [
            div([class("setting-label")], [text("Include Keywords")]),
            div([class("setting-description")], [
              text(
                "Only download if title contains any of these (comma-separated, empty = all)",
              ),
            ]),
          ]),
          input([
            type_("text"),
            name("keyword_filter"),
            class("setting-input"),
            attribute("value", string.join(config.keyword_filter, ", ")),
            placeholder("e.g. tutorial, review, gameplay"),
          ]),
        ]),
        // Keyword exclude
        div([class("setting-row")], [
          div([], [
            div([class("setting-label")], [text("Exclude Keywords")]),
            div([class("setting-description")], [
              text(
                "Skip videos if title contains any of these (comma-separated)",
              ),
            ]),
          ]),
          input([
            type_("text"),
            name("keyword_exclude"),
            class("setting-input"),
            attribute("value", string.join(config.keyword_exclude, ", ")),
            placeholder("e.g. sponsored, ad, trailer"),
          ]),
        ]),
      ]),
      // Save button
      div([class("mt-4")], [
        button(
          [
            type_("submit"),
            class("cta-button"),
            attribute("style", "font-size: 1.1em; padding: 12px 30px;"),
          ],
          [text("Save Settings")],
        ),
      ]),
    ],
  )
}

/// Subscription feed list showing recent discoveries
pub fn subscription_feed_list(videos: List(SeenVideo)) -> Element(a) {
  div([class("settings-section mt-6")], [
    html.h3([], [text("Recent Discoveries")]),
    case list.is_empty(videos) {
      True ->
        p([class("text-gray-400")], [
          text(
            "No videos discovered yet. Enable subscriptions and click Refresh to check for new videos.",
          ),
        ])
      False -> ul([class("list-none")], list.map(videos, seen_video_card))
    },
  ])
}

/// Individual seen video card
fn seen_video_card(video: SeenVideo) -> Element(a) {
  let status_badge_content = case video.downloaded, video.skipped {
    True, _ ->
      span([class("status-badge status-completed")], [text("Downloaded")])
    _, True ->
      span([class("status-badge status-pending")], [
        text(option.unwrap(video.skip_reason, "Skipped")),
      ])
    _, _ -> span([class("status-badge status-downloading")], [text("Queued")])
  }

  let duration_text = case video.duration_seconds {
    Some(d) -> video_filter.format_duration(d)
    None -> "Unknown"
  }

  li([class("download-card mb-4")], [
    div([class("flex justify-between items-start")], [
      div([class("flex-grow")], [
        p([class("card-title")], [text(truncate_url(video.title, 60))]),
        p([class("text-sm text-gray-400 mt-1")], [
          text(option.unwrap(video.channel_name, "Unknown Channel")),
        ]),
        div([class("text-sm text-gray-500 mt-2")], [
          span([], [text("Duration: " <> duration_text)]),
          span([class("mx-2")], [text("|")]),
          a(
            [
              href(video.url),
              attribute("target", "_blank"),
              class("text-blue-400"),
            ],
            [text("View on YouTube")],
          ),
        ]),
      ]),
      status_badge_content,
    ]),
  ])
}

/// Helper to add selected attribute conditionally
fn selected_attr(is_selected: Bool) -> attribute.Attribute(a) {
  case is_selected {
    True -> attribute("selected", "")
    False -> attribute("data-not-selected", "")
  }
}

/// Format Unix timestamp to readable string
fn format_timestamp(ts: Int) -> String {
  // Simple formatting - just show relative time
  let now = get_timestamp()
  let age_seconds = now - ts

  case age_seconds {
    s if s < 60 -> "Just now"
    s if s < 3600 -> int.to_string(s / 60) <> " minutes ago"
    s if s < 86_400 -> int.to_string(s / 3600) <> " hours ago"
    s -> int.to_string(s / 86_400) <> " days ago"
  }
}

/// Get current Unix timestamp
@external(erlang, "os", "system_time")
fn get_timestamp() -> Int
