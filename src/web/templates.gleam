/// HTML Templates using Lustre SSR
///
/// Server-side rendered HTML templates for the video puller UI.
/// Uses Lustre's element API for type-safe HTML generation.
import domain/core_types.{
  type VideoStatus, Completed, Downloading, Failed, Pending, job_id_to_string,
}
import domain/subscription_types.{
  type SeenVideo, type SubscriptionConfig, type SubscriptionStatus,
}
import domain/types.{type VideoJob}
import engine/subscription_progress.{type Snapshot}
import engine/subscription_settings.{type Settings}
import engine/ytdlp
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import lustre/attribute.{attribute, class, href, name, placeholder, type_}
import lustre/element.{type Element, text}
import lustre/element/html.{
  a, body, button, details, div, form, h1, h2, h4, head, html, img, input, label,
  li, link, main, meta, nav, option, p, script, select, span, style, summary,
  textarea, title, ul,
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
      sidebar(page_title),
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
    width: 230px;
    flex: 0 0 230px;
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

  @media (max-width: 768px) {
    .flex-layout { flex-direction: column; }
    .nav-sidebar { width: 100%; flex: 0 0 auto; flex-direction: row; flex-wrap: wrap; padding: 8px; }
    .nav-logo { width: 100%; padding: 4px 8px 10px; }
    .main-content { padding: 16px; }
    .setting-row { flex-direction: column; gap: 12px; }
    .setting-input { width: 100%; min-width: 0; }
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
fn sidebar(page_title: String) -> Element(a) {
  nav([class("nav-sidebar")], [
    div([class("nav-logo")], [text("GleanDown")]),
    sidebar_link("/download", "Download", "Download", page_title),
    sidebar_link("/queue", "Queue", "Queue", page_title),
    sidebar_link("/subscriptions", "Subscriptions", "Subscriptions", page_title),
    sidebar_link("/history", "History", "History", page_title),
    sidebar_link("/settings", "Settings", "Settings", page_title),
  ])
}

fn sidebar_link(
  path: String,
  name: String,
  label: String,
  page_title: String,
) -> Element(a) {
  let item_class = case page_title == name {
    True -> "nav-item active"
    False -> "nav-item"
  }
  a([href(path), class(item_class)], [text(label)])
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
  // Check if there are any downloading jobs to enable auto-refresh
  let has_downloading =
    list.any(jobs, fn(job) {
      case job.status {
        Downloading(_) -> True
        _ -> False
      }
    })

  div([], [
    h2([class("text-2xl font-bold mb-6")], [text("Download Queue")]),
    queue_list(jobs),
    // Auto-refresh script when there are active downloads
    case has_downloading {
      True -> script([], "setTimeout(function() { location.reload(); }, 3000);")
      False -> div([], [])
    },
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
    // Single URL form
    form(
      [
        attribute("action", "/jobs"),
        attribute("method", "POST"),
        class("download-form"),
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
    // Batch URL form
    batch_url_form(),
  ])
}

/// Batch URL upload form - one URL per line
fn batch_url_form() -> Element(a) {
  details([class("advanced-options"), attribute("style", "margin-top: 25px;")], [
    summary([], [text("Batch Download (Multiple URLs)")]),
    div([class("advanced-options-content")], [
      form(
        [
          attribute("action", "/jobs/batch"),
          attribute("method", "POST"),
        ],
        [
          html.textarea(
            [
              name("urls"),
              class("url-input"),
              attribute("rows", "8"),
              attribute("required", ""),
              placeholder(
                "Paste multiple URLs here, one per line:\nhttps://youtube.com/watch?v=abc123\nhttps://youtube.com/watch?v=xyz789\n...",
              ),
              attribute(
                "style",
                "width: 100%; resize: vertical; font-family: monospace;",
              ),
            ],
            "",
          ),
          p(
            [
              class("text-sm text-gray-400"),
              attribute("style", "margin-top: 10px;"),
            ],
            [
              text(
                "Each line should contain one video URL. Empty lines will be skipped.",
              ),
            ],
          ),
          button(
            [
              type_("submit"),
              class("cta-button"),
              attribute("style", "margin-top: 15px;"),
            ],
            [text("DOWNLOAD ALL")],
          ),
        ],
      ),
    ]),
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
    p([class("subscription-library-count")], [
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
      let len = string.length(url)
      case len > max_len {
        True -> string.slice(url, 0, max_len - 3) <> "..."
        False -> url
      }
    }
  }
}

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

/// Settings page content - now with editable download configuration
pub fn settings_page(
  config: ytdlp.DownloadConfig,
  ytdlp_version: String,
) -> Element(a) {
  div([], [
    h2([class("text-2xl font-bold mb-6")], [text("Download Settings")]),
    // Download Settings Form
    html.form([attribute("method", "POST"), attribute("action", "/settings")], [
      div([class("settings-section")], [
        html.h3([], [text("Output & Organization")]),
        // Output Directory
        div([class("setting-row")], [
          div([], [
            html.label(
              [class("setting-label"), attribute("for", "output_directory")],
              [
                text("Output Directory"),
              ],
            ),
            div([class("setting-description")], [
              text(
                "Effective download destination. Defaults to OUTPUT_DIR when configured.",
              ),
            ]),
          ]),
          input([
            type_("text"),
            class("setting-input"),
            attribute("name", "output_directory"),
            attribute("id", "output_directory"),
            attribute("value", config.output_directory),
          ]),
        ]),
        // Channel Folders Toggle
        div([class("setting-row")], [
          div([], [
            html.label(
              [class("setting-label"), attribute("for", "use_channel_folders")],
              [
                text("Organize by Channel"),
              ],
            ),
            div([class("setting-description")], [
              text("Create subfolders for each channel (Plex TV show style)"),
            ]),
          ]),
          div([class("toggle-container")], [
            input(
              list.flatten([
                [
                  type_("checkbox"),
                  class("toggle-checkbox"),
                  attribute("name", "use_channel_folders"),
                  attribute("id", "use_channel_folders"),
                  attribute("value", "true"),
                ],
                case config.use_channel_folders {
                  True -> [attribute("checked", "")]
                  False -> []
                },
              ]),
            ),
            html.label(
              [class("toggle-label"), attribute("for", "use_channel_folders")],
              [],
            ),
          ]),
        ]),
      ]),
      div([class("settings-section")], [
        html.h3([], [text("Quality & Format")]),
        // Format String
        div([class("setting-row")], [
          div([], [
            html.label([class("setting-label"), attribute("for", "format")], [
              text("Format Selection"),
            ]),
            div([class("setting-description")], [
              text("yt-dlp format string (e.g., 'best', 'bestvideo+bestaudio')"),
            ]),
          ]),
          input([
            type_("text"),
            class("setting-input"),
            attribute("name", "format"),
            attribute("id", "format"),
            attribute("value", config.format),
          ]),
        ]),
        // Max Filesize
        div([class("setting-row")], [
          div([], [
            html.label(
              [class("setting-label"), attribute("for", "max_filesize")],
              [
                text("Max File Size"),
              ],
            ),
            div([class("setting-description")], [
              text("Skip videos larger than this (e.g., '2G', '500M')"),
            ]),
          ]),
          input([
            type_("text"),
            class("setting-input"),
            attribute("name", "max_filesize"),
            attribute("id", "max_filesize"),
            attribute("value", config.max_filesize),
          ]),
        ]),
        // Audio Only Toggle
        div([class("setting-row")], [
          div([], [
            html.label(
              [class("setting-label"), attribute("for", "audio_only")],
              [
                text("Audio Only Mode"),
              ],
            ),
            div([class("setting-description")], [
              text("Extract audio only, no video"),
            ]),
          ]),
          div([class("toggle-container")], [
            input(
              list.flatten([
                [
                  type_("checkbox"),
                  class("toggle-checkbox"),
                  attribute("name", "audio_only"),
                  attribute("id", "audio_only"),
                  attribute("value", "true"),
                ],
                case config.audio_only {
                  True -> [attribute("checked", "")]
                  False -> []
                },
              ]),
            ),
            html.label(
              [class("toggle-label"), attribute("for", "audio_only")],
              [],
            ),
          ]),
        ]),
        // Audio Format
        div([class("setting-row")], [
          div([], [
            html.label(
              [class("setting-label"), attribute("for", "audio_format")],
              [
                text("Audio Format"),
              ],
            ),
            div([class("setting-description")], [
              text("Preferred audio format when extracting audio"),
            ]),
          ]),
          html.select(
            [
              class("setting-select"),
              attribute("name", "audio_format"),
              attribute("id", "audio_format"),
            ],
            [
              html.option(
                list.flatten([
                  [attribute("value", "best")],
                  case config.audio_format {
                    ytdlp.BestAudio -> [attribute("selected", "")]
                    _ -> []
                  },
                ]),
                "Best Available",
              ),
              html.option(
                list.flatten([
                  [attribute("value", "mp3")],
                  case config.audio_format {
                    ytdlp.MP3 -> [attribute("selected", "")]
                    _ -> []
                  },
                ]),
                "MP3",
              ),
              html.option(
                list.flatten([
                  [attribute("value", "aac")],
                  case config.audio_format {
                    ytdlp.AAC -> [attribute("selected", "")]
                    _ -> []
                  },
                ]),
                "AAC",
              ),
              html.option(
                list.flatten([
                  [attribute("value", "opus")],
                  case config.audio_format {
                    ytdlp.OPUS -> [attribute("selected", "")]
                    _ -> []
                  },
                ]),
                "Opus",
              ),
            ],
          ),
        ]),
        // Allow Playlist
        div([class("setting-row")], [
          div([], [
            html.label(
              [class("setting-label"), attribute("for", "allow_playlist")],
              [
                text("Allow Playlists"),
              ],
            ),
            div([class("setting-description")], [
              text("Download entire playlists when URL points to one"),
            ]),
          ]),
          div([class("toggle-container")], [
            input(
              list.flatten([
                [
                  type_("checkbox"),
                  class("toggle-checkbox"),
                  attribute("name", "allow_playlist"),
                  attribute("id", "allow_playlist"),
                  attribute("value", "true"),
                ],
                case config.allow_playlist {
                  True -> [attribute("checked", "")]
                  False -> []
                },
              ]),
            ),
            html.label(
              [class("toggle-label"), attribute("for", "allow_playlist")],
              [],
            ),
          ]),
        ]),
      ]),
      div([class("settings-section")], [
        html.h3([], [text("Performance & Rate Limiting")]),
        // Download Timeout
        div([class("setting-row")], [
          div([], [
            html.label(
              [
                class("setting-label"),
                attribute("for", "download_timeout_minutes"),
              ],
              [
                text("Download Timeout"),
              ],
            ),
            div([class("setting-description")], [
              text("Maximum time per download in minutes"),
            ]),
          ]),
          input([
            type_("number"),
            class("setting-input"),
            attribute("name", "download_timeout_minutes"),
            attribute("id", "download_timeout_minutes"),
            attribute(
              "value",
              int.to_string(config.download_timeout_ms / 60_000),
            ),
            attribute("min", "1"),
            attribute("max", "240"),
          ]),
        ]),
        // Rate Limit Delay
        div([class("setting-row")], [
          div([], [
            html.label(
              [class("setting-label"), attribute("for", "rate_limit_delay_ms")],
              [
                text("Rate Limit Delay"),
              ],
            ),
            div([class("setting-description")], [
              text(
                "Milliseconds to wait between starting downloads (prevents rate limiting)",
              ),
            ]),
          ]),
          input([
            type_("number"),
            class("setting-input"),
            attribute("name", "rate_limit_delay_ms"),
            attribute("id", "rate_limit_delay_ms"),
            attribute("value", int.to_string(config.rate_limit_delay_ms)),
            attribute("min", "0"),
            attribute("max", "10000"),
          ]),
        ]),
        // Bandwidth Limit
        div([class("setting-row")], [
          div([], [
            html.label(
              [class("setting-label"), attribute("for", "bandwidth_limit")],
              [
                text("Bandwidth Limit"),
              ],
            ),
            div([class("setting-description")], [
              text(
                "Per-download bandwidth limit (e.g., '5M' for 5MB/s, empty for unlimited)",
              ),
            ]),
          ]),
          input([
            type_("text"),
            class("setting-input"),
            attribute("name", "bandwidth_limit"),
            attribute("id", "bandwidth_limit"),
            attribute("value", config.bandwidth_limit),
            attribute("placeholder", "Unlimited"),
          ]),
        ]),
      ]),
      // Submit button
      div([class("settings-actions")], [
        html.button([type_("submit"), class("btn btn-primary")], [
          text("Save Settings"),
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
        span([class("text-gray-400")], [text(ytdlp_version)]),
      ]),
      div([class("setting-row")], [
        div([class("setting-label")], [text("Database")]),
        span([class("text-gray-400")], [text("SQLite 3.x")]),
      ]),
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

// =============================================================================
// Subscription Templates
// =============================================================================

/// Metadata used to render one YouTube-style card. Optional source fields stay
/// absent when the database or cached metadata does not contain them.
pub type FeedCard {
  FeedCard(
    video: SeenVideo,
    plex_url: Option(String),
    description: Option(String),
    view_count: Option(Int),
  )
}

/// Subscriptions page content
pub fn subscriptions_page(
  config: SubscriptionConfig,
  status: SubscriptionStatus,
  recent_videos: List(FeedCard),
  library_dir: String,
  settings: Settings,
  ytdlp_version: String,
  plex_url: Option(String),
  progress: List(Snapshot),
  library_count: Int,
) -> Element(a) {
  div([], [
    div([class("subscriptions-title-row")], [
      h2([class("text-2xl font-bold mb-4")], [text("YouTube Subscriptions")]),
      a(
        [
          href("#subscription-settings"),
          class("settings-jump-link"),
          attribute(
            "onclick",
            "document.getElementById('subscription-controls').open = true",
          ),
        ],
        [text("Settings")],
      ),
    ]),
    details(
      [
        class("subscription-controls"),
        attribute("id", "subscription-controls"),
      ],
      [
        summary([], [text("Settings and channels")]),
        div([class("subscription-controls-body")], [
          div([class("settings-section")], [
            html.h3([], [text("Library")]),
            p([class("text-gray-400")], [
              text("Downloaded videos are written by ytdl-sub to: "),
              span([class("path")], [text(library_dir)]),
            ]),
            p([class("text-sm text-gray-400 mt-2")], [
              text(
                "Subscription channels are listed in channels.txt beside that folder - one public YouTube channel, playlist or video URL per line, optionally as \"Label = URL\" to name the Plex show. Drop a Google Takeout subscriptions.csv there to import channels without signing in. The upload age limit applies to both poll passes; 0 allows all available history. Independent channels run in parallel up to the configured worker count. No browser cookies are used.",
              ),
            ]),
            channel_add_form(),
            channel_import_form(),
            case plex_url {
              Some(url) ->
                p([class("text-sm mt-2")], [
                  a(
                    [
                      href(url),
                      attribute("target", "_blank"),
                      attribute("rel", "noopener noreferrer"),
                    ],
                    [text("Open YouTube library in Plex")],
                  ),
                ])
              None -> div([], [])
            },
          ]),
          subscription_settings_form(config, settings),
        ]),
      ],
    ),
    // Status and feed refresh independently so unsaved controls survive polling.
    subscription_live_fragment(status, recent_videos, progress, library_count),
    div([class("watched-cleanup-note")], [
      p([], [
        text(
          "Settings: "
          <> int.to_string(settings.days)
          <> " day upload window (0 = unlimited) · "
          <> int.to_string(settings.workers)
          <> " workers · restricted videos "
          <> case settings.skip_restricted {
            True -> "skipped"
            False -> "included"
          }
          <> " · yt-dlp "
          <> ytdlp_version,
        ),
      ]),
      p([], [
        text(
          "Watched-video cleanup runs from the configured Plex library during reconciliation. Deleted YouTube IDs are kept in the durable archive so those videos are not downloaded again.",
        ),
      ]),
    ]),
    script([], subscription_live_refresh_script()),
  ])
}

fn channel_import_form() -> Element(a) {
  form(
    [
      class("channel-import-form"),
      attribute("action", "/subscriptions/channels"),
      attribute("method", "POST"),
    ],
    [
      html.h3([], [text("Sync desired channel list")]),
      p([class("text-sm text-gray-400")], [
        text(
          "Paste the complete channel list you want to keep, using copied YouTube subscriptions or lines such as Label = https://www.youtube.com/@handle. This replaces the desired list and triggers reconciliation immediately; removed channels are stopped before the replacement poll and their library data is retired by the app.",
        ),
      ]),
      textarea(
        [
          name("channel_text"),
          attribute("rows", "7"),
          attribute("required", ""),
          placeholder(
            "Channel name\n@channelhandle•12.3K subscribers\nSubscribed",
          ),
          attribute("aria-label", "Channel list to import"),
        ],
        "",
      ),
      button([type_("submit")], [text("Sync channel list")]),
    ],
  )
}

fn channel_add_form() -> Element(a) {
  form(
    [
      class("channel-add-form"),
      attribute("action", "/subscriptions/channels/add"),
      attribute("method", "POST"),
    ],
    [
      html.h3([], [text("Add one channel")]),
      p([class("text-sm text-gray-400")], [
        text(
          "Add a single channel, playlist, or video URL to the desired list. This addition remains when you add more channels later.",
        ),
      ]),
      input([
        type_("url"),
        name("channel_url"),
        placeholder("https://www.youtube.com/@channel"),
        attribute("required", ""),
        attribute("aria-label", "YouTube URL"),
      ]),
      input([
        type_("text"),
        name("channel_label"),
        placeholder("Optional display name"),
        attribute("aria-label", "Optional channel display name"),
      ]),
      button([type_("submit")], [text("Add channel")]),
    ],
  )
}

pub fn channel_import_result(
  added: Int,
  removed: Int,
  found: Int,
) -> Element(a) {
  div([class("settings-section")], [
    html.h2([], [text("Channel import complete")]),
    p([], [
      text(
        "Desired list saved: "
        <> int.to_string(found)
        <> " channel(s), with "
        <> int.to_string(added)
        <> " new and "
        <> int.to_string(removed)
        <> " removed. Reconciliation starts immediately, stopping old-channel work before the replacement poll; removed channels are retired by the app.",
      ),
    ]),
    p([class("mt-4")], [
      a([href("/subscriptions")], [text("Return to subscriptions")]),
    ]),
  ])
}

fn subscription_live_refresh_script() -> String {
  "
  (function(){
    const live = document.getElementById('subscription-live');
    if (!live) return;
    const refresh = async () => {
      try {
        const response = await fetch('/subscriptions/live', { headers: { 'Accept': 'text/html' }, cache: 'no-store' });
        if (!response.ok) return;
        const detailStates = Array.from(live.querySelectorAll('[data-live-details]'))
          .map((item) => [item.getAttribute('data-live-details'), item.open]);
        const descriptionStates = Array.from(live.querySelectorAll('[data-feed-details]'))
          .map((item) => [item.getAttribute('data-feed-details'), item.open]);
        const html = await response.text();
        const fragment = new DOMParser().parseFromString(html, 'text/html');
        const fresh = fragment.getElementById('subscription-live');
        if (fresh) {
          live.innerHTML = fresh.innerHTML;
          for (const [key, isOpen] of detailStates) {
            const item = Array.from(live.querySelectorAll('[data-live-details]'))
              .find((element) => element.getAttribute('data-live-details') === key);
            if (item) item.open = isOpen;
          }
          for (const [key, isOpen] of descriptionStates) {
            const item = Array.from(live.querySelectorAll('[data-feed-details]'))
              .find((element) => element.getAttribute('data-feed-details') === key);
            if (item) item.open = isOpen;
          }
        }
      } catch (_) {}
    };
    window.setInterval(refresh, 4000);
  })();
  "
}

pub fn subscription_live_fragment(
  status: SubscriptionStatus,
  recent_videos: List(FeedCard),
  progress: List(Snapshot),
  library_count: Int,
) -> Element(a) {
  div([attribute("id", "subscription-live")], [
    subscription_status_banner(status),
    p([class("text-gray-400")], [
      text(
        int.to_string(library_count)
        <> " completed videos in your library. Showing the latest "
        <> int.to_string(list.length(recent_videos))
        <> " below.",
      ),
    ]),
    subscription_progress_summary(progress),
    subscription_feed_list(recent_videos),
  ])
}

fn subscription_progress_summary(progress: List(Snapshot)) -> Element(a) {
  case list.is_empty(progress) {
    True -> div([], [])
    False -> {
      let attrs = [
        class("poll-progress"),
        attribute("data-live-details", "poll-progress"),
        attribute("data-default-open", "false"),
      ]
      details(attrs, [
        summary([class("poll-progress-summary")], [
          span([class("font-semibold")], [text("Live Poll Progress")]),
          span([class("text-sm text-gray-400")], [
            text(progress_overview(progress)),
          ]),
        ]),
        phase_progress("Recent uploads", "recent", progress),
        phase_progress("Backfill check", "backfill", progress),
      ])
    }
  }
}

fn progress_overview(progress: List(Snapshot)) -> String {
  let active =
    list.filter(progress, fn(snapshot) { snapshot.status == "running" })
  let queued =
    list.filter(progress, fn(snapshot) { snapshot.status == "queued" })
  int.to_string(list.length(active))
  <> " active · "
  <> int.to_string(list.length(queued))
  <> " queued"
}

fn phase_progress(
  label: String,
  phase: String,
  progress: List(Snapshot),
) -> Element(a) {
  let snapshots =
    list.filter(progress, fn(snapshot) { snapshot.phase == phase })
  case list.is_empty(snapshots) {
    True -> div([], [])
    False -> {
      let total = case list.first(snapshots) {
        Ok(snapshot) -> snapshot.phase_total
        Error(_) -> 0
      }
      let active =
        list.filter(snapshots, fn(snapshot) { snapshot.status == "running" })
      let queued =
        list.filter(snapshots, fn(snapshot) { snapshot.status == "queued" })
      let finished =
        list.filter(snapshots, fn(snapshot) {
          snapshot.status == "completed"
          || snapshot.status == "failed"
          || snapshot.status == "stale"
        })
      let failed =
        list.filter(snapshots, fn(snapshot) {
          snapshot.status == "failed" || snapshot.status == "stale"
        })
      let failed_count = list.length(failed)
      let percent = case total > 0 {
        True -> int.clamp(list.length(finished) * 100 / total, 0, 100)
        False -> 0
      }
      div([class("poll-phase")], [
        div([class("poll-phase-heading")], [
          span([class("font-semibold")], [text(label)]),
          span([class("text-sm text-gray-400")], [
            text(
              int.to_string(list.length(finished))
              <> " / "
              <> int.to_string(total)
              <> " finished · "
              <> int.to_string(list.length(active))
              <> " active"
              <> " · "
              <> int.to_string(list.length(queued))
              <> " queued"
              <> case failed_count > 0 {
                True -> " · " <> int.to_string(failed_count) <> " failed/stale"
                False -> ""
              },
            ),
          ]),
        ]),
        div([class("progress-bar")], [
          div(
            [
              class("progress-fill"),
              attribute("style", "width: " <> int.to_string(percent) <> "%"),
            ],
            [],
          ),
        ]),
        case list.is_empty(active) {
          True -> div([], [])
          False ->
            ul(
              [class("poll-active-list")],
              list.map(active, active_channel_row),
            )
        },
      ])
    }
  }
}

fn active_channel_row(snapshot: Snapshot) -> Element(a) {
  let percent_text = case snapshot.percent {
    Some(value) -> " · " <> int.to_string(value) <> "%"
    None -> ""
  }
  li([class("poll-active-item")], [
    div([class("poll-active-heading")], [
      span([class("font-semibold")], [text(snapshot.channel)]),
      span([class("text-sm text-gray-400")], [
        text(snapshot.message <> percent_text),
      ]),
    ]),
    case snapshot.percent {
      Some(value) ->
        div([class("progress-bar")], [
          div(
            [
              class("progress-fill"),
              attribute(
                "style",
                "width: " <> int.to_string(int.clamp(value, 0, 100)) <> "%",
              ),
            ],
            [],
          ),
        ])
      None -> div([], [])
    },
  ])
}

/// Status banner showing current subscription state
fn subscription_status_banner(status: SubscriptionStatus) -> Element(a) {
  let status_text = case status.enabled, status.is_polling {
    True, True -> "Subscription downloads are running"
    True, False -> "Automatic downloads are enabled"
    False, _ -> "Automatic downloads are paused"
  }
  let status_class = case status.enabled, status.is_polling {
    True, True -> "subscription-status-dot is-running"
    True, False -> "subscription-status-dot is-enabled"
    False, _ -> "subscription-status-dot is-paused"
  }
  div([class("subscription-status-panel")], [
    div([class("subscription-status-copy")], [
      span([class(status_class)], []),
      div([], [
        p([class("subscription-status-title")], [text(status_text)]),
        p([class("subscription-status-meta")], [
          case status.last_poll_at {
            Some(ts) -> text("Last checked " <> format_timestamp(ts))
            None -> text("No poll has run yet")
          },
          case status.last_result {
            Some(result) ->
              case result.errors != [] {
                True ->
                  text(
                    " · "
                    <> int.to_string(list.length(result.errors))
                    <> " errors",
                  )
                False -> text("")
              }
            None -> text("")
          },
        ]),
      ]),
    ]),
    form(
      [
        attribute("action", "/subscriptions/poll"),
        attribute("method", "POST"),
        class("status-refresh-form"),
      ],
      [
        button([type_("submit"), class("subscription-refresh-button")], [
          text("Refresh now"),
        ]),
      ],
    ),
  ])
}

/// Subscription settings form
fn subscription_settings_form(
  config: SubscriptionConfig,
  settings: Settings,
) -> Element(a) {
  form(
    [
      attribute("id", "subscription-settings"),
      attribute("action", "/subscriptions/settings"),
      attribute("method", "POST"),
    ],
    [
      div([class("settings-section")], [
        html.h3([], [text("Subscription Settings")]),
        div([class("setting-row")], [
          div([], [
            div([class("setting-label")], [
              text("Maximum upload age (days)"),
            ]),
            div([class("setting-description")], [
              text(
                "Limits both poll passes to uploads this recent. Set to 0 for unlimited history.",
              ),
            ]),
          ]),
          input([
            type_("number"),
            class("setting-input"),
            name("days"),
            attribute("min", "0"),
            attribute("max", "365"),
            attribute("value", int.to_string(settings.days)),
          ]),
        ]),
        div([class("setting-row")], [
          div([], [
            div([class("setting-label")], [text("Subscription workers")]),
            div([class("setting-description")], [
              text("Maximum simultaneous download workers"),
            ]),
          ]),
          input([
            type_("number"),
            class("setting-input"),
            name("workers"),
            attribute("min", "1"),
            attribute("max", "16"),
            attribute("value", int.to_string(settings.workers)),
          ]),
        ]),
        div([class("setting-row")], [
          div([], [
            div([class("setting-label")], [text("Skip restricted videos")]),
            div([class("setting-description")], [
              text(
                "Skip premium-only and channel-members-only uploads. Videos without availability metadata still pass.",
              ),
            ]),
          ]),
          html.label([class("toggle-switch")], [
            input([
              type_("checkbox"),
              name("skip_restricted"),
              attribute("value", "true"),
              case settings.skip_restricted {
                True -> attribute("checked", "")
                False -> attribute("data-unchecked", "")
              },
            ]),
            span([class("toggle-slider")], []),
          ]),
        ]),
        // Enable toggle
        div([class("setting-row")], [
          div([], [
            div([class("setting-label")], [text("Enable Auto-Download")]),
            div([class("setting-description")], [
              text(
                "Run the ytdl-sub engine over the configured channels and download new videos",
              ),
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

/// Subscription feed list showing recent downloads
pub fn subscription_feed_list(videos: List(FeedCard)) -> Element(a) {
  div([class("feed-section mt-6")], [
    div([class("feed-section-heading")], [
      html.h3([], [text("Latest Videos")]),
      span([class("text-sm text-gray-400")], [text("Newest published first")]),
    ]),
    case list.is_empty(videos) {
      True ->
        p([class("text-gray-400")], [
          text(
            "No downloads recorded yet. Enable subscriptions and click Refresh Now to pull the channel list.",
          ),
        ])
      False ->
        ul([class("feed-grid list-none")], list.map(videos, seen_video_card))
    },
  ])
}

/// Individual title-first video card in the YouTube-style grid.
fn seen_video_card(card: FeedCard) -> Element(a) {
  let FeedCard(video, plex_url, description, view_count) = card
  let source_url = video_source_url(video)
  let primary_url = case plex_url {
    Some(url) -> Some(url)
    None -> source_url
  }
  let thumbnail = case video.thumbnail_url {
    Some(url) ->
      img([
        attribute("src", url),
        attribute("alt", "Thumbnail for " <> video.title),
        class("feed-thumbnail-image"),
        attribute("loading", "lazy"),
      ])
    None -> div([class("feed-thumbnail-fallback")], [text("No thumbnail")])
  }
  let thumbnail_frame =
    div([class("feed-thumbnail-frame")], [
      thumbnail,
      case video.duration_seconds {
        Some(seconds) ->
          span([class("feed-duration")], [text(format_duration(seconds))])
        None -> div([], [])
      },
    ])
  let status_text = case video.downloaded, video.skipped {
    True, _ -> "Downloaded"
    _, True -> option.unwrap(video.skip_reason, "Skipped")
    _, _ -> "Queued"
  }

  li([class("feed-card")], [
    case primary_url {
      Some(url) ->
        a(
          [
            href(url),
            class("feed-primary-link"),
            attribute("target", "_blank"),
            attribute("rel", "noopener noreferrer"),
            attribute("aria-label", "Open " <> video.title),
          ],
          [thumbnail_frame],
        )
      None -> thumbnail_frame
    },
    div([class("feed-card-body")], [
      h4([class("feed-title")], [
        case primary_url {
          Some(url) ->
            a(
              [
                href(url),
                class("feed-title-link"),
                attribute("target", "_blank"),
                attribute("rel", "noopener noreferrer"),
              ],
              [text(video.title)],
            )
          None -> text(video.title)
        },
      ]),
      p([class("feed-channel")], [
        text(option.unwrap(video.channel_name, "Unknown Channel")),
      ]),
      div([class("feed-meta")], [
        case view_count {
          Some(count) if count >= 0 ->
            span([], [text(format_view_count(count) <> " views")])
          _ -> div([], [])
        },
        case video.published_at {
          Some(timestamp) -> span([], [text(format_timestamp(timestamp))])
          None -> div([], [])
        },
      ]),
      case description {
        Some(body) ->
          case string.trim(body) {
            "" -> div([], [])
            _ ->
              details(
                [
                  class("feed-description"),
                  attribute("data-feed-details", video.video_id),
                ],
                [summary([], [text("Description")]), p([], [text(body)])],
              )
          }
        _ -> div([], [])
      },
      div([class("feed-card-actions")], [
        case plex_url {
          Some(_) ->
            case source_url {
              Some(url) ->
                a(
                  [
                    href(url),
                    class("feed-source-link"),
                    attribute("target", "_blank"),
                    attribute("rel", "noopener noreferrer"),
                  ],
                  [text("YouTube ↗")],
                )
              None -> div([], [])
            }
          None -> div([], [])
        },
        span([class("feed-status")], [text(status_text)]),
      ]),
    ]),
  ])
}

fn video_source_url(video: SeenVideo) -> Option(String) {
  case string.starts_with(video.url, "http") {
    True -> Some(video.url)
    False ->
      case string.length(video.video_id) == 11 {
        True -> Some("https://www.youtube.com/watch?v=" <> video.video_id)
        False -> None
      }
  }
}

fn format_view_count(count: Int) -> String {
  int.to_string(count)
}

/// Format duration as human-readable string
fn format_duration(seconds: Int) -> String {
  let hours = seconds / 3600
  let remaining = seconds % 3600
  let minutes = remaining / 60
  let secs = remaining % 60

  case hours {
    h if h > 0 -> int.to_string(h) <> ":" <> pad2(minutes) <> ":" <> pad2(secs)
    _ -> int.to_string(minutes) <> ":" <> pad2(secs)
  }
}

fn pad2(n: Int) -> String {
  case n < 10 {
    True -> "0" <> int.to_string(n)
    False -> int.to_string(n)
  }
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
type TimeUnit {
  Second
}

@external(erlang, "erlang", "system_time")
fn get_system_time_seconds(unit: TimeUnit) -> Int

fn get_timestamp() -> Int {
  get_system_time_seconds(Second)
}
