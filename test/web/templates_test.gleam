import gleam/string
import gleeunit
import gleeunit/should
import lustre/element
import web/templates

pub fn main() {
  gleeunit.main()
}

// Test download_dir_info rendering
pub fn download_dir_info_renders_path_test() {
  let output_dir = "/home/user/downloads"
  let html = templates.download_dir_info(output_dir)
  let html_string = element.to_string(html)

  // Should contain the path
  string.contains(html_string, "/home/user/downloads")
  |> should.be_true()
}

pub fn download_dir_info_contains_label_test() {
  let output_dir = "/var/videos"
  let html = templates.download_dir_info(output_dir)
  let html_string = element.to_string(html)

  // Should contain the label text
  string.contains(html_string, "Downloads saved to:")
  |> should.be_true()
}

pub fn download_dir_info_has_open_folder_button_test() {
  let output_dir = "/downloads"
  let html = templates.download_dir_info(output_dir)
  let html_string = element.to_string(html)

  // Should contain Open Folder button
  string.contains(html_string, "Open Folder")
  |> should.be_true()

  // Should have form pointing to /open-folder
  string.contains(html_string, "/open-folder")
  |> should.be_true()
}

pub fn download_dir_info_has_correct_styling_test() {
  let output_dir = "/downloads"
  let html = templates.download_dir_info(output_dir)
  let html_string = element.to_string(html)

  // Should have the banner styling class
  string.contains(html_string, "download-dir-banner")
  |> should.be_true()
}

// Test open_folder_button rendering
pub fn open_folder_button_has_form_test() {
  let html = templates.open_folder_button()
  let html_string = element.to_string(html)

  // Should be a POST form
  string.contains(html_string, "method=\"POST\"")
  |> should.be_true()

  string.contains(html_string, "action=\"/open-folder\"")
  |> should.be_true()
}

pub fn open_folder_button_has_submit_button_test() {
  let html = templates.open_folder_button()
  let html_string = element.to_string(html)

  // Should have a submit button
  string.contains(html_string, "type=\"submit\"")
  |> should.be_true()

  string.contains(html_string, "Open Folder")
  |> should.be_true()
}

pub fn open_folder_button_has_neon_styling_test() {
  let html = templates.open_folder_button()
  let html_string = element.to_string(html)

  // Should have neon pink color
  string.contains(html_string, "#FF00A0")
  |> should.be_true()
}

// Test show_in_folder_button rendering
pub fn show_in_folder_button_has_form_test() {
  let file_path = "/home/user/videos/video.mp4"
  let html = templates.show_in_folder_button(file_path)
  let html_string = element.to_string(html)

  // Should be a POST form
  string.contains(html_string, "method=\"POST\"")
  |> should.be_true()

  string.contains(html_string, "action=\"/show-in-folder\"")
  |> should.be_true()
}

pub fn show_in_folder_button_includes_path_test() {
  let file_path = "/home/user/videos/my-video.mp4"
  let html = templates.show_in_folder_button(file_path)
  let html_string = element.to_string(html)

  // Should include the file path as a hidden input
  string.contains(html_string, "type=\"hidden\"")
  |> should.be_true()

  string.contains(html_string, "name=\"path\"")
  |> should.be_true()

  string.contains(html_string, "/home/user/videos/my-video.mp4")
  |> should.be_true()
}

pub fn show_in_folder_button_has_button_text_test() {
  let file_path = "/videos/test.mp4"
  let html = templates.show_in_folder_button(file_path)
  let html_string = element.to_string(html)

  // Should have correct button text
  string.contains(html_string, "Show in Folder")
  |> should.be_true()
}

pub fn show_in_folder_button_has_styling_test() {
  let file_path = "/videos/test.mp4"
  let html = templates.show_in_folder_button(file_path)
  let html_string = element.to_string(html)

  // Should have gray styling (not neon pink)
  string.contains(html_string, "bg-gray-700")
  |> should.be_true()
}

// Test advanced_options_section rendering
pub fn advanced_options_section_has_details_element_test() {
  let html = templates.advanced_options_section()
  let html_string = element.to_string(html)

  // Should be a details element
  string.contains(html_string, "<details")
  |> should.be_true()

  string.contains(html_string, "<summary")
  |> should.be_true()

  string.contains(html_string, "Advanced Options")
  |> should.be_true()
}

pub fn advanced_options_section_has_quality_select_test() {
  let html = templates.advanced_options_section()
  let html_string = element.to_string(html)

  // Should have quality selection
  string.contains(html_string, "Video Quality")
  |> should.be_true()

  string.contains(html_string, "name=\"quality\"")
  |> should.be_true()

  // Should have quality options
  string.contains(html_string, "Best Available")
  |> should.be_true()

  string.contains(html_string, "1080p")
  |> should.be_true()

  string.contains(html_string, "720p")
  |> should.be_true()
}

pub fn advanced_options_section_has_audio_only_checkbox_test() {
  let html = templates.advanced_options_section()
  let html_string = element.to_string(html)

  // Should have audio only checkbox
  string.contains(html_string, "Audio Only")
  |> should.be_true()

  string.contains(html_string, "name=\"audio_only\"")
  |> should.be_true()

  string.contains(html_string, "type=\"checkbox\"")
  |> should.be_true()
}

pub fn advanced_options_section_has_playlist_option_test() {
  let html = templates.advanced_options_section()
  let html_string = element.to_string(html)

  // Should have playlist handling
  string.contains(html_string, "Download Playlist")
  |> should.be_true()

  string.contains(html_string, "name=\"playlist\"")
  |> should.be_true()

  string.contains(html_string, "Single Video Only")
  |> should.be_true()

  string.contains(html_string, "Full Playlist")
  |> should.be_true()
}

// Test dashboard_page integration
pub fn dashboard_page_includes_download_dir_test() {
  let jobs = []
  let output_dir = "/test/downloads"
  let html = templates.dashboard_page(jobs, output_dir)
  let html_string = element.to_string(html)

  // Should include the download directory info
  string.contains(html_string, "/test/downloads")
  |> should.be_true()

  string.contains(html_string, "Downloads saved to:")
  |> should.be_true()
}

pub fn dashboard_page_has_glean_it_form_test() {
  let jobs = []
  let output_dir = "/downloads"
  let html = templates.dashboard_page(jobs, output_dir)
  let html_string = element.to_string(html)

  // Should have the main form
  string.contains(html_string, "DOWNLOAD NOW")
  |> should.be_true()

  string.contains(html_string, "action=\"/jobs\"")
  |> should.be_true()
}

// Test history_page integration
pub fn history_page_includes_download_dir_test() {
  let jobs = []
  let filter = "all"
  let output_dir = "/home/videos"
  let html = templates.history_page(jobs, filter, output_dir)
  let html_string = element.to_string(html)

  // Should include the download directory info
  string.contains(html_string, "/home/videos")
  |> should.be_true()

  string.contains(html_string, "Downloads saved to:")
  |> should.be_true()
}

pub fn history_page_has_title_test() {
  let jobs = []
  let filter = "all"
  let output_dir = "/downloads"
  let html = templates.history_page(jobs, filter, output_dir)
  let html_string = element.to_string(html)

  // Should have history title
  string.contains(html_string, "Download History")
  |> should.be_true()
}

// Test settings_page integration
pub fn settings_page_shows_output_directory_test() {
  let output_dir = "/configured/downloads/path"
  let html = templates.settings_page(output_dir)
  let html_string = element.to_string(html)

  // Should display the output directory
  string.contains(html_string, "Output Directory")
  |> should.be_true()

  string.contains(html_string, "/configured/downloads/path")
  |> should.be_true()
}

pub fn settings_page_has_disabled_input_test() {
  let output_dir = "/downloads"
  let html = templates.settings_page(output_dir)
  let html_string = element.to_string(html)

  // The input should be disabled (read-only)
  // Lustre may render as disabled="" or just disabled
  string.contains(html_string, "disabled")
  |> should.be_true()
}
