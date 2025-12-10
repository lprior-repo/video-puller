import gleeunit
import gleeunit/should
import web/handlers

pub fn main() {
  gleeunit.main()
}

// Test get_parent_directory function
pub fn get_parent_directory_simple_test() {
  handlers.get_parent_directory("/home/user/videos/file.mp4")
  |> should.equal("/home/user/videos")
}

pub fn get_parent_directory_root_test() {
  handlers.get_parent_directory("/file.mp4")
  |> should.equal("")
}

pub fn get_parent_directory_nested_test() {
  handlers.get_parent_directory(
    "/home/user/downloads/videos/subfolder/video.mp4",
  )
  |> should.equal("/home/user/downloads/videos/subfolder")
}

pub fn get_parent_directory_no_extension_test() {
  handlers.get_parent_directory("/home/user/videos/myfile")
  |> should.equal("/home/user/videos")
}

pub fn get_parent_directory_empty_test() {
  handlers.get_parent_directory("")
  |> should.equal(".")
}

pub fn get_parent_directory_single_component_test() {
  handlers.get_parent_directory("file.mp4")
  |> should.equal(".")
}

pub fn get_parent_directory_trailing_slash_test() {
  handlers.get_parent_directory("/home/user/videos/")
  |> should.equal("/home/user/videos")
}

pub fn get_parent_directory_multiple_extensions_test() {
  handlers.get_parent_directory("/home/user/videos/archive.tar.gz")
  |> should.equal("/home/user/videos")
}

// Test validate_video_url function
pub fn validate_video_url_https_test() {
  case handlers.validate_video_url("https://youtube.com/watch?v=123") {
    Ok(url) -> url |> should.equal("https://youtube.com/watch?v=123")
    Error(_) -> should.fail()
  }
}

pub fn validate_video_url_http_test() {
  case handlers.validate_video_url("http://example.com/video") {
    Ok(url) -> url |> should.equal("http://example.com/video")
    Error(_) -> should.fail()
  }
}

pub fn validate_video_url_no_protocol_test() {
  handlers.validate_video_url("youtube.com/watch?v=123")
  |> should.be_error()
}

pub fn validate_video_url_empty_test() {
  handlers.validate_video_url("")
  |> should.be_error()
}

pub fn validate_video_url_whitespace_test() {
  handlers.validate_video_url("   ")
  |> should.be_error()
}

pub fn validate_video_url_trim_whitespace_test() {
  case handlers.validate_video_url("  https://youtube.com/watch?v=123  ") {
    Ok(url) -> url |> should.equal("https://youtube.com/watch?v=123")
    Error(_) -> should.fail()
  }
}

pub fn validate_video_url_invalid_protocol_test() {
  handlers.validate_video_url("ftp://example.com/video")
  |> should.be_error()
}

// Test URL error messages
pub fn validate_video_url_empty_error_message_test() {
  case handlers.validate_video_url("") {
    Error(msg) -> msg |> should.equal("URL cannot be empty")
    Ok(_) -> should.fail()
  }
}

pub fn validate_video_url_no_protocol_error_message_test() {
  case handlers.validate_video_url("example.com") {
    Error(msg) -> msg |> should.equal("URL must start with http:// or https://")
    Ok(_) -> should.fail()
  }
}
