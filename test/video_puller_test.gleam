import gleeunit
import gleeunit/should
import video_puller

pub fn main() -> Nil {
  gleeunit.main()
}

// URL validation tests
pub fn validate_url_with_https_test() {
  video_puller.validate_url("https://example.com")
  |> should.be_ok()
  |> should.equal("https://example.com")
}

pub fn validate_url_with_http_test() {
  video_puller.validate_url("http://example.com")
  |> should.be_ok()
  |> should.equal("http://example.com")
}

pub fn validate_url_empty_string_test() {
  video_puller.validate_url("")
  |> should.be_error()
  |> should.equal("URL cannot be empty")
}

pub fn validate_url_no_protocol_test() {
  video_puller.validate_url("example.com")
  |> should.be_error()
  |> should.equal("URL must start with http:// or https://")
}

pub fn validate_url_with_path_test() {
  video_puller.validate_url("https://example.com/path/to/video")
  |> should.be_ok()
  |> should.equal("https://example.com/path/to/video")
}

// Domain extraction tests
pub fn extract_domain_https_test() {
  video_puller.extract_domain("https://example.com/path")
  |> should.be_ok()
  |> should.equal("example.com")
}

pub fn extract_domain_http_test() {
  video_puller.extract_domain("http://example.com/path")
  |> should.be_ok()
  |> should.equal("example.com")
}

pub fn extract_domain_no_path_test() {
  video_puller.extract_domain("https://example.com")
  |> should.be_ok()
  |> should.equal("example.com")
}

pub fn extract_domain_invalid_url_test() {
  video_puller.extract_domain("not-a-url")
  |> should.be_error()
}

pub fn extract_domain_subdomain_test() {
  video_puller.extract_domain("https://api.example.com/v1/endpoint")
  |> should.be_ok()
  |> should.equal("api.example.com")
}

// Filter valid URLs tests
pub fn filter_valid_urls_all_valid_test() {
  let urls = [
    "https://example.com",
    "http://test.com",
    "https://api.example.com/path",
  ]

  video_puller.filter_valid_urls(urls)
  |> should.equal(urls)
}

pub fn filter_valid_urls_mixed_test() {
  let urls = [
    "https://example.com",
    "not-a-url",
    "http://test.com",
    "",
    "ftp://invalid.com",
  ]

  video_puller.filter_valid_urls(urls)
  |> should.equal(["https://example.com", "http://test.com"])
}

pub fn filter_valid_urls_empty_list_test() {
  video_puller.filter_valid_urls([])
  |> should.equal([])
}

pub fn filter_valid_urls_all_invalid_test() {
  let urls = ["not-a-url", "", "ftp://invalid.com"]

  video_puller.filter_valid_urls(urls)
  |> should.equal([])
}

// Safe get tests
pub fn safe_get_valid_index_test() {
  video_puller.safe_get([1, 2, 3, 4, 5], 2)
  |> should.be_ok()
  |> should.equal(3)
}

pub fn safe_get_first_element_test() {
  video_puller.safe_get(["a", "b", "c"], 0)
  |> should.be_ok()
  |> should.equal("a")
}

pub fn safe_get_negative_index_test() {
  video_puller.safe_get([1, 2, 3], -1)
  |> should.be_error()
  |> should.equal("Index out of bounds")
}

pub fn safe_get_out_of_bounds_test() {
  video_puller.safe_get([1, 2, 3], 10)
  |> should.be_error()
  |> should.equal("Index out of bounds")
}

pub fn safe_get_empty_list_test() {
  video_puller.safe_get([], 0)
  |> should.be_error()
  |> should.equal("Index out of bounds")
}
