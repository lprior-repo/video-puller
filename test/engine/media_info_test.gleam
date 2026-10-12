import engine/media_info
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import gleeunit
import gleeunit/should
import simplifile

pub fn main() {
  gleeunit.main()
}

pub fn parses_all_fields_and_fractional_duration_in_one_record_test() {
  let root = "/tmp/test_media_info_parse"
  let media = root <> "/Show/Season 2024/episode.mp4"
  let sidecar = media_info.info_json_path(media)
  let cache = root <> "/cache/video-metadata"
  let _ = simplifile.create_directory_all(parent_path(media))
  let content =
    "{\"id\":\"abc123\",\"channel_id\":\"UC456\",\"title\":\"Original title\","
    <> "\"channel\":\"Creator\",\"upload_date\":\"20240917\",\"duration\":214.4,"
    <> "\"thumbnail\":\"https://img.test/thumb.jpg\",\"webpage_url\":"
    <> "\"https://www.youtube.com/watch?v=abc123\",\"description\":\"Long description\","
    <> "\"view_count\":12345,"
    <> "\"formats\":\""
    <> string.repeat("x", 10_000)
    <> "\"}"
  let _ = simplifile.write(sidecar, content)

  media_info.read(media, cache)
  |> should.equal(
    Some(media_info.Metadata(
      id: Some("abc123"),
      channel_id: Some("UC456"),
      title: Some("Original title"),
      channel: Some("Creator"),
      upload_date: Some("20240917"),
      duration: Some(214),
      thumbnail: Some("https://img.test/thumb.jpg"),
      webpage_url: Some("https://www.youtube.com/watch?v=abc123"),
      description: Some("Long description"),
      view_count: Some(12_345),
    )),
  )

  simplifile.read(sidecar) |> should.equal(Ok(content))
  let assert Ok(cache_files) = simplifile.read_directory(cache)
  cache_files |> list.length |> should.equal(1)
  let assert [cache_filename] = cache_files
  let assert Ok(cache_content) = simplifile.read(cache <> "/" <> cache_filename)
  should.be_true(string.length(cache_content) < string.length(content))
  let _ = simplifile.delete(root)
  Nil
}

pub fn cache_invalidates_when_sidecar_size_changes_test() {
  let root = "/tmp/test_media_info_invalidation"
  let media = root <> "/episode.mkv"
  let sidecar = media_info.info_json_path(media)
  let cache = root <> "/cache/video-metadata"
  let _ = simplifile.create_directory_all(root)
  let _ =
    simplifile.write(
      sidecar,
      "{\"id\":\"one\",\"title\":\"Old\",\"duration\":42}",
    )

  case media_info.read(media, cache) {
    Some(value) -> value.title |> should.equal(Some("Old"))
    None -> should.fail()
  }
  case media_info.read(media, cache) {
    Some(value) -> value.duration |> should.equal(Some(42))
    None -> should.fail()
  }

  let _ =
    simplifile.write(
      sidecar,
      "{\"id\":\"two\",\"title\":\"New title\",\"duration\":43}",
    )
  media_info.read(media, cache)
  |> should.equal(
    Some(media_info.Metadata(
      id: Some("two"),
      channel_id: None,
      title: Some("New title"),
      channel: None,
      upload_date: None,
      duration: Some(43),
      thumbnail: None,
      webpage_url: None,
      description: None,
      view_count: None,
    )),
  )
  let _ = simplifile.delete(root)
  Nil
}

pub fn legacy_cache_without_schema_version_refreshes_view_count_once_test() {
  let root = "/tmp/test_media_info_cache_schema"
  let media = root <> "/episode.mp4"
  let sidecar = media_info.info_json_path(media)
  let cache = root <> "/cache/video-metadata"
  let _ = simplifile.create_directory_all(root)
  let _ =
    simplifile.write(
      sidecar,
      "{\"id\":\"schema1\",\"description\":\"A description\","
        <> "\"thumbnail\":\"https://img.test/thumb.jpg\",\"view_count\":77}",
    )

  let assert Some(metadata) = media_info.read(media, cache)
  metadata.view_count |> should.equal(Some(77))
  metadata.description |> should.equal(Some("A description"))
  metadata.thumbnail |> should.equal(Some("https://img.test/thumb.jpg"))

  let assert Ok(files) = simplifile.read_directory(cache)
  let assert [filename] = files
  let cache_path = cache <> "/" <> filename
  let assert Ok(current_cache) = simplifile.read(cache_path)
  let legacy_cache =
    current_cache
    |> string.replace("\"schema_version\":2,", "")
    |> string.replace(",\"view_count\":77", "")
  let _ = simplifile.write(cache_path, legacy_cache)

  let assert Some(refreshed) = media_info.read(media, cache)
  refreshed.view_count |> should.equal(Some(77))
  let assert Ok(refreshed_cache) = simplifile.read(cache_path)
  refreshed_cache |> string.contains("\"schema_version\":2") |> should.be_true()
  refreshed_cache |> string.contains("\"view_count\":77") |> should.be_true()

  let assert Some(cached) = media_info.read(media, cache)
  cached.view_count |> should.equal(Some(77))
  let _ = simplifile.delete(root)
  Nil
}

pub fn integer_duration_and_missing_or_corrupt_sidecars_are_tolerated_test() {
  let root = "/tmp/test_media_info_fallbacks"
  let cache = root <> "/cache/video-metadata"
  let media = root <> "/known.mp4"
  let corrupt = root <> "/corrupt.mp4"
  let missing = root <> "/missing.mp4"
  let _ = simplifile.create_directory_all(root)
  let _ =
    simplifile.write(
      media_info.info_json_path(media),
      "{\"id\":\"known\",\"duration\":215}",
    )
  let _ = simplifile.write(media_info.info_json_path(corrupt), "not json")

  media_info.read(media, cache)
  |> should.equal(
    Some(media_info.Metadata(
      id: Some("known"),
      channel_id: None,
      title: None,
      channel: None,
      upload_date: None,
      duration: Some(215),
      thumbnail: None,
      webpage_url: None,
      description: None,
      view_count: None,
    )),
  )
  media_info.read(corrupt, cache) |> should.equal(None)
  let assert Ok(cache_files) = simplifile.read_directory(cache)
  cache_files |> list.length |> should.equal(1)
  let _ =
    simplifile.write(
      media_info.info_json_path(corrupt),
      "{\"id\":\"repaired\",\"title\":\"Fixed\"}",
    )
  media_info.read(corrupt, cache)
  |> should.equal(
    Some(media_info.Metadata(
      id: Some("repaired"),
      channel_id: None,
      title: Some("Fixed"),
      channel: None,
      upload_date: None,
      duration: None,
      thumbnail: None,
      webpage_url: None,
      description: None,
      view_count: None,
    )),
  )
  media_info.read(missing, cache) |> should.equal(None)
  let _ = simplifile.delete(root)
  Nil
}

fn parent_path(path: String) -> String {
  path
  |> string.split("/")
  |> list.reverse
  |> list.drop(1)
  |> list.reverse
  |> string.join("/")
}
