/// Enrich the subscription grid from local caches without network requests.
import domain/subscription_types.{type SeenVideo}
import engine/media_info
import engine/ytdl_sub
import gleam/dict
import gleam/list
import gleam/option.{None, Some}
import infra/plex
import web/templates.{type FeedCard, FeedCard}

pub fn cards(
  layout: ytdl_sub.Layout,
  videos: List(SeenVideo),
) -> List(FeedCard) {
  let index = plex.cached_video_keys()
  let metadata =
    ytdl_sub.list_library_media(layout.library_dir)
    |> list.filter_map(fn(path) {
      case media_info.read(path, layout.root <> "/cache/video-metadata") {
        Some(info) ->
          case info.id {
            Some(id) -> Ok(#(id, #(path, info)))
            None -> Error(Nil)
          }
        None -> Error(Nil)
      }
    })
    |> dict.from_list
  list.map(videos, fn(video) {
    case dict.get(metadata, video.video_id) {
      Ok(#(path, info)) -> {
        let page = case dict.get(index, path) {
          Ok(key) -> plex.video_page_url(key)
          Error(_) -> None
        }
        FeedCard(video, page, info.description, info.view_count)
      }
      Error(_) -> FeedCard(video, None, None, None)
    }
  })
}
