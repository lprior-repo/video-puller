/// Additional options for the ytdl-sub Plex subscription preset.
///
/// Keep these as preset fragments so the subscription generator can append
/// them after the preset's defaults without replacing its artwork, chapters,
/// thumbnails, or embedded video tags.
import gleam/int

/// Build a recent-upload date window. A zero or negative value leaves the
/// preset unrestricted, which is how the full-history backfill is configured.
pub fn preset_block(days: Int) -> String {
  let date_range = case days > 0 {
    True ->
      "  date_range:\n"
      <> "    after: \"today-"
      <> int.to_string(days)
      <> "days\"\n"
      <> "    breaks: True\n"
      <> "    type: upload_date\n"
    False -> ""
  }

  date_range
  <> "  subtitles:\n"
  <> "    subtitles_name: \"{episode_file_path}.{lang}.{subtitles_ext}\"\n"
  <> "    subtitles_type: srt\n"
  <> "    embed_subtitles: True\n"
  // English already includes en-orig. Matching every *-orig track also
  // fetches captions for translated audio tracks and can exhaust a poll.
  <> "    languages: [\"en.*\"]\n"
  <> "    allow_auto_generated_subtitles: True\n"
}

/// Keep Plex episode titles equal to the source video title. Date and channel
/// metadata are retained in their dedicated Plex fields by the base preset.
pub fn overrides_block() -> String {
  "    episode_title: \"{title}\"\n"
}

/// Exclude subscriber and premium-only videos before download attempts.
///
/// The optional comparison marker lets entries without availability metadata
/// pass, preserving public and unlisted videos. Keep this in the match_filters
/// plugin so it composes with the preset date-range plugin.
pub fn restricted_filter_block(skip_restricted: Bool) -> String {
  case skip_restricted {
    True ->
      "  match_filters:\n"
      <> "    filters:\n"
      <> "      - \"availability !=? premium_only & availability !=? subscriber_only\"\n"
    False -> ""
  }
}
