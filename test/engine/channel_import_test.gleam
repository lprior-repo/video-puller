import engine/channel_import
import engine/ytdl_sub
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import gleeunit/should
import simplifile

pub fn parse_copied_youtube_subscriptions_test() {
  let pasted =
    "Beautybyrositax\n@Beautybyrosita•58.1K subscribers\nYour beauty bestie\nSubscribed\nBECKY FOWLER\n@BECKYLOUFOWLER•13.3K subscribers\nDescription text\nSubscribed\n"
  let channels = channel_import.parse_channels(pasted)

  list.length(channels) |> should.equal(2)
  channels
  |> should.equal([
    ytdl_sub.Channel(
      Some("Beautybyrositax"),
      "https://www.youtube.com/@Beautybyrosita",
    ),
    ytdl_sub.Channel(
      Some("BECKY FOWLER"),
      "https://www.youtube.com/@BECKYLOUFOWLER",
    ),
  ])
}

pub fn parse_channels_file_lines_test() {
  channel_import.parse_channels(
    "Old Channel = https://youtube.com/@OldChannel/videos\nhttps://www.youtube.com/@another\n",
  )
  |> should.equal([
    ytdl_sub.Channel(Some("Old Channel"), "https://youtube.com/@OldChannel"),
    ytdl_sub.Channel(None, "https://www.youtube.com/@another"),
  ])
}

pub fn parse_copied_list_skips_external_url_description_test() {
  let creators =
    list.range(1, 50)
    |> list.map(fn(index) {
      let display_name = "Creator " <> int.to_string(index)
      let handle = "@creator" <> int.to_string(index)
      let description = case index == 1 {
        True ->
          "\nhttps://www.margaritanazarenko.com TIKTOK INSTAGRAM FOLLOW ME"
        False -> ""
      }
      display_name
      <> "\n"
      <> handle
      <> "•1K subscribers"
      <> description
      <> "\nSubscribed"
    })
    |> string.join("\n")
  let explicit =
    "\nPlaylist = https://www.youtube.com/playlist?list=PL123"
    <> "\nVideo = https://www.youtube.com/watch?v=video123"
    <> "\nShort link = https://youtu.be/video456"
    <> "\nChannel URL = https://www.youtube.com/channel/UC123"

  channel_import.parse_channels(creators <> explicit)
  |> list.length
  |> should.equal(54)
}

pub fn desired_reconciliation_preserves_labels_and_reports_removed_test() {
  let existing =
    "Archive Name = https://www.youtube.com/@ArchiveName/videos\nRetired Show = https://www.youtube.com/@retired\n"
  let desired =
    "New Display Name\n@archivename•2K subscribers\nSubscribed\nAdded Channel = https://www.youtube.com/@AddedChannel\n"
  let channels_file = "/tmp/channel_import_channels.txt"
  let desired_file = "/tmp/channel_import_desired-channels.txt"
  let _ = simplifile.delete(channels_file)
  let _ = simplifile.delete(desired_file)
  let _ = simplifile.write(channels_file, existing)

  channel_import.reconcile_content(desired, channels_file)
  |> should.equal(
    Ok(channel_import.ImportSummary(
      added: 1,
      removed: [
        ytdl_sub.Channel(
          Some("Retired Show"),
          "https://www.youtube.com/@retired",
        ),
      ],
      found: 2,
    )),
  )
  // Staging desired state leaves active channels intact until the coordinator
  // has retired removed libraries and synchronized the database and Plex.
  simplifile.read(channels_file) |> should.equal(Ok(existing))
  simplifile.read(desired_file)
  |> should.equal(Ok(
    "Archive Name = https://www.youtube.com/@ArchiveName\nAdded Channel = https://www.youtube.com/@AddedChannel\n",
  ))

  channel_import.sync_desired(channels_file)
  |> should.equal(
    Ok(channel_import.ImportSummary(
      added: 1,
      removed: [
        ytdl_sub.Channel(
          Some("Retired Show"),
          "https://www.youtube.com/@retired",
        ),
      ],
      found: 2,
    )),
  )
  simplifile.read(channels_file)
  |> should.equal(Ok(
    "Archive Name = https://www.youtube.com/@ArchiveName\nAdded Channel = https://www.youtube.com/@AddedChannel\n",
  ))
  let _ = simplifile.delete(channels_file)
  let _ = simplifile.delete(desired_file)
}

pub fn import_file_persists_desired_state_without_changing_active_list_test() {
  let source = "/tmp/channel_import_source.txt"
  let channels_file = "/tmp/channel_import_target_channels.txt"
  let desired_file = "/tmp/channel_import_target_desired-channels.txt"
  let _ = simplifile.delete(source)
  let _ = simplifile.delete(channels_file)
  let _ = simplifile.delete(desired_file)
  let _ =
    simplifile.write(
      source,
      "First Channel\n@FirstChannel•1K subscribers\nSubscribed\n",
    )
  let _ =
    simplifile.write(
      channels_file,
      "Kept Label = https://youtube.com/@kept/videos\n",
    )

  let result = channel_import.reconcile_file(source, channels_file)
  result
  |> should.equal(
    Ok(channel_import.ImportSummary(
      added: 1,
      removed: [
        ytdl_sub.Channel(Some("Kept Label"), "https://youtube.com/@kept"),
      ],
      found: 1,
    )),
  )
  simplifile.read(desired_file)
  |> should.equal(Ok("First Channel = https://www.youtube.com/@FirstChannel\n"))
  simplifile.read(channels_file)
  |> should.equal(Ok("Kept Label = https://youtube.com/@kept/videos\n"))

  let _ = simplifile.delete(source)
  let _ = simplifile.delete(channels_file)
  let _ = simplifile.delete(desired_file)
}

pub fn empty_import_is_rejected_without_overwriting_channels_test() {
  let channels_file = "/tmp/channel_import_empty_target.txt"
  let _ = simplifile.delete(channels_file)
  let _ =
    simplifile.write(channels_file, "Keep = https://www.youtube.com/@keep\n")
  channel_import.reconcile_content("Just some description text", channels_file)
  |> should.equal(Error("no YouTube channels found in the supplied text"))
  simplifile.read(channels_file)
  |> should.equal(Ok("Keep = https://www.youtube.com/@keep\n"))
  let _ = simplifile.delete(channels_file)
}

pub fn empty_persisted_desired_list_cannot_remove_active_channels_test() {
  let channels_file = "/tmp/channel_import_guard_channels.txt"
  let desired_file = "/tmp/channel_import_guard_desired-channels.txt"
  let active = "Keep = https://www.youtube.com/@keep\n"
  let _ = simplifile.delete(channels_file)
  let _ = simplifile.write(channels_file, active)
  let _ = simplifile.write(desired_file, "\n# intentionally empty\n")

  channel_import.sync_desired(channels_file)
  |> should.equal(Error(
    "desired channel file contains no channels; active list unchanged",
  ))
  simplifile.read(channels_file) |> should.equal(Ok(active))

  let _ = simplifile.delete(channels_file)
  let _ = simplifile.delete(desired_file)
}

pub fn additive_channel_stays_in_desired_state_on_next_sync_test() {
  let channels_file = "/tmp/channel_add_active_channels.txt"
  let desired_file = "/tmp/channel_add_active_desired-channels.txt"
  let existing = "Existing = https://www.youtube.com/@Existing\n"
  let _ = simplifile.delete(channels_file)
  let _ = simplifile.delete(desired_file)
  let _ = simplifile.write(channels_file, existing)
  let _ = simplifile.write(desired_file, existing)

  channel_import.add_desired_channel(
    ytdl_sub.Channel(Some("Added Later"), "https://www.youtube.com/@AddedLater"),
    channels_file,
  )
  |> should.equal(
    Ok(channel_import.ImportSummary(added: 1, removed: [], found: 2)),
  )
  simplifile.read(desired_file)
  |> should.equal(Ok(
    "Existing = https://www.youtube.com/@Existing\nAdded Later = https://www.youtube.com/@AddedLater\n",
  ))

  channel_import.add_desired_channel(
    ytdl_sub.Channel(
      Some("Replacement Label"),
      "https://www.youtube.com/@addedlater",
    ),
    channels_file,
  )
  |> should.equal(
    Ok(channel_import.ImportSummary(added: 0, removed: [], found: 2)),
  )
  simplifile.read(desired_file)
  |> should.equal(Ok(
    "Existing = https://www.youtube.com/@Existing\nAdded Later = https://www.youtube.com/@AddedLater\n",
  ))

  channel_import.sync_desired(channels_file)
  |> should.equal(
    Ok(channel_import.ImportSummary(added: 1, removed: [], found: 2)),
  )
  simplifile.read(channels_file)
  |> should.equal(Ok(
    "Existing = https://www.youtube.com/@Existing\nAdded Later = https://www.youtube.com/@AddedLater\n",
  ))

  let _ = simplifile.delete(channels_file)
  let _ = simplifile.delete(desired_file)
}

pub fn pending_changes_detects_staged_and_applied_state_test() {
  let channels_file = "/tmp/channel_import_pending_channels.txt"
  let desired_file = "/tmp/channel_import_pending_desired-channels.txt"
  let active = "Active = https://www.youtube.com/@active\n"
  let _ = simplifile.delete(channels_file)
  let _ = simplifile.delete(desired_file)
  let _ = simplifile.write(channels_file, active)

  channel_import.has_pending_changes(channels_file) |> should.equal(Ok(False))
  channel_import.reconcile_content(
    "Desired\n@desired•1K subscribers\nSubscribed\n",
    channels_file,
  )
  |> should.be_ok()
  channel_import.has_pending_changes(channels_file) |> should.equal(Ok(True))

  channel_import.sync_desired(channels_file) |> should.be_ok()
  channel_import.has_pending_changes(channels_file) |> should.equal(Ok(False))

  let _ = simplifile.delete(channels_file)
  let _ = simplifile.delete(desired_file)
}

pub fn pending_empty_desired_state_is_error_test() {
  let channels_file = "/tmp/channel_import_pending_empty_channels.txt"
  let desired_file = "/tmp/channel_import_pending_empty_desired-channels.txt"
  let _ = simplifile.delete(channels_file)
  let _ = simplifile.delete(desired_file)
  let _ = simplifile.write(channels_file, "Keep = https://youtube.com/@keep\n")
  let _ = simplifile.write(desired_file, "\n# no channels\n")

  channel_import.has_pending_changes(channels_file)
  |> should.equal(Error("desired channel file contains no channels"))

  let _ = simplifile.delete(channels_file)
  let _ = simplifile.delete(desired_file)
}

pub fn malformed_desired_mixture_is_rejected_without_changing_files_test() {
  let channels_file = "/tmp/channel_import_malformed_channels.txt"
  let desired_file = "/tmp/channel_import_malformed_desired-channels.txt"
  let active = "Active = https://www.youtube.com/@active\n"
  let bad_description_url =
    "https://www.margaritanazarenko.com TIKTOK INSTAGRAM FOLLOW ME"
  let desired =
    "Keep = https://www.youtube.com/@keep\n" <> bad_description_url <> "\n"
  let _ = simplifile.delete(channels_file)
  let _ = simplifile.delete(desired_file)
  let _ = simplifile.write(channels_file, active)
  let _ = simplifile.write(desired_file, desired)

  channel_import.sync_desired(channels_file)
  |> should.equal(Error("invalid channel list line: " <> bad_description_url))
  channel_import.has_pending_changes(channels_file)
  |> should.equal(Error("invalid channel list line: " <> bad_description_url))
  simplifile.read(channels_file) |> should.equal(Ok(active))
  simplifile.read(desired_file) |> should.equal(Ok(desired))

  let _ = simplifile.delete(channels_file)
  let _ = simplifile.delete(desired_file)
}

pub fn malformed_additive_base_is_rejected_without_writing_desired_test() {
  let channels_file = "/tmp/channel_import_invalid_base_channels.txt"
  let desired_file = "/tmp/channel_import_invalid_base_desired-channels.txt"
  let active = "Keep = https://www.youtube.com/@keep\ninvalid row\n"
  let _ = simplifile.delete(channels_file)
  let _ = simplifile.delete(desired_file)
  let _ = simplifile.write(channels_file, active)

  channel_import.add_desired_channel(
    ytdl_sub.Channel(Some("New"), "https://www.youtube.com/@new"),
    channels_file,
  )
  |> should.equal(Error("invalid channel list line: invalid row"))
  simplifile.read(channels_file) |> should.equal(Ok(active))
  simplifile.is_file(desired_file) |> should.equal(Ok(False))

  let _ = simplifile.delete(channels_file)
  let _ = simplifile.delete(desired_file)
}

pub fn malformed_active_base_is_rejected_without_syncing_test() {
  let channels_file = "/tmp/channel_import_bad_active_channels.txt"
  let desired_file = "/tmp/channel_import_bad_active_desired-channels.txt"
  let active = "Keep = https://www.youtube.com/@keep\ninvalid active row\n"
  let desired = "Wanted = https://www.youtube.com/@wanted\n"
  let _ = simplifile.delete(channels_file)
  let _ = simplifile.delete(desired_file)
  let _ = simplifile.write(channels_file, active)
  let _ = simplifile.write(desired_file, desired)

  channel_import.sync_desired(channels_file)
  |> should.equal(Error("invalid channel list line: invalid active row"))
  channel_import.has_pending_changes(channels_file)
  |> should.equal(Error("invalid channel list line: invalid active row"))
  simplifile.read(channels_file) |> should.equal(Ok(active))
  simplifile.read(desired_file) |> should.equal(Ok(desired))

  let _ = simplifile.delete(channels_file)
  let _ = simplifile.delete(desired_file)
}
