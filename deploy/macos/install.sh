#!/usr/bin/env bash
# Install video-puller on macOS for a Plex library and run it as a LaunchAgent.
#
# Usage:
#   ./deploy/macos/install.sh
#
# Override defaults with environment variables:
#   PORT, INSTALL_DIR, DATA_DIR, POLL_TIMEOUT_MINUTES, PLIST_DIR
#
# Requires Homebrew (https://brew.sh). Run it from a checkout that already has
# build/erlang-shipment to install that build, or anywhere else to clone and
# build the public repository.
set -euo pipefail

PORT="${PORT:-8080}"
INSTALL_DIR="${INSTALL_DIR:-$HOME/video-puller}"
DATA_DIR="${DATA_DIR:-$HOME/PlexMedia/YouTube}"
POLL_TIMEOUT_MINUTES="${POLL_TIMEOUT_MINUTES:-720}"
SUBSCRIPTION_WORKERS="${SUBSCRIPTION_WORKERS:-1}"
SUBSCRIPTION_DAYS="${SUBSCRIPTION_DAYS:-30}"
POLL_ON_START="${POLL_ON_START:-true}"
STORAGE_VOLUME="${STORAGE_VOLUME:-}"
STORAGE_VOLUME_UUID="${STORAGE_VOLUME_UUID:-}"
MIN_FREE_GB="${MIN_FREE_GB:-50}"
STATE_BACKUP_DIR="${STATE_BACKUP_DIR:-$HOME/Library/Application Support/video-puller/state-backups}"
PLEX_URL="${PLEX_URL:-http://127.0.0.1:32400}"
PLEX_SECTION_ID="${PLEX_SECTION_ID:-}"
PLEX_MANAGED_TRASH_CLEANUP="${PLEX_MANAGED_TRASH_CLEANUP:-false}"
PLEX_MACHINE_ID="${PLEX_MACHINE_ID:-}"
PLIST_DIR="${PLIST_DIR:-$HOME/Library/LaunchAgents}"
LABEL="local.video-puller"
PLIST="$PLIST_DIR/$LABEL.plist"
# Preserve explicit local-session authorization and recent-only policy on upgrades.
preserve_launch_setting() {
  local setting_name=$1
  local saved_value
  if ! declare -p "$setting_name" >/dev/null 2>&1 && [[ -f "$PLIST" && -x /usr/libexec/PlistBuddy ]]; then
    if saved_value="$(/usr/libexec/PlistBuddy -c "Print :EnvironmentVariables:$setting_name" "$PLIST" 2>/dev/null)"; then
      printf -v "$setting_name" '%s' "$saved_value"
    fi
  fi
}
for setting_name in YOUTUBE_COOKIE_FILE YOUTUBE_SESSION_BROWSER YOUTUBE_SESSION_PYTHON YOUTUBE_SESSION_HELPER SUBSCRIPTION_BACKFILL; do
  preserve_launch_setting "$setting_name"
done
YOUTUBE_COOKIE_FILE="${YOUTUBE_COOKIE_FILE:-}"
YOUTUBE_SESSION_BROWSER="${YOUTUBE_SESSION_BROWSER:-}"
YOUTUBE_SESSION_PYTHON="${YOUTUBE_SESSION_PYTHON:-}"
YOUTUBE_SESSION_HELPER="${YOUTUBE_SESSION_HELPER:-$INSTALL_DIR/bin/export-youtube-session.py}"
SUBSCRIPTION_BACKFILL="${SUBSCRIPTION_BACKFILL:-true}"
CHANNEL_TIMEOUT_MINUTES="${CHANNEL_TIMEOUT_MINUTES:-10}"
REPO_URL="https://github.com/lprior-repo/video-puller.git"

info() { printf '\033[0;32m[INFO]\033[0m %s\n' "$1"; }
warn() { printf '\033[0;33m[WARN]\033[0m %s\n' "$1"; }
error() {
  printf '\033[0;31m[ERROR]\033[0m %s\n' "$1"
  exit 1
}

xml_escape() {
  local value=$1
  # Escape replacement ampersands too: Bash 5.2 expands unquoted & to the
  # matched text, unlike the Bash 3.2 shipped with macOS.
  value=${value//&/\&amp;}
  value=${value//</\&lt;}
  value=${value//>/\&gt;}
  printf '%s' "$value"
}

if [[ -n "$YOUTUBE_SESSION_BROWSER" && ( -z "$YOUTUBE_COOKIE_FILE" || -z "$YOUTUBE_SESSION_PYTHON" ) ]]; then
  error "Automatic session refresh requires YOUTUBE_COOKIE_FILE and YOUTUBE_SESSION_PYTHON (the ytdl-sub Python interpreter)"
fi

if [[ "$(uname -s)" != "Darwin" && "${ALLOW_NON_MAC:-0}" != "1" ]]; then
  error "This installer targets macOS (set ALLOW_NON_MAC=1 only for dry runs)"
fi

command -v brew >/dev/null 2>&1 || error "Homebrew is required: https://brew.sh"
BREW_PREFIX="$(brew --prefix)" || error "Could not determine the Homebrew prefix"
BREW_BIN="$BREW_PREFIX/bin"
LAUNCH_PATH="$BREW_BIN:$HOME/.local/bin:/usr/bin:/bin:/usr/sbin:/sbin"

info "Installing runtime dependencies with Homebrew"
for formula in erlang ffmpeg pipx deno yt-dlp; do
  if brew list --formula "$formula" >/dev/null 2>&1; then
    info "  $formula already installed"
  else
    info "  installing $formula"
    brew install "$formula"
  fi
done

if PATH="$LAUNCH_PATH" command -v ytdl-sub >/dev/null 2>&1; then
  info "  ytdl-sub already installed"
else
  info "  installing ytdl-sub with pipx"
  pipx install ytdl-sub
fi

PATH="$LAUNCH_PATH" command -v ytdl-sub >/dev/null 2>&1 ||
  error "ytdl-sub is not available on the LaunchAgent PATH ($LAUNCH_PATH)"
PATH="$LAUNCH_PATH" command -v yt-dlp >/dev/null 2>&1 ||
  error "yt-dlp is not available on the LaunchAgent PATH ($LAUNCH_PATH)"

if command -v erl >/dev/null 2>&1; then
  OTP="$(erl -noshell -eval 'io:format("~s", [erlang:system_info(otp_release)]), halt().' 2>/dev/null || echo unknown)"
  info "  Erlang OTP $OTP"
  case "$OTP" in
    2[89] | 3[0-9]) ;;
    *) warn "  the shipped release needs OTP 28 or newer: brew upgrade erlang" ;;
  esac
else
  warn "  erl is not on PATH: brew install erlang"
fi

if [[ -f "$PWD/gleam.toml" && -d "$PWD/build/erlang-shipment" ]]; then
  info "Installing from this checkout"
  SOURCE_DIR="$PWD"
else
  if [[ -d "$INSTALL_DIR/.git" ]]; then
    info "Updating $INSTALL_DIR"
    git -C "$INSTALL_DIR" pull --ff-only
  elif [[ -d "$INSTALL_DIR" && -n "$(find "$INSTALL_DIR" -mindepth 1 -maxdepth 1 -print -quit)" ]]; then
    error "$INSTALL_DIR is a non-git deployment; refusing to clone into its non-empty directory"
  else
    info "Cloning $REPO_URL"
    git clone "$REPO_URL" "$INSTALL_DIR"
  fi

  command -v gleam >/dev/null 2>&1 || brew install gleam

  info "Building the release"
  (cd "$INSTALL_DIR" && gleam deps download && gleam export erlang-shipment)

  SOURCE_DIR="$INSTALL_DIR"
fi

if [[ "$SOURCE_DIR" != "$INSTALL_DIR" ]]; then
  info "Copying application files to $INSTALL_DIR"
  mkdir -p "$INSTALL_DIR"
  rsync -a --delete \
    --include 'build/' \
    --include 'build/erlang-shipment/***' \
    --include 'priv/***' \
    --include 'gleam.toml' \
    --exclude '*' \
    "$SOURCE_DIR"/ "$INSTALL_DIR"/
fi

[[ -d "$INSTALL_DIR/build/erlang-shipment" ]] ||
  error "No release at $INSTALL_DIR/build/erlang-shipment"

mkdir -p "$INSTALL_DIR/bin"
install -m 700 "$SOURCE_DIR/deploy/macos/export_youtube_session.py" \
  "$INSTALL_DIR/bin/export-youtube-session.py"

LIBRARY_DIR="$DATA_DIR/library"
info "Preparing data directory $DATA_DIR"
mkdir -p "$DATA_DIR/logs" "$DATA_DIR/tmp" "$LIBRARY_DIR"

info "Writing LaunchAgent $PLIST"
mkdir -p "$PLIST_DIR"

XML_LABEL="$(xml_escape "$LABEL")"
XML_ENTRYPOINT="$(xml_escape "$INSTALL_DIR/build/erlang-shipment/entrypoint.sh")"
XML_INSTALL_DIR="$(xml_escape "$INSTALL_DIR")"
XML_PATH="$(xml_escape "$LAUNCH_PATH")"
XML_PORT="$(xml_escape "$PORT")"
XML_DATA_DIR="$(xml_escape "$DATA_DIR")"
XML_DB_PATH="$(xml_escape "$DATA_DIR/video_eater.db")"
XML_STATIC_DIR="$(xml_escape "$INSTALL_DIR/priv/static")"
XML_POLL_TIMEOUT="$(xml_escape "$POLL_TIMEOUT_MINUTES")"
XML_LOG_PATH="$(xml_escape "$DATA_DIR/logs/video-puller.log")"

TEMP_PLIST="$(mktemp "$PLIST_DIR/.${LABEL}.plist.XXXXXX")" ||
  error "Could not create a temporary plist in $PLIST_DIR"
trap 'rm -f "$TEMP_PLIST"' EXIT
cat > "$TEMP_PLIST" <<PLIST
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>Label</key>
  <string>$XML_LABEL</string>
  <key>ProgramArguments</key>
  <array>
    <string>$XML_ENTRYPOINT</string>
    <string>run</string>
  </array>
  <key>WorkingDirectory</key>
  <string>$XML_INSTALL_DIR</string>
  <key>EnvironmentVariables</key>
  <dict>
    <key>PATH</key>
    <string>$XML_PATH</string>
    <key>PORT</key>
    <string>$XML_PORT</string>
    <key>DATA_DIR</key>
    <string>$XML_DATA_DIR</string>
    <key>DB_PATH</key>
    <string>$XML_DB_PATH</string>
    <key>STATIC_DIR</key>
    <string>$XML_STATIC_DIR</string>
    <key>POLL_TIMEOUT_MINUTES</key>
    <string>$XML_POLL_TIMEOUT</string>
    <key>SUBSCRIPTION_WORKERS</key>
    <string>$(xml_escape "$SUBSCRIPTION_WORKERS")</string>
    <key>YOUTUBE_COOKIE_FILE</key>
    <string>$(xml_escape "$YOUTUBE_COOKIE_FILE")</string>
    <key>YOUTUBE_SESSION_BROWSER</key>
    <string>$(xml_escape "$YOUTUBE_SESSION_BROWSER")</string>
    <key>YOUTUBE_SESSION_PYTHON</key>
    <string>$(xml_escape "$YOUTUBE_SESSION_PYTHON")</string>
    <key>YOUTUBE_SESSION_HELPER</key>
    <string>$(xml_escape "$YOUTUBE_SESSION_HELPER")</string>
    <key>SUBSCRIPTION_BACKFILL</key>
    <string>$(xml_escape "$SUBSCRIPTION_BACKFILL")</string>
    <key>CHANNEL_TIMEOUT_MINUTES</key>
    <string>$(xml_escape "$CHANNEL_TIMEOUT_MINUTES")</string>
    <key>POLL_ON_START</key>
    <string>$(xml_escape "$POLL_ON_START")</string>
    <key>SUBSCRIPTION_DAYS</key>
    <string>$(xml_escape "$SUBSCRIPTION_DAYS")</string>
    <key>MIN_FREE_GB</key>
    <string>$(xml_escape "$MIN_FREE_GB")</string>
    <key>STATE_BACKUP_DIR</key>
    <string>$(xml_escape "$STATE_BACKUP_DIR")</string>
    <key>STORAGE_VOLUME</key>
    <string>$(xml_escape "$STORAGE_VOLUME")</string>
    <key>STORAGE_VOLUME_UUID</key>
    <string>$(xml_escape "$STORAGE_VOLUME_UUID")</string>
    <key>PLEX_URL</key>
    <string>$(xml_escape "$PLEX_URL")</string>
    <key>PLEX_SECTION_ID</key>
    <string>$(xml_escape "$PLEX_SECTION_ID")</string>
    <key>PLEX_MANAGED_TRASH_CLEANUP</key>
    <string>$(xml_escape "$PLEX_MANAGED_TRASH_CLEANUP")</string>
    <key>PLEX_MACHINE_ID</key>
    <string>$(xml_escape "$PLEX_MACHINE_ID")</string>
    <key>OUTPUT_DIR</key>
    <string>$XML_DATA_DIR/downloads</string>
    <key>TMPDIR</key>
    <string>$XML_DATA_DIR/tmp</string>
  </dict>
  <key>RunAtLoad</key>
  <true/>
  <key>KeepAlive</key>
  <true/>
  <key>StandardOutPath</key>
  <string>$XML_LOG_PATH</string>
  <key>StandardErrorPath</key>
  <string>$XML_LOG_PATH</string>
</dict>
</plist>
PLIST

if command -v plutil >/dev/null 2>&1; then
  plutil -lint "$TEMP_PLIST" || error "Generated plist failed validation"
else
  warn "plutil not available; skipping plist validation"
fi

mv -f "$TEMP_PLIST" "$PLIST"
trap - EXIT

if [[ "$(uname -s)" == "Darwin" ]] && command -v launchctl >/dev/null 2>&1; then
  info "Starting the service"
  launchctl bootout "gui/$UID/$LABEL" 2>/dev/null || true
  launchctl bootstrap "gui/$UID" "$PLIST"
  launchctl kickstart -k "gui/$UID/$LABEL"
else
  warn "launchctl not available on this platform; load $PLIST manually"
fi

info "Waiting for the web server on port $PORT"
for _ in $(seq 1 30); do
  if curl -fsS "http://127.0.0.1:$PORT/health" >/dev/null 2>&1; then
    break
  fi
  sleep 1
done

if curl -fsS "http://127.0.0.1:$PORT/health" >/dev/null 2>&1; then
  info "Service is up: http://127.0.0.1:$PORT/subscriptions"
else
  warn "Service did not answer yet; check $DATA_DIR/logs/video-puller.log"
fi

cat <<SUMMARY

Installation complete.

  Web UI        http://127.0.0.1:$PORT/subscriptions
  Channel list  $DATA_DIR/ytdl-sub/channels.txt   (one public channel URL per line)
  Library       $LIBRARY_DIR
  Logs          $DATA_DIR/logs/video-puller.log

Next steps:
  1. Add channel URLs to the channel list, then press "Refresh Now" in the UI.
     Or drop a Google Takeout subscriptions.csv beside it to import your
     channels without signing in.
     The first poll pulls uploads within SUBSCRIPTION_DAYS ($SUBSCRIPTION_DAYS);
     zero selects full history. Later polls skip archived downloads. Keep
     POLL_TIMEOUT_MINUTES ($POLL_TIMEOUT_MINUTES) generous.
  2. In Plex, add "$LIBRARY_DIR" as a TV Shows library.
  3. Enable automatic polling on the Subscriptions page.

Service control:
  launchctl kickstart -k gui/$UID/$LABEL   # restart
  launchctl bootout gui/$UID/$LABEL        # stop and unload
SUMMARY
