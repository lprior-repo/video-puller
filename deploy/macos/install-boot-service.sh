#!/usr/bin/env bash
# Convert the current user's video-puller LaunchAgent into a system
# LaunchDaemon. This script only prepares/installs the job when explicitly run
# by the user through sudo; do not invoke it during unattended deployment.
set -euo pipefail

AGENT_LABEL="local.video-puller"
BOOT_LABEL="local.video-puller.boot"
SYSTEM_PLIST="/Library/LaunchDaemons/$BOOT_LABEL.plist"

info() { printf '[INFO] %s\n' "$1"; }
error() {
  printf '[ERROR] %s\n' "$1" >&2
  exit 1
}

[[ "$(uname -s)" == "Darwin" ]] || error "This installer only runs on macOS."
[[ "$EUID" == "0" ]] || error "Run this installer through sudo."
[[ -n "${SUDO_USER:-}" && "$SUDO_USER" != "root" ]] ||
  error "Run sudo from the account that owns the existing LaunchAgent."

RUN_USER="$SUDO_USER"
USER_HOME="$(/usr/bin/dscl . -read "/Users/$RUN_USER" NFSHomeDirectory | /usr/bin/sed 's/^NFSHomeDirectory: //')"
[[ -n "$USER_HOME" && -d "$USER_HOME" ]] ||
  error "Could not determine the home directory for $RUN_USER."
RUN_UID="$(/usr/bin/id -u "$RUN_USER")"
AGENT_PLIST="$USER_HOME/Library/LaunchAgents/$AGENT_LABEL.plist"
AGENT_BACKUP="$AGENT_PLIST.disabled"

# A repeat run after successful conversion should be a harmless no-op.
if [[ ! -f "$AGENT_PLIST" && -f "$SYSTEM_PLIST" ]]; then
  INSTALLED_LABEL="$(/usr/libexec/PlistBuddy -c 'Print :Label' "$SYSTEM_PLIST" 2>/dev/null || true)"
  if [[ "$INSTALLED_LABEL" == "$BOOT_LABEL" ]]; then
    info "The system LaunchDaemon is already installed at $SYSTEM_PLIST."
    exit 0
  fi
fi

[[ -f "$AGENT_PLIST" ]] ||
  error "Expected the installed LaunchAgent at $AGENT_PLIST. Run deploy/macos/install.sh first."
[[ ! -e "$AGENT_BACKUP" ]] ||
  error "Refusing to overwrite the preserved LaunchAgent backup at $AGENT_BACKUP."
[[ "$(/usr/bin/stat -f '%Su' "$AGENT_PLIST")" == "$RUN_USER" ]] ||
  error "The LaunchAgent is not owned by $RUN_USER; refusing to convert it."

TEMP_PLIST="$(/usr/bin/mktemp "/Library/LaunchDaemons/.$BOOT_LABEL.XXXXXX")"
BACKUP_PLIST=""
AGENT_DISABLED=0
AGENT_STOPPED=0
NEW_PLIST_INSTALLED=0

rollback() {
  local status=$?
  trap - EXIT
  if [[ "$status" -ne 0 ]]; then
    /bin/launchctl bootout "system/$BOOT_LABEL" >/dev/null 2>&1 || true
    if [[ "$NEW_PLIST_INSTALLED" == "1" ]]; then
      /bin/rm -f "$SYSTEM_PLIST"
    fi
    if [[ -n "$BACKUP_PLIST" && -f "$BACKUP_PLIST" ]]; then
      /bin/cp -p "$BACKUP_PLIST" "$SYSTEM_PLIST" || true
      /usr/sbin/chown root:wheel "$SYSTEM_PLIST" || true
      /bin/chmod 0644 "$SYSTEM_PLIST" || true
      /bin/launchctl bootstrap system "$SYSTEM_PLIST" >/dev/null 2>&1 || true
    fi
    if [[ "$AGENT_DISABLED" == "1" && -f "$AGENT_BACKUP" ]]; then
      /bin/mv "$AGENT_BACKUP" "$AGENT_PLIST" || true
    fi
    if [[ "$AGENT_STOPPED" == "1" && -f "$AGENT_PLIST" ]]; then
      /bin/launchctl bootstrap "gui/$RUN_UID" "$AGENT_PLIST" >/dev/null 2>&1 || true
    fi
    /bin/rm -f "$TEMP_PLIST"
    exit "$status"
  fi
}
trap rollback EXIT

/bin/cp "$AGENT_PLIST" "$TEMP_PLIST"

plist_keypath() {
  printf '%s' "$1" | /usr/bin/tr '.' ':'
}

plist_set_string() {
  local key="$1" value="$2" plistbuddy_key
  plistbuddy_key="$(plist_keypath "$key")"
  if /usr/libexec/PlistBuddy -c "Print :$plistbuddy_key" "$TEMP_PLIST" >/dev/null 2>&1; then
    /usr/bin/plutil -replace "$key" -string "$value" "$TEMP_PLIST"
  else
    /usr/bin/plutil -insert "$key" -string "$value" "$TEMP_PLIST"
  fi
}

plist_set_bool() {
  local key="$1" value="$2" plistbuddy_key
  plistbuddy_key="$(plist_keypath "$key")"
  if /usr/libexec/PlistBuddy -c "Print :$plistbuddy_key" "$TEMP_PLIST" >/dev/null 2>&1; then
    /usr/bin/plutil -replace "$key" -bool "$value" "$TEMP_PLIST"
  else
    /usr/bin/plutil -insert "$key" -bool "$value" "$TEMP_PLIST"
  fi
}

plist_set_integer() {
  local key="$1" value="$2" plistbuddy_key
  plistbuddy_key="$(plist_keypath "$key")"
  if /usr/libexec/PlistBuddy -c "Print :$plistbuddy_key" "$TEMP_PLIST" >/dev/null 2>&1; then
    /usr/bin/plutil -replace "$key" -integer "$value" "$TEMP_PLIST"
  else
    /usr/bin/plutil -insert "$key" -integer "$value" "$TEMP_PLIST"
  fi
}

plist_set_string Label "$BOOT_LABEL"
plist_set_string UserName "$RUN_USER"
plist_set_bool RunAtLoad true
plist_set_bool KeepAlive true
plist_set_integer ThrottleInterval 60
plist_set_integer ExitTimeOut 15
plist_set_string EnvironmentVariables.HOME "$USER_HOME"
plist_set_string EnvironmentVariables.USER "$RUN_USER"
plist_set_string EnvironmentVariables.LOGNAME "$RUN_USER"

PORT_VALUE="$(/usr/libexec/PlistBuddy -c 'Print :EnvironmentVariables:PORT' "$AGENT_PLIST" 2>/dev/null || printf '8080')"
DATA_DIR_VALUE="$(/usr/libexec/PlistBuddy -c 'Print :EnvironmentVariables:DATA_DIR' "$AGENT_PLIST" 2>/dev/null || printf '%s/PlexMedia/YouTube' "$USER_HOME")"
LOG_PATH="$(/usr/libexec/PlistBuddy -c 'Print :StandardOutPath' "$AGENT_PLIST" 2>/dev/null || printf '%s/logs/video-puller.log' "$DATA_DIR_VALUE")"
[[ "$PORT_VALUE" =~ ^[0-9]+$ ]] || error "The LaunchAgent has an invalid PORT value."
PORT_NUMBER=$((10#$PORT_VALUE))
(( PORT_NUMBER > 0 && PORT_NUMBER <= 65535 )) || error "The LaunchAgent PORT must be between 1 and 65535."
PORT_VALUE="$PORT_NUMBER"

/usr/bin/plutil -lint "$TEMP_PLIST" >/dev/null ||
  error "The generated LaunchDaemon plist failed validation."

if [[ -f "$SYSTEM_PLIST" ]]; then
  EXISTING_LABEL="$(/usr/libexec/PlistBuddy -c 'Print :Label' "$SYSTEM_PLIST" 2>/dev/null || true)"
  [[ "$EXISTING_LABEL" == "$BOOT_LABEL" ]] ||
    error "Refusing to replace an unrelated plist at $SYSTEM_PLIST."
  BACKUP_PLIST="$SYSTEM_PLIST.backup.$(/bin/date +%Y%m%d%H%M%S)"
  /bin/cp -p "$SYSTEM_PLIST" "$BACKUP_PLIST"
  /bin/launchctl bootout "system/$BOOT_LABEL" >/dev/null 2>&1 || true
fi

# Boot out the login service before installing the system service so both
# processes cannot contend for the same port. Keep its plist for easy rollback.
if /bin/launchctl print "gui/$RUN_UID/$AGENT_LABEL" >/dev/null 2>&1; then
  /bin/launchctl bootout "gui/$RUN_UID/$AGENT_LABEL"
  AGENT_STOPPED=1
fi
for _ in $(/usr/bin/seq 1 15); do
  if ! /usr/sbin/lsof -nP -iTCP:"$PORT_VALUE" -sTCP:LISTEN >/dev/null 2>&1; then
    break
  fi
  /bin/sleep 1
done
if /usr/sbin/lsof -nP -iTCP:"$PORT_VALUE" -sTCP:LISTEN >/dev/null 2>&1; then
  error "Port $PORT_VALUE is still in use after stopping the LaunchAgent; stop that process and retry."
fi
/bin/mv "$AGENT_PLIST" "$AGENT_BACKUP"
AGENT_DISABLED=1

/usr/sbin/chown root:wheel "$TEMP_PLIST"
/bin/chmod 0644 "$TEMP_PLIST"
NEW_PLIST_INSTALLED=1
/bin/mv -f "$TEMP_PLIST" "$SYSTEM_PLIST"

/usr/bin/pmset -a sleep 0 disksleep 0 autorestart 1
/bin/launchctl bootstrap system "$SYSTEM_PLIST"

info "Installed $SYSTEM_PLIST to run as $RUN_USER at system startup."
info "The former LaunchAgent is preserved at $AGENT_BACKUP."
info "Check the app health endpoint and logs after boot:"
info "  http://127.0.0.1:$PORT_VALUE/health"
info "  $LOG_PATH"
cat <<'NOTICE'

FileVault note: this does not configure automatic login or bypass FileVault.
After a restart, the startup disk must first be unlocked at the FileVault
preboot screen. Once macOS boots, the LaunchDaemon can start before the user
logs in. Plex Media Server has its own startup behavior and may not be
available until its own process starts; this installer does not change that.
NOTICE
