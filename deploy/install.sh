#!/usr/bin/env bash
set -euo pipefail

umask 022


# Installation script for video-puller systemd service
# This script must be run as root or with sudo

# Configuration
SERVICE_NAME="video-puller"
SERVICE_USER="video-puller"
SERVICE_GROUP="video-puller"
INSTALL_DIR="/opt/video-puller"
DATA_DIR="/var/lib/video-puller"
SYSTEMD_DIR="/etc/systemd/system"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

error() {
    echo -e "${RED}[ERROR]${NC} $1"
    exit 1
}

# Check if running as root
if [[ $EUID -ne 0 ]]; then
   error "This script must be run as root (use sudo)"
fi

info "Installing video-puller systemd service..."

# Create service user if it doesn't exist
if id "$SERVICE_USER" &>/dev/null; then
    info "User $SERVICE_USER already exists"
else
    info "Creating system user: $SERVICE_USER"
    useradd --system --no-create-home --shell /usr/sbin/nologin "$SERVICE_USER"
fi

# Create installation directory
if [[ -d "$INSTALL_DIR" ]]; then
    warn "Installation directory $INSTALL_DIR already exists"
else
    info "Creating installation directory: $INSTALL_DIR"
    mkdir -p "$INSTALL_DIR"
fi

# Create data directory
info "Creating data directory: $DATA_DIR"
mkdir -p "$DATA_DIR/library"

# Plex should use ${DATA_DIR}/library as its TV Shows library. The service
# account and Plex need no shared login; Plex only needs filesystem read access.
chmod 711 "$DATA_DIR"
chmod -R 755 "$DATA_DIR/library"
# ytdl-sub is an administrator-managed dependency; install it with the
# package manager or pipx of your choice. The systemd unit supplies its PATH.

# Check if Gleam is installed
if ! command -v gleam &> /dev/null; then
    warn "Gleam is not installed in /usr/bin/gleam"
    warn "Please ensure Gleam is installed and accessible at /usr/bin/gleam"
    warn "or update the ExecStart path in the service file"
fi

# Copy project files to installation directory
info "Copying project files to $INSTALL_DIR..."
if [[ -f "gleam.toml" ]]; then
    # We're in the project directory
    cp -r . "$INSTALL_DIR/"
    # Remove development files
    rm -rf "$INSTALL_DIR/.git" "$INSTALL_DIR/data" 2>/dev/null || true
else
    error "gleam.toml not found. Please run this script from the project root directory"
fi
# Set ownership
info "Setting ownership..."
chown -R "$SERVICE_USER:$SERVICE_GROUP" "$INSTALL_DIR"
chown -R "$SERVICE_USER:$SERVICE_GROUP" "$DATA_DIR"

# With a 711 data root the database filename remains reachable; keep its
# contents private.
find "$DATA_DIR" -maxdepth 1 -name 'video_eater.db*' -exec chmod 600 {} + 2>/dev/null || true

# Build the project release
info "Building the project release..."
cd "$INSTALL_DIR"
sudo -u "$SERVICE_USER" gleam export erlang-shipment || error "Failed to build release"

# Install systemd service file
info "Installing systemd service file..."
cp "$INSTALL_DIR/deploy/video-puller.service" "$SYSTEMD_DIR/$SERVICE_NAME.service"

# Reload systemd
info "Reloading systemd daemon..."
systemctl daemon-reload

# Enable service
info "Enabling service to start on boot..."
systemctl enable "$SERVICE_NAME"

info ""
info "Installation complete!"
info ""
info "Next steps:"
info "  1. Review and edit the service configuration if needed:"
info "     $SYSTEMD_DIR/$SERVICE_NAME.service"
info ""
info "  2. Start the service:"
info "     sudo systemctl start $SERVICE_NAME"
info ""
info "  3. Check service status:"
info "     sudo systemctl status $SERVICE_NAME"
info ""
info "  4. View logs:"
info "     sudo journalctl -u $SERVICE_NAME -f"
info ""
info "The application will be accessible at http://localhost:8080"
info "Database location: $DATA_DIR/video_eater.db"
