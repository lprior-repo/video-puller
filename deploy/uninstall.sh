#!/usr/bin/env bash
set -euo pipefail

# Uninstall script for video-puller systemd service
# This script must be run as root or with sudo

# Configuration
SERVICE_NAME="video-puller"
SERVICE_USER="video-puller"
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

info "Uninstalling video-puller systemd service..."

# Stop the service if running
if systemctl is-active --quiet "$SERVICE_NAME"; then
    info "Stopping service..."
    systemctl stop "$SERVICE_NAME"
fi

# Disable the service
if systemctl is-enabled --quiet "$SERVICE_NAME" 2>/dev/null; then
    info "Disabling service..."
    systemctl disable "$SERVICE_NAME"
fi

# Remove systemd service file
if [[ -f "$SYSTEMD_DIR/$SERVICE_NAME.service" ]]; then
    info "Removing systemd service file..."
    rm "$SYSTEMD_DIR/$SERVICE_NAME.service"
fi

# Reload systemd
info "Reloading systemd daemon..."
systemctl daemon-reload
systemctl reset-failed 2>/dev/null || true

# Ask about data removal
echo ""
read -p "Do you want to remove the data directory ($DATA_DIR)? [y/N] " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]; then
    if [[ -d "$DATA_DIR" ]]; then
        info "Removing data directory..."
        rm -rf "$DATA_DIR"
    fi
else
    warn "Data directory preserved at: $DATA_DIR"
fi

# Ask about installation directory removal
echo ""
read -p "Do you want to remove the installation directory ($INSTALL_DIR)? [y/N] " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]; then
    if [[ -d "$INSTALL_DIR" ]]; then
        info "Removing installation directory..."
        rm -rf "$INSTALL_DIR"
    fi
else
    warn "Installation directory preserved at: $INSTALL_DIR"
fi

# Ask about user removal
echo ""
read -p "Do you want to remove the system user ($SERVICE_USER)? [y/N] " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]; then
    if id "$SERVICE_USER" &>/dev/null; then
        info "Removing system user..."
        userdel "$SERVICE_USER" 2>/dev/null || warn "Could not remove user (may have running processes)"
    fi
else
    warn "System user preserved: $SERVICE_USER"
fi

info ""
info "Uninstall complete!"
