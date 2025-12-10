# Release Build Configuration

This document describes the release build configuration for the video-puller project.

## Build Configuration

The project is configured to target the **Erlang** runtime:

- **Target**: `erlang` (specified in `gleam.toml`)
- **Build Type**: Erlang shipment (self-contained release)
- **Gleam Version**: 1.13.0
- **Erlang/OTP Version**: 26.x (minimum 25.0)

## Build Commands

### Development Build

For local development and testing:

```bash
just build
# or
gleam build
```

This creates a development build in `build/dev/erlang/`.

### Release Build

For production deployment:

```bash
just release
# or
gleam export erlang-shipment
```

This creates an optimized release in `build/erlang-shipment/` with:
- All compiled BEAM files
- All dependencies bundled
- Entry scripts for POSIX shell and PowerShell
- Static assets symlinked

## Runtime Dependencies

The production system requires:

### Required

1. **Erlang/OTP** (>= 25.0, tested with 26.x)
   - Only the runtime is needed, not the full development environment
   - The release bundles all BEAM files, so no Gleam installation needed

2. **yt-dlp** (latest version recommended)
   - Install via pip: `pip install yt-dlp`
   - Or package manager: `apt install yt-dlp` or `pacman -S yt-dlp`
   - Used for downloading videos from supported platforms

### Optional

3. **FFmpeg**
   - Install via package manager: `apt install ffmpeg` or `pacman -S ffmpeg`
   - Used by yt-dlp for video post-processing and format conversion

## Running the Release

The release can be run in several ways:

### Using the Entry Script

```bash
./build/erlang-shipment/entrypoint.sh run
```

### Using Systemd (Production)

The project includes systemd service files in `deploy/`:

```bash
# Install (as root)
cd deploy
sudo ./install.sh

# Manage service
sudo systemctl start video-puller
sudo systemctl status video-puller
sudo systemctl stop video-puller
```

See `deploy/README.txt` for detailed deployment instructions.

## Distribution

The `build/erlang-shipment/` directory is self-contained and can be:

1. Copied to any compatible Linux system with Erlang installed
2. Archived for distribution: `tar -czf video-puller-release.tar.gz build/erlang-shipment/`
3. Deployed via the install script in `deploy/`

### Compatibility

The release is compatible with:
- Linux x86_64 (primary target)
- Any system with Erlang/OTP 25.0+ installed
- Both development and production environments

## Environment Variables

The application respects these environment variables:

- `PORT` - HTTP server port (default: 8080)
- `DB_PATH` - SQLite database location (default: `data/video_eater.db`)
- `STATIC_DIR` - Static assets directory (default: `priv/static`)
- `SECRET_KEY` - Session secret key (auto-generated if not set)

Set these in the systemd service file or shell environment.

## Build Output

Release build produces:

```
build/erlang-shipment/
├── entrypoint.sh          # POSIX shell entry script
├── entrypoint.ps1         # PowerShell entry script
├── video_puller/          # Main application
│   ├── ebin/             # Compiled BEAM files
│   ├── include/          # Header files
│   └── priv/             # Static assets (symlink)
├── gleam_stdlib/         # Bundled dependencies
├── mist/
├── wisp/
└── ... (all dependencies)
```

## Troubleshooting

### Build Fails

- Ensure Gleam 1.13.0+ is installed
- Run `gleam clean` and try again
- Check `gleam check` for type errors

### Runtime Fails

- Verify Erlang/OTP 25.0+ is installed: `erl -version`
- Check yt-dlp is available: `yt-dlp --version`
- Review logs: `sudo journalctl -u video-puller -f`
- Ensure database directory has write permissions

## CI/CD Integration

The release build is suitable for CI/CD pipelines:

```bash
# In CI pipeline
gleam export erlang-shipment
tar -czf video-puller-release.tar.gz build/erlang-shipment/
# Upload artifact
```

See `.github/workflows/` for GitHub Actions integration.
