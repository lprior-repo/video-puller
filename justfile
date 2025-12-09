# Justfile for video-puller
# Run `just --list` to see all available commands

# Default recipe to display help
default:
    @just --list

# Install dependencies
deps:
    gleam deps download

# Build the project
build:
    gleam build

# Run the project
run:
    gleam run

# Run tests
test:
    gleam test

# Run tests in watch mode
test-watch:
    watchexec -e gleam -- gleam test

# Format code
fmt:
    gleam format

# Check formatting without making changes
fmt-check:
    gleam format --check

# Type check the project
check:
    gleam check

# Clean build artifacts
clean:
    rm -rf build

# Generate and open documentation
docs:
    gleam docs build
    gleam docs publish

# Run the REPL
repl:
    gleam shell

# Full CI check (format, check, test)
ci: fmt-check check test
    @echo "✓ All CI checks passed!"

# Development setup - install deps and run initial build
setup: deps build
    @echo "✓ Project setup complete!"

# Watch mode for development
dev:
    watchexec -e gleam -- gleam run

# Export to JavaScript target
export-js:
    gleam export javascript-prelude

# Run with Erlang target (default)
run-erlang:
    gleam run --target erlang

# Run with JavaScript target
run-javascript:
    gleam run --target javascript

# Add a new dependency
add-dep dep:
    gleam add {{dep}}

# Remove a dependency
remove-dep dep:
    gleam remove {{dep}}

# Update all dependencies
update:
    gleam update

# Publish to Hex (requires proper configuration in gleam.toml)
publish:
    gleam publish

# Setup beads for the project
setup-beads:
    nu scripts/setup-beads.nu

# Setup MCP agent-mail registration
setup-mcp:
    nu scripts/setup-mcp-agent.nu

# Start MCP agent session
mcp-start:
    nu scripts/mcp-start-session.nu

# Reserve files with MCP (requires AGENT parameter)
mcp-reserve AGENT:
    nu scripts/mcp-reserve-files.nu {{AGENT}}

# Run comprehensive development checks (includes nushell script)
dev-check:
    nu scripts/dev.nu
