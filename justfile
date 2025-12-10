# Justfile for video-puller
# Run `just --list` to see all available commands

# Default recipe to display help
default:
    @just --list

# Install dependencies
deps:
    gleam deps download

# Build the project (development)
build:
    gleam build

# Build optimized release with Erlang shipment
release:
    gleam export erlang-shipment
    @echo "✓ Release build complete!"
    @echo "  Location: build/erlang-shipment/"
    @echo "  Run with: ./build/erlang-shipment/entrypoint.sh run"

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

# Beads Commands
# Show ready work (unblocked issues)
beads-ready:
    bd ready --json

# Show all open issues
beads-list:
    bd list

# Show database status
beads-status:
    bd status

# Create a new issue (interactive)
beads-create TITLE TYPE="task" PRIORITY="2":
    bd create "{{TITLE}}" -t {{TYPE}} -p {{PRIORITY}} --json

# Update issue status
beads-update ID STATUS:
    bd update {{ID}} --status {{STATUS}} --json

# Close an issue
beads-close ID REASON="Completed":
    bd close {{ID}} --reason "{{REASON}}" --json

# Show issue details
beads-show ID:
    bd show {{ID}} --json

# Search issues
beads-search QUERY:
    bd search "{{QUERY}}"

# Show dependency tree
beads-deps ID:
    bd dep tree {{ID}}

# Show blocked issues
beads-blocked:
    bd blocked

# Show stale issues (30 days)
beads-stale:
    bd stale --days 30

# Force sync with git
beads-sync:
    bd sync

# Run beads health check
beads-doctor:
    bd doctor

# Validate database integrity
beads-validate:
    bd validate
