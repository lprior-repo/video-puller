# video_puller

A Gleam project scaffold with comprehensive tooling for modern development workflows.

[![Package Version](https://img.shields.io/hexpm/v/video_puller)](https://hex.pm/packages/video_puller)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/video_puller/)

## Features

- 🎯 Idiomatic Gleam project structure
- 🔧 Task runner with `just`
- 🤖 Claude Code integration with custom slash commands
- 📬 MCP (Model Context Protocol) server support
- 🐚 Nushell scripts for common workflows
- ✅ CI/CD ready with GitHub Actions
- 📝 Comprehensive documentation

## Quick Start

### Prerequisites

- [Gleam](https://gleam.run/getting-started/installing/) >= 1.0.0
- [just](https://github.com/casey/just) (optional, for task running)
- [Nushell](https://www.nushell.sh/) (optional, for scripts)

### Installation

```sh
gleam add video_puller@1
```

### Basic Usage

```gleam
import video_puller

pub fn main() -> Nil {
  video_puller.main()
}
```

## Development

### Using Just (Recommended)

```sh
just --list         # Show all available commands
just setup          # Initial project setup
just dev            # Run in watch mode
just test           # Run tests
just ci             # Run all CI checks
```

### Using Gleam Directly

```sh
gleam run           # Run the project
gleam test          # Run the tests
gleam format        # Format code
gleam check         # Type check
```

### Using Nushell Scripts

```sh
nu scripts/setup.nu       # Initial setup
nu scripts/dev.nu         # Run development checks
nu scripts/watch.nu       # Watch mode
nu scripts/clean.nu       # Clean build artifacts
nu scripts/deps-update.nu # Update dependencies
nu scripts/docs-serve.nu  # Serve documentation locally
```

## Project Structure

```
video-puller/
├── .claude/              # Claude Code configuration
│   ├── commands/         # Custom slash commands
│   ├── mcp/             # MCP server configuration
│   └── CLAUDE.md        # Project instructions for Claude
├── scripts/             # Nushell automation scripts
├── src/                 # Source code
├── test/                # Tests
├── justfile            # Task runner configuration
└── gleam.toml          # Project manifest
```

## Claude Code Integration

This project is optimized for use with Claude Code. Available slash commands:

### Development Commands
- `/test` - Run tests and fix failures
- `/build` - Build and fix compilation errors
- `/format` - Format all code
- `/check` - Type check and fix errors
- `/add-test` - Add comprehensive tests
- `/refactor` - Refactor to idiomatic Gleam
- `/deps` - Analyze dependencies
- `/ci` - Run all CI checks

### Setup & Integration Commands
- `/setup-beads` - Research and set up Steve Yegge's beads system
- `/setup-mcp-agent` - Research and register project with MCP agent-mail server
- `/research` - Conduct thorough research on any topic

See `.claude/commands/` for all available commands.

## MCP Server Support

This project supports Model Context Protocol for agent coordination:

- **Agent Mail**: Coordinate between multiple AI agents
- **File Reservations**: Prevent edit conflicts
- **Message Threading**: Organize agent communications
- **Build Slots**: Manage concurrent build operations

### Quick Start with MCP

```bash
# Display setup checklist
just setup-mcp

# Start an MCP session (use in Claude Code)
just mcp-start

# Reserve files for editing
just mcp-reserve YourAgentName
```

See `.claude/docs/mcp-agent-setup.md` for comprehensive setup instructions.

## Beads Integration

This project uses [beads](https://github.com/steveyegge/beads) - a lightweight, distributed issue tracker designed for AI coding agents.

### Quick Start

```bash
# Show ready work (unblocked issues)
just beads-ready

# Create a new issue
just beads-create "Issue title" task 1

# Update issue status
just beads-update bd-a1b2 in_progress

# Show database status
just beads-status

# Sync with git
just beads-sync
```

### Nushell Scripts

```bash
nu scripts/beads-ready.nu      # Check ready work with nice formatting
nu scripts/beads-create.nu     # Interactive issue creation
nu scripts/beads-update.nu     # Update issue fields
nu scripts/beads-sync.nu       # Sync and optionally commit
nu scripts/beads-status.nu     # Comprehensive status overview
```

### Features

- 🎯 **Zero-Setup Distributed Database** - Git-backed, no server required
- 🔗 **Four Dependency Types** - blocks, related, parent-child, discovered-from
- 🆔 **Hash-Based IDs** - Collision-resistant (e.g., `bd-a1b2`)
- ✅ **Ready Work Detection** - Automatically finds unblocked issues
- 📊 **JSON Output** - All commands support `--json` for agents

### Essential Commands

```bash
bd ready                        # Show unblocked issues
bd create "Task" -t feature    # Create new issue
bd update bd-a1b2 --status in_progress  # Claim work
bd close bd-a1b2 --reason "Done"       # Complete issue
bd sync                        # Force git sync
```

See `.claude/docs/beads-setup.md` for comprehensive documentation and AGENTS.md for workflow guidelines.

## Testing

```sh
# Run all tests
just test

# Watch mode
just test-watch

# Or with Nushell
nu scripts/watch.nu
```

## Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Run `just ci` to ensure all checks pass
5. Submit a pull request

## Code Style

This project follows idiomatic Gleam conventions:

- Use pattern matching over conditionals
- Prefer `Result` and `Option` types
- Keep functions pure when possible
- Write comprehensive tests
- Document public APIs

See `.claude/CLAUDE.md` for detailed guidelines.

## Documentation

Generate and view documentation:

```sh
just docs

# Or serve locally
nu scripts/docs-serve.nu
```

Further documentation can be found at <https://hexdocs.pm/video_puller>.

## License

This project is licensed under the Apache License 2.0.

## Resources

- [Gleam Language](https://gleam.run/)
- [Gleam Standard Library](https://hexdocs.pm/gleam_stdlib/)
- [Claude Code](https://claude.ai/claude-code)
- [Just Task Runner](https://just.systems/)
- [Nushell](https://www.nushell.sh/)
