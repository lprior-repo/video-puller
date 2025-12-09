# Nushell Scripts

This directory contains Nushell scripts for common development tasks, including setup for beads and MCP agent coordination.

## Development Scripts

### `setup.nu`
Initial setup for new developers. Downloads dependencies, builds the project, and runs tests.

```bash
nu scripts/setup.nu
```

### `dev.nu`
Runs the full development workflow: format, type check, and test.

```bash
nu scripts/dev.nu
```

### `watch.nu`
Watch for file changes and automatically run tests. Requires `watchexec` to be installed.

```bash
nu scripts/watch.nu
```

### `clean.nu`
Clean all build artifacts and caches.

```bash
nu scripts/clean.nu
```

### `deps-update.nu`
Update all dependencies and run tests to ensure compatibility.

```bash
nu scripts/deps-update.nu
```

### `docs-serve.nu`
Generate documentation and serve it locally on port 8000.

```bash
nu scripts/docs-serve.nu
```

## Setup & Integration Scripts

### `setup-beads.nu`
Set up Steve Yegge's beads system for this project. Guides you through installation and configuration.

```bash
nu scripts/setup-beads.nu
```

See `.claude/docs/beads-setup.md` for detailed documentation.

### `setup-mcp-agent.nu`
Display checklist and instructions for registering this project with the MCP agent-mail server.

```bash
nu scripts/setup-mcp-agent.nu --task "Feature development"
```

Options:
- `--task`: Task description (default: "Gleam development")
- `--program`: Program name (default: "claude-code")
- `--model`: Model name (default: "sonnet-4.5")

See `.claude/docs/mcp-agent-setup.md` for detailed documentation.

## MCP Agent Coordination Scripts

### `mcp-start-session.nu`
Start an MCP agent-mail session with project registration and setup.

```bash
nu scripts/mcp-start-session.nu --task "Building authentication"
```

Options:
- `--agent-name`: Specific agent name (leave empty to auto-generate)
- `--task`: Task description

### `mcp-reserve-files.nu`
Reserve files in the MCP system to prevent concurrent edit conflicts.

```bash
nu scripts/mcp-reserve-files.nu YourAgentName \
  --exclusive=true \
  --ttl=7200 \
  --reason="Feature development"
```

Parameters:
- `agent_name`: Your agent name (required)
- `--exclusive`: Exclusive reservation (default: true)
- `--ttl`: Time to live in seconds (default: 7200 = 2 hours)
- `--reason`: Reason for reservation (default: "Development work")

## Requirements

- [Nushell](https://www.nushell.sh/) - A modern shell
- [Gleam](https://gleam.run/) - The Gleam compiler
- [watchexec](https://github.com/watchexec/watchexec) (optional) - For watch mode

## Why Nushell?

Nushell provides:
- Cross-platform compatibility
- Structured data handling
- Modern scripting features
- Better error messages than traditional shells
