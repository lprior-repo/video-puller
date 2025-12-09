# Beads Setup Documentation

This document provides comprehensive information about the beads issue tracking system configured for this project.

## What is Beads?

Beads (command: `bd`) is a lightweight, distributed issue tracker designed specifically for AI coding agents. Created by [Steve Yegge](https://github.com/steveyegge), it functions as a "memory upgrade" that enables agents to manage complex, multi-step tasks effectively across sessions and machines.

### Key Insight

Beads solves the fundamental problem of AI agents losing context between sessions. Instead of scattered markdown files and forgotten TODOs, beads provides a structured, git-backed database that agents can query and update programmatically.

## Core Features

### 1. Zero-Setup Distributed Database

Beads operates as a distributed database synchronized through git:
- **Storage**: JSONL format in `.beads/issues.jsonl` (committed to git)
- **Cache**: Local SQLite for fast queries (<100ms)
- **Sync**: Automatic git-based synchronization across machines
- **No Infrastructure**: No centralized server, API keys, or external services required

### 2. Four Dependency Types

Beads employs four relationship categories to structure work:

1. **Blocks** — Issue A must complete before Issue B can proceed
2. **Related** — Associated work without sequential requirements
3. **Parent-Child** — Hierarchical decomposition (epics to tasks)
4. **Discovered-From** — Traceability linking newly-found work to its origin

### 3. Hash-Based Collision-Resistant IDs

Version 0.20.1+ uses collision-resistant hash IDs:
- **Format**: 4-6 character hexadecimal (e.g., `bd-a1b2`, `bd-f14c`)
- **Progressive scaling**: Length increases with database size
- **Hierarchical children**: Support for nested IDs (`bd-a3f8e9.1`)
- **Benefits**: Eliminates merge conflicts from concurrent branch creation

### 4. Ready Work Detection

Automatically identifies issues with no blocking dependencies, enabling agents to:
- Query `bd ready --json` to find actionable work
- Avoid manual dependency graph traversal
- Stay productive across session boundaries

### 5. Agent-Friendly JSON Output

All commands support `--json` flag for programmatic integration:
```bash
bd ready --json
bd create "Task" -p 1 --json
bd show bd-a1b2 --json
```

## Installation

Beads is already installed in this project. Current version: **0.29.0**

### Installation Methods (for reference)

**Homebrew (macOS/Linux):**
```bash
brew tap steveyegge/beads
brew install bd
```

**Go Install:**
```bash
go install github.com/steveyegge/beads/cmd/bd@latest
```

**Script (all platforms):**
```bash
curl -fsSL https://raw.githubusercontent.com/steveyegge/beads/main/scripts/install.sh | bash
```

**Windows:**
```powershell
irm https://raw.githubusercontent.com/steveyegge/beads/main/install.ps1 | iex
```

## Project Configuration

### Initialized Files

- `.beads/beads.db` - SQLite cache (gitignored)
- `.beads/issues.jsonl` - Git-synced issue storage (committed)
- `.beads/config.yaml` - Project configuration
- `.beads/bd.sock` - Daemon socket (gitignored)

### Configuration Settings

Key settings in `.beads/config.yaml`:

```yaml
# Sync branch for beads commits
sync-branch: "beads-sync"

# Auto-start daemon if not running
auto-start-daemon: true

# Debounce interval for auto-flush
flush-debounce: "5s"
```

### Git Hooks Installed

Beads automatically installed the following git hooks:
- **pre-commit**: Validates issue state before commit
- **post-merge**: Auto-imports from JSONL after git pull
- **pre-push**: Ensures JSONL is synced before push
- **post-checkout**: Updates local cache after branch switches

## CLI Command Reference

### Essential Commands

**Check for ready work:**
```bash
bd ready                    # List unblocked issues
bd ready --json            # JSON format for agents
```

**Create issues:**
```bash
bd create "Issue title"                              # Basic issue
bd create "Task" -t feature -p 1                    # With type and priority
bd create "Bug fix" -t bug -p 0 --json             # Critical bug (JSON output)
bd create "Subtask" --parent bd-a1b2               # Child of epic
bd create "Follow-up" --deps discovered-from:bd-123 # Link to origin
```

**Update issues:**
```bash
bd update bd-a1b2 --status in_progress             # Claim work
bd update bd-a1b2 --priority 1                     # Raise priority
bd update bd-a1b2 --assignee "AgentName"          # Assign to agent
bd update bd-a1b2 --json                           # JSON output
```

**Close issues:**
```bash
bd close bd-a1b2 --reason "Completed"              # Mark done
bd close bd-a1b2 bd-b3c4 --reason "Fixed"         # Close multiple
```

**View issues:**
```bash
bd list                                            # List all open issues
bd list --status open --priority 1                # Filter by status/priority
bd show bd-a1b2                                   # Show detailed info
bd show bd-a1b2 --json                            # JSON format
```

**Search and filter:**
```bash
bd search "authentication"                         # Full-text search
bd blocked                                         # Show blocked issues
bd stale --days 30                                # Find stale issues
```

**Dependency management:**
```bash
bd dep tree bd-a1b2                               # Visualize dependencies
bd dep add bd-a1b2 --blocks bd-b3c4              # Add dependency
bd dep remove bd-a1b2 --blocks bd-b3c4           # Remove dependency
```

**Sync operations:**
```bash
bd sync                                           # Force immediate sync
bd status                                         # Database overview
bd doctor                                         # Health check
```

### Issue Types

- `bug` - Something broken
- `feature` - New functionality
- `task` - Work item (tests, docs, refactoring)
- `epic` - Large feature with subtasks
- `chore` - Maintenance (dependencies, tooling)

### Priority Levels

- `0` - Critical (security, data loss, broken builds)
- `1` - High (major features, important bugs)
- `2` - Medium (default, nice-to-have)
- `3` - Low (polish, optimization)
- `4` - Backlog (future ideas)

### Status Values

- `open` - Not started
- `in_progress` - Currently being worked on
- `blocked` - Cannot proceed due to dependencies
- `closed` - Completed
- `wontfix` - Decided not to implement

## Workflow for AI Agents

### Standard Workflow

1. **Check ready work**
   ```bash
   bd ready --json
   ```

2. **Claim your task**
   ```bash
   bd update <id> --status in_progress --json
   ```

3. **Work on it**
   - Implement the feature/fix
   - Write tests
   - Update documentation

4. **Discover new work?**
   Create linked issue:
   ```bash
   bd create "Found bug during implementation" -p 1 --deps discovered-from:<parent-id> --json
   ```

5. **Complete**
   ```bash
   bd close <id> --reason "Implemented and tested" --json
   ```

6. **Commit together**
   Always commit `.beads/issues.jsonl` with code changes:
   ```bash
   git add .beads/issues.jsonl src/
   git commit -m "Feature: Add authentication (bd-a1b2)"
   ```

### Session-End Protocol

Before finishing a session:

1. **File remaining work** - Create issues for discovered bugs, TODOs
2. **Run quality gates** - Tests, linters (if code changed)
3. **Sync carefully** - Reconcile local/remote issues
4. **Verify clean state** - All changes committed
5. **Choose next work** - Provide context for next session

### Multi-Agent Coordination

When multiple agents work on the same project:

1. **Claim issues** - Use `--status in_progress` to signal active work
2. **Link work** - Use `discovered-from` to track issue origins
3. **Sync frequently** - Run `bd sync` to share updates
4. **Avoid conflicts** - Check `bd list --assignee <name>` before claiming

## Integration with This Project

### Gleam-Specific Considerations

For this Gleam project, beads tracks:
- Feature development tasks
- Bug fixes and issues
- Refactoring work
- Documentation updates
- Test coverage improvements
- Dependency updates

### Just Commands

Beads integrates with the justfile:

```bash
just beads-ready        # Show ready work
just beads-status       # Database overview
just beads-sync         # Force sync with git
```

### Nushell Scripts

Beads scripts available:

```bash
nu scripts/beads-ready.nu          # Check ready work (JSON)
nu scripts/beads-create.nu         # Interactive issue creation
nu scripts/beads-update.nu         # Update issue status
nu scripts/beads-sync.nu           # Sync and commit
```

## MCP Integration

### Beads MCP Server

Install the beads MCP server for native function calls:

```bash
pip install beads-mcp
```

Configure in `~/.config/claude/config.json`:
```json
{
  "mcpServers": {
    "beads": {
      "command": "beads-mcp",
      "args": []
    }
  }
}
```

### Available MCP Functions

- `mcp__beads__ready()` - Get ready work
- `mcp__beads__create()` - Create new issue
- `mcp__beads__update()` - Update issue
- `mcp__beads__close()` - Close issue
- `mcp__beads__show()` - Show issue details
- `mcp__beads__list()` - List issues with filters

## Advanced Features

### Hierarchical Issues

Create epic/subtask relationships:

```bash
# Create epic
bd create "Authentication system" -t epic -p 1
# Output: bd-a1b2

# Create subtasks
bd create "Add login endpoint" --parent bd-a1b2
bd create "Add session management" --parent bd-a1b2
bd create "Add logout endpoint" --parent bd-a1b2
```

Subtasks get IDs like `bd-a1b2.1`, `bd-a1b2.2`, etc.

### Dependency Chains

Chain issues with various dependency types:

```bash
# Issue A blocks issue B
bd dep add bd-a1b2 --blocks bd-b3c4

# Issues are related
bd dep add bd-a1b2 --related bd-c5d6

# Track discovery origin
bd create "Refactor needed" --deps discovered-from:bd-a1b2
```

### Labels and Metadata

Add labels for categorization:

```bash
bd label add bd-a1b2 "authentication" "security"
bd label remove bd-a1b2 "security"
bd list --label "authentication"
```

### Comments

Add comments to issues:

```bash
bd comment bd-a1b2 "Implementation approach: use JWT tokens"
bd comments bd-a1b2
```

## Best Practices

### Do's ✅

- **Use bd for ALL task tracking** - No markdown TODOs
- **Always use `--json` flag** for programmatic use
- **Link discovered work** with `discovered-from` dependencies
- **Check `bd ready`** before asking "what should I work on?"
- **Commit issues.jsonl** with code changes
- **Run `bd <cmd> --help`** to discover available flags
- **Store AI planning docs** in `history/` directory

### Don'ts ❌

- **Do NOT create markdown TODO lists** - Use bd instead
- **Do NOT use external issue trackers** - Beads is the source of truth
- **Do NOT duplicate tracking systems** - Causes confusion
- **Do NOT clutter repo root** with planning documents
- **Do NOT skip committing** `.beads/issues.jsonl`

## Troubleshooting

### Common Issues

**Command not found:**
```bash
# Check if bd is in PATH
which bd
# If not, ensure Go bin is in PATH
export PATH="$PATH:$HOME/go/bin"
```

**Database corruption:**
```bash
# Run doctor to diagnose
bd doctor

# Reimport from JSONL
bd sync
```

**Merge conflicts in issues.jsonl:**
```bash
# Beads includes a smart merge driver
# Conflicts should be rare, but if they occur:
git mergetool

# Or manually resolve and run:
bd validate
```

**Daemon not starting:**
```bash
# Check daemon status
bd daemon status

# Force restart
bd daemon stop
bd daemon start
```

### Health Checks

Run comprehensive diagnostics:

```bash
bd doctor                          # Full health check
bd validate                        # Validate database integrity
bd info                           # Database information
```

## Resources

### Official Documentation

- **Repository**: [github.com/steveyegge/beads](https://github.com/steveyegge/beads)
- **Installation Guide**: [docs/INSTALLING.md](https://github.com/steveyegge/beads/blob/main/docs/INSTALLING.md)
- **Blog Post**: [Beads for Blobfish](https://steve-yegge.medium.com/beads-for-blobfish-80c7a2977ffa)
- **MCP Server**: [integrations/beads-mcp/](https://github.com/steveyegge/beads/tree/main/integrations/beads-mcp)

### Related Tools

- **Beads Viewer**: [github.com/Dicklesworthstone/beads_viewer](https://github.com/Dicklesworthstone/beads_viewer) - TUI for viewing issues
- **Beads UI**: [github.com/mantoni/beads-ui](https://github.com/mantoni/beads-ui) - Local web UI for beads

### Community

- **Discussions**: GitHub Discussions on steveyegge/beads
- **Issues**: GitHub Issues for bug reports and feature requests
- **Twitter**: [@Steve_Yegge](https://x.com/Steve_Yegge) for updates

## Version Information

- **Beads Version**: 0.29.0
- **Installation Date**: 2025-12-09
- **Configuration**: sync-branch enabled, hooks installed
- **Database Format**: Hash-based IDs (v0.20.1+)

---

**Summary**: Beads is now fully configured for this project. Use `bd ready --json` to find work, create issues with `bd create`, and always commit `.beads/issues.jsonl` with your code changes. For questions, see AGENTS.md or run `bd <command> --help`.
