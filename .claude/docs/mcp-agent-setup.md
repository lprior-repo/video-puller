# MCP Agent-Mail Server Setup Guide

Complete guide for registering and using the MCP agent-mail server with this project.

## Overview

The MCP (Model Context Protocol) agent-mail server enables coordination between multiple AI agents working on the same project. It provides:

- **Project Registration:** Unique project identity
- **Agent Identities:** Named agents with roles
- **File Reservations:** Prevent concurrent edit conflicts
- **Message Passing:** Coordination between agents
- **Build Slots:** Manage concurrent build operations

## Prerequisites

### 1. Install MCP Agent-Mail Server

```bash
# Using uvx (recommended)
uvx mcp-agent-mail --help

# Or install globally
pip install mcp-agent-mail
```

### 2. Configure in Claude Code

Add to `~/.config/claude/config.json`:

```json
{
  "mcpServers": {
    "mcp-agent-mail": {
      "command": "uvx",
      "args": ["mcp-agent-mail"]
    }
  }
}
```

### 3. Verify Installation

In Claude Code, check that MCP tools are available:
- `mcp__mcp-agent-mail__health_check`
- `mcp__mcp-agent-mail__ensure_project`
- `mcp__mcp-agent-mail__register_agent`

## Project Setup

### Quick Setup (Recommended)

Use the macro to set up everything at once:

```javascript
mcp__mcp-agent-mail__macro_start_session({
  human_key: "/home/lewis/src/video-puller",
  program: "claude-code",
  model: "sonnet-4.5",
  task_description: "Gleam development",
  inbox_limit: 10,
  file_reservation_paths: ["src/**/*.gleam", "test/**/*.gleam"]
})
```

This will:
1. Ensure project exists
2. Register agent with auto-generated name
3. Reserve specified files
4. Fetch recent inbox messages

### Manual Setup

#### Step 1: Ensure Project

```javascript
mcp__mcp-agent-mail__ensure_project({
  human_key: "/home/lewis/src/video-puller"
})
```

**Important:** The `human_key` MUST be the absolute path to the project directory.

Response will include:
- `id`: Database ID
- `slug`: URL-safe project identifier
- `human_key`: Your project path
- `created_at`: Timestamp

#### Step 2: Register Agent

```javascript
mcp__mcp-agent-mail__register_agent({
  project_key: "/home/lewis/src/video-puller",
  program: "claude-code",
  model: "sonnet-4.5",
  task_description: "Gleam development",
  // name: "BlueLake"  // Optional - omit to auto-generate
})
```

**Agent Naming Rules:**
- Names MUST be adjective+noun format (e.g., "GreenCastle", "BlueLake")
- Names are NOT descriptive of role (NOT "BackendDeveloper")
- Best practice: Omit `name` to auto-generate
- Names are unique per project

Response includes your agent profile with:
- `id`: Agent ID
- `name`: Your unique agent name (save this!)
- `inception_ts`: When agent was created
- `last_active_ts`: Last activity timestamp

#### Step 3: Set Contact Policy

```javascript
mcp__mcp-agent-mail__set_contact_policy({
  project_key: "/home/lewis/src/video-puller",
  agent_name: "YourAgentName",
  policy: "auto"  // Options: open, auto, contacts_only, block_all
})
```

Recommended policies:
- **auto**: Automatically accept from active agents (good for solo development)
- **contacts_only**: Only accept from approved contacts (good for teams)
- **open**: Accept from anyone
- **block_all**: No coordination

## File Reservations

### Reserve Files

Before editing files, reserve them to signal intent:

```javascript
mcp__mcp-agent-mail__file_reservation_paths({
  project_key: "/home/lewis/src/video-puller",
  agent_name: "YourAgentName",
  paths: [
    "src/**/*.gleam",
    "test/**/*.gleam",
    "gleam.toml"
  ],
  ttl_seconds: 7200,  // 2 hours
  exclusive: true,
  reason: "Implementing new feature"
})
```

**Response:**
- `granted`: List of successful reservations
- `conflicts`: Any conflicting reservations with holder info

### Using Nushell Script

```bash
nu scripts/mcp-reserve-files.nu YourAgentName \
  --exclusive=true \
  --ttl=7200 \
  --reason="Feature development"
```

### Renew Reservations

```javascript
mcp__mcp-agent-mail__renew_file_reservations({
  project_key: "/home/lewis/src/video-puller",
  agent_name: "YourAgentName",
  extend_seconds: 3600  // Add 1 more hour
})
```

### Release Reservations

```javascript
mcp__mcp-agent-mail__release_file_reservations({
  project_key: "/home/lewis/src/video-puller",
  agent_name: "YourAgentName"
  // Omit paths to release all
})
```

## Agent Communication

### Send a Message

```javascript
mcp__mcp-agent-mail__send_message({
  project_key: "/home/lewis/src/video-puller",
  sender_name: "YourAgentName",
  to: ["OtherAgentName"],
  subject: "Test coverage discussion",
  body_md: "## Current Status\n\nTest coverage is at 85%...",
  importance: "normal"  // low, normal, high, urgent
})
```

### Reply to a Message

```javascript
mcp__mcp-agent-mail__reply_message({
  project_key: "/home/lewis/src/video-puller",
  message_id: 1234,
  sender_name: "YourAgentName",
  body_md: "Agreed. Let's target 90% coverage."
})
```

### Check Inbox

```javascript
mcp__mcp-agent-mail__fetch_inbox({
  project_key: "/home/lewis/src/video-puller",
  agent_name: "YourAgentName",
  limit: 20,
  urgent_only: false,
  include_bodies: true
})
```

### Acknowledge Message

```javascript
mcp__mcp-agent-mail__acknowledge_message({
  project_key: "/home/lewis/src/video-puller",
  agent_name: "YourAgentName",
  message_id: 1234
})
```

## Thread Management

### Search Messages

```javascript
mcp__mcp-agent-mail__search_messages({
  project_key: "/home/lewis/src/video-puller",
  query: "test coverage AND gleam",
  limit: 50
})
```

### Summarize Thread

```javascript
mcp__mcp-agent-mail__summarize_thread({
  project_key: "/home/lewis/src/video-puller",
  thread_id: "feature-123",
  include_examples: true,
  llm_mode: true
})
```

## Build Slots

### Acquire Build Slot

```javascript
mcp__mcp-agent-mail__acquire_build_slot({
  project_key: "/home/lewis/src/video-puller",
  agent_name: "YourAgentName",
  slot: "default",
  exclusive: true,
  ttl_seconds: 3600
})
```

### Release Build Slot

```javascript
mcp__mcp-agent-mail__release_build_slot({
  project_key: "/home/lewis/src/video-puller",
  agent_name: "YourAgentName",
  slot: "default"
})
```

## Workflows

### Solo Development Workflow

1. **Start Session:**
   ```bash
   nu scripts/mcp-start-session.nu --task="Adding authentication"
   ```

2. **Reserve Files:**
   ```bash
   nu scripts/mcp-reserve-files.nu YourAgentName
   ```

3. **Work on Code**

4. **Release Reservations** (when done)

5. **Check Inbox** (periodically for any coordination messages)

### Multi-Agent Workflow

1. **Agent A:** Registers and reserves `src/auth/*.gleam`
2. **Agent B:** Registers and reserves `src/api/*.gleam`
3. **Agent A:** Sends message to Agent B about shared types
4. **Agent B:** Replies and acknowledges
5. **Both:** Work without conflicts
6. **Both:** Release reservations when done

### Code Review Workflow

1. **Developer Agent:** Completes feature, sends message to Reviewer
2. **Reviewer Agent:** Registers, reads code, sends feedback
3. **Developer Agent:** Makes changes, replies
4. **Reviewer Agent:** Approves, acknowledges

## Best Practices

### File Reservations

✅ **Do:**
- Reserve files before significant edits
- Use specific patterns (e.g., `src/auth/*.gleam`)
- Set realistic TTLs (1-2 hours for active work)
- Release when done or switching tasks
- Check for conflicts before forcing

❌ **Don't:**
- Reserve entire repository (`**/*`)
- Hold reservations when not actively working
- Ignore conflicts
- Use very long TTLs (>8 hours)

### Messaging

✅ **Do:**
- Use clear, specific subjects
- Keep threads focused on one topic
- Acknowledge important messages
- Use appropriate importance levels
- Include context in messages

❌ **Don't:**
- Broadcast to all agents unnecessarily
- Change topics mid-thread
- Leave urgent messages unacknowledged
- Send large attachments repeatedly

### Agent Naming

✅ **Do:**
- Let system auto-generate names
- Use adjective+noun format if specifying
- Keep names memorable and short

❌ **Don't:**
- Use descriptive role names
- Use spaces or special characters
- Reuse names across projects

## Troubleshooting

### Project Not Found

**Issue:** `project_key` doesn't match
**Solution:** Use exact absolute path: `/home/lewis/src/video-puller`

### Agent Name Conflict

**Issue:** Agent name already in use
**Solution:**
- Omit `name` parameter to auto-generate
- Choose a different adjective+noun combination

### File Reservation Conflict

**Issue:** Files already reserved by another agent
**Solution:**
- Check who holds the reservation
- Wait for expiry or contact the agent
- Use `force_release_file_reservation` if agent is inactive

### Message Not Received

**Issue:** Messages not showing in inbox
**Solution:**
- Verify recipient agent name is correct
- Check contact policy settings
- Use `search_messages` to find messages

## Helper Commands

Add to your workflow:

### Justfile Commands

```just
# Start MCP session
mcp-start:
    nu scripts/mcp-start-session.nu

# Reserve common files
mcp-reserve AGENT:
    nu scripts/mcp-reserve-files.nu {{AGENT}}

# Setup MCP agent
mcp-setup:
    nu scripts/setup-mcp-agent.nu
```

### Slash Commands

Use in Claude Code:
- `/setup-mcp-agent` - Full guided setup
- Check inbox and coordinate with other agents as needed

## Resources

- **MCP Specification:** [Model Context Protocol](https://modelcontextprotocol.io)
- **Agent-Mail Docs:** Check server documentation
- **This Project:** `.claude/mcp/README.md`

## Quick Reference

### Project Details
- **Project Path:** `/home/lewis/src/video-puller`
- **Project Key:** (same as path)
- **Agent Name:** (generated during registration)

### Common File Patterns
- All Gleam source: `src/**/*.gleam`
- All tests: `test/**/*.gleam`
- Config: `gleam.toml`
- Scripts: `scripts/*.nu`

### Important Notes

1. Always use absolute paths for `human_key`/`project_key`
2. Save your agent name after registration
3. Set contact policy early
4. Reserve files before editing
5. Release reservations when done
6. Check inbox periodically

---

**Getting Started:**

Run `/setup-mcp-agent` in Claude Code for an interactive setup experience.
