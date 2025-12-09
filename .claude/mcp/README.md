# MCP Server Configuration

This directory contains configuration for Model Context Protocol (MCP) servers.

## Available MCP Servers

### Agent Mail Server
The `mcp-agent-mail` server enables agent-to-agent coordination and communication.

To use it in your Claude Code sessions:

1. Ensure the agent-mail MCP server is configured in your global Claude Code settings
2. Use the agent mail tools for:
   - Project coordination
   - File reservations
   - Message passing between agents
   - Build slot management

### Setting Up Agent Mail

Add to your `~/.config/claude/config.json`:

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

## Example Usage in This Project

### Initialize Project Session
```
Start a coordinated session with:
- Project key: /home/lewis/src/video-puller
- Program: claude-code
- Model: sonnet-4.5
```

### File Reservations
When working on files, reserve them to coordinate with other agents:
```
Reserve src/video_puller.gleam for editing
```

### Message Coordination
Send messages to other agents working on the project:
```
Send status update about test coverage to other agents
```

## Best Practices

1. **Always initialize** your project session at the start
2. **Reserve files** before making significant edits
3. **Release reservations** when done
4. **Use threads** for related discussions
5. **Set appropriate policies** for agent coordination
