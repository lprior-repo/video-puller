#!/usr/bin/env nu
# Register this project with MCP agent-mail server

def main [
    --task: string = "Gleam development"  # Task description
    --program: string = "claude-code"     # Program name
    --model: string = "sonnet-4.5"        # Model name
] {
    print "📬 Setting up MCP agent-mail for video-puller..."
    print ""

    let project_path = (pwd | path expand)
    print $"Project path: ($project_path)"
    print ""

    # Note: This script provides the workflow
    # Actual MCP operations should be done through Claude Code's MCP tools

    print "📋 MCP Agent Setup Checklist:"
    print ""
    print "1. ✓ Ensure MCP agent-mail server is configured in ~/.config/claude/config.json"
    print "2. □ Call ensure_project with human_key: ($project_path)"
    print "3. □ Call register_agent with:"
    print $"   - project_key: ($project_path)"
    print $"   - program: ($program)"
    print $"   - model: ($model)"
    print $"   - task_description: ($task)"
    print "4. □ Set contact policy (recommended: 'auto')"
    print "5. □ Reserve files: src/**/*.gleam, test/**/*.gleam"
    print "6. □ Document agent name and project key"
    print ""
    print "📝 To execute these steps, use Claude Code with MCP tools:"
    print ""
    print "Example MCP tool calls:"
    print $"  mcp__mcp-agent-mail__ensure_project({human_key: '($project_path)'})"
    print $"  mcp__mcp-agent-mail__register_agent({project_key: '($project_path)', program: '($program)', model: '($model)', task_description: '($task)'})"
    print ""
    print "💡 Or use the /setup-mcp-agent slash command in Claude Code for guided setup"
    print ""
    print "📚 See .claude/docs/mcp-agent-setup.md for detailed documentation"
}
