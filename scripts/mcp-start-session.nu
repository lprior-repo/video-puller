#!/usr/bin/env nu
# Start an MCP agent-mail session for this project

def main [
    --agent-name: string = ""            # Agent name (leave empty to auto-generate)
    --task: string = "Gleam development" # Task description
] {
    print "🚀 Starting MCP agent session..."
    print ""

    let project_path = (pwd | path expand)

    print "📍 Project Information:"
    print $"   Path: ($project_path)"
    print $"   Task: ($task)"
    if $agent_name != "" {
        print $"   Agent: ($agent_name)"
    } else {
        print "   Agent: (will auto-generate)"
    }
    print ""

    print "🔧 Session Workflow:"
    print ""
    print "1. Use macro_start_session to initialize everything at once:"
    print ""
    print "   Parameters:"
    print $"   - human_key: ($project_path)"
    print "   - program: claude-code"
    print "   - model: sonnet-4.5"
    print $"   - task_description: ($task)"
    if $agent_name != "" {
        print $"   - agent_name: ($agent_name)"
    }
    print "   - inbox_limit: 10"
    print ""
    print "2. Or use individual steps:"
    print "   a. ensure_project"
    print "   b. register_agent"
    print "   c. file_reservation_paths (for Gleam files)"
    print "   d. fetch_inbox"
    print ""
    print "💡 Use Claude Code's MCP tools to execute these operations"
    print "📚 See .claude/docs/mcp-agent-setup.md for examples"
}
