#!/usr/bin/env nu
# Reserve files in the MCP agent-mail system

def main [
    agent_name: string,                   # Your agent name
    --exclusive: bool = true,             # Exclusive reservation
    --ttl: int = 7200,                    # Time to live in seconds (default 2 hours)
    --reason: string = "Development work" # Reason for reservation
] {
    print $"🔒 Reserving files for agent: ($agent_name)"
    print ""

    let project_path = (pwd | path expand)

    print "📋 File Patterns to Reserve:"
    print "   - src/**/*.gleam"
    print "   - test/**/*.gleam"
    print "   - gleam.toml"
    print ""
    print "⚙️  Reservation Settings:"
    print $"   - Exclusive: ($exclusive)"
    print $"   - TTL: ($ttl) seconds (({$ttl / 60}) minutes)"
    print $"   - Reason: ($reason)"
    print ""

    print "💡 To execute this reservation, use MCP tool in Claude Code:"
    print ""
    print "mcp__mcp-agent-mail__file_reservation_paths({"
    print $"  project_key: '($project_path)',"
    print $"  agent_name: '($agent_name)',"
    print "  paths: ['src/**/*.gleam', 'test/**/*.gleam', 'gleam.toml'],"
    print $"  ttl_seconds: ($ttl),"
    print $"  exclusive: ($exclusive),"
    print $"  reason: '($reason)'"
    print "})"
    print ""
    print "📚 See .claude/docs/mcp-agent-setup.md for more details"
}
