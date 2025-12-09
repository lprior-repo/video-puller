#!/usr/bin/env nu
# Update issue status and other fields

def main [
    id: string                    # Issue ID to update
    --status (-s): string         # New status (open, in_progress, blocked, closed)
    --priority (-p): int          # New priority (0-4)
    --assignee (-a): string       # Assignee name
    --title (-t): string          # New title
] {
    print $"🔄 Updating issue (ansi cyan)($id)(ansi reset)...\n"

    # Build the command
    mut cmd = ["bd" "update" $id "--json"]

    # Add optional parameters
    if ($status | is-not-empty) {
        $cmd = ($cmd | append ["--status" $status])
        print $"  Setting status to: (ansi green)($status)(ansi reset)"
    }

    if ($priority | is-not-empty) {
        $cmd = ($cmd | append ["--priority" ($priority | into string)])
        print $"  Setting priority to: ($priority)"
    }

    if ($assignee | is-not-empty) {
        $cmd = ($cmd | append ["--assignee" $assignee])
        print $"  Assigning to: ($assignee)"
    }

    if ($title | is-not-empty) {
        $cmd = ($cmd | append ["--title" $title])
        print $"  Setting title to: ($title)"
    }

    # Execute the command
    let result = (run-external $cmd.0 ...$cmd.1.. | from json)

    print $"\n✅ Updated issue: (ansi green)($result.id)(ansi reset)"
    print $"   Title: ($result.title)"
    print $"   Status: ($result.status) | Priority: ($result.priority)"
}
