#!/usr/bin/env nu
# Check for ready work (unblocked issues) in beads

def main [] {
    print "🔍 Checking for ready work in beads...\n"

    # Get ready issues in JSON format
    let ready_issues = (bd ready --json | from json)

    if ($ready_issues | is-empty) {
        print "✅ No ready issues found. All work is either completed or blocked."
        return
    }

    print $"📋 Found ($ready_issues | length) ready issue(s):\n"

    # Display issues in a nice format
    $ready_issues | each { |issue|
        print $"  • (ansi green)($issue.id)(ansi reset) - ($issue.title)"
        print $"    Type: ($issue.type) | Priority: ($issue.priority) | Status: ($issue.status)"
        if ($issue.description? | is-not-empty) {
            print $"    Description: ($issue.description)"
        }
        print ""
    }

    # Show summary
    print $"\n(ansi cyan)Summary:(ansi reset)"
    print $"  Total ready: ($ready_issues | length)"
    print $"  Highest priority: (($ready_issues | sort-by priority | first).priority)"
    print $"\n💡 Tip: Use 'bd update <id> --status in_progress' to claim an issue"
}
