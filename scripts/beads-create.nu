#!/usr/bin/env nu
# Interactive issue creation for beads

def main [
    --title (-t): string          # Issue title
    --type: string = "task"       # Issue type (bug, feature, task, epic, chore)
    --priority (-p): int = 2      # Priority (0-4, 0=critical, 4=backlog)
    --description (-d): string    # Issue description
    --parent: string              # Parent issue ID (for subtasks)
    --blocks: string              # Issue this blocks
    --discovered-from: string     # Origin issue ID
] {
    print "📝 Creating new beads issue...\n"

    # Get title if not provided
    let issue_title = if ($title | is-empty) {
        input "Issue title: "
    } else {
        $title
    }

    # Build the command
    mut cmd = ["bd" "create" $issue_title "-t" $type "-p" ($priority | into string) "--json"]

    # Add optional parameters
    if ($description | is-not-empty) {
        $cmd = ($cmd | append ["--description" $description])
    }

    if ($parent | is-not-empty) {
        $cmd = ($cmd | append ["--parent" $parent])
    }

    # Build dependencies
    mut deps = []
    if ($blocks | is-not-empty) {
        $deps = ($deps | append $"blocks:($blocks)")
    }
    if ($discovered_from | is-not-empty) {
        $deps = ($deps | append $"discovered-from:($discovered_from)")
    }
    if ($deps | is-not-empty) {
        $cmd = ($cmd | append ["--deps" ($deps | str join ",")])
    }

    # Execute the command
    print $"Running: (ansi cyan)($cmd | str join ' ')(ansi reset)\n"
    let result = (run-external $cmd.0 ...$cmd.1.. | from json)

    print $"✅ Created issue: (ansi green)($result.id)(ansi reset)"
    print $"   Title: ($result.title)"
    print $"   Type: ($result.type) | Priority: ($result.priority)"
    print $"\n💡 Use 'bd show ($result.id) --json' to view details"
}
