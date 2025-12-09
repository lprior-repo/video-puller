#!/usr/bin/env nu
# Show comprehensive beads database status

def main [] {
    print "📊 Beads Database Status\n"
    print "─" * 60

    # Get database status
    print "\n(ansi cyan)Database Overview:(ansi reset)"
    bd status

    # Get ready work count
    print "\n(ansi cyan)Ready Work:(ansi reset)"
    let ready = (bd ready --json | from json)
    print $"  Issues ready to work on: (ansi green)($ready | length)(ansi reset)"

    # Get blocked issues
    print "\n(ansi cyan)Blocked Issues:(ansi reset)"
    let blocked_output = (bd blocked | complete)
    if $blocked_output.exit_code == 0 {
        let blocked_lines = ($blocked_output.stdout | lines | where {|line| $line !~ "^No blocked" and ($line | str trim | is-not-empty)} | length)
        if $blocked_lines > 0 {
            print $"  (ansi yellow)($blocked_lines) issue(s) are blocked(ansi reset)"
        } else {
            print "  (ansi green)No blocked issues(ansi reset)"
        }
    }

    # Check for stale issues
    print "\n(ansi cyan)Stale Issues (30+ days):(ansi reset)"
    let stale_output = (bd stale --days 30 | complete)
    if $stale_output.exit_code == 0 {
        let stale_lines = ($stale_output.stdout | lines | where {|line| $line !~ "^No stale" and ($line | str trim | is-not-empty)} | length)
        if $stale_lines > 0 {
            print $"  (ansi yellow)($stale_lines) stale issue(s) found(ansi reset)"
        } else {
            print "  (ansi green)No stale issues(ansi reset)"
        }
    }

    # Git sync status
    print "\n(ansi cyan)Git Sync Status:(ansi reset)"
    let git_status = (git status --porcelain .beads/issues.jsonl | str trim)
    if ($git_status | is-empty) {
        print "  ✅ In sync with git"
    } else {
        print "  ⚠️  Uncommitted changes in issues.jsonl"
        print "     Run: git add .beads/issues.jsonl && git commit"
    }

    # Health check
    print "\n(ansi cyan)Health Check:(ansi reset)"
    let doctor = (bd doctor | complete)
    if $doctor.exit_code == 0 {
        if ($doctor.stdout | str contains "⚠") {
            print "  ⚠️  Some warnings found - run 'bd doctor' for details"
        } else {
            print "  ✅ All health checks passed"
        }
    }

    print "\n─" * 60
    print "\n💡 Tip: Use 'bd ready --json' to see actionable work"
}
