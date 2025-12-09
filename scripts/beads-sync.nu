#!/usr/bin/env nu
# Sync beads database with git

def main [
    --commit (-c): bool = false   # Commit changes after sync
    --message (-m): string        # Commit message (requires --commit)
] {
    print "🔄 Syncing beads with git...\n"

    # Run bd sync
    print "Running bd sync..."
    bd sync

    # Check if there are changes to commit
    let status = (git status --porcelain .beads/issues.jsonl | str trim)

    if ($status | is-empty) {
        print "✅ No changes to commit - database is in sync"
        return
    }

    print $"\n📝 Changes detected in .beads/issues.jsonl"

    if $commit {
        let commit_msg = if ($message | is-not-empty) {
            $message
        } else {
            "Update beads issue tracker"
        }

        print $"\nCommitting changes with message: (ansi cyan)($commit_msg)(ansi reset)"
        git add .beads/issues.jsonl
        git commit -m $commit_msg

        print "✅ Changes committed"

        # Ask if user wants to push
        let should_push = (input "Push to remote? [y/N]: " | str downcase)
        if $should_push == "y" {
            print "\nPushing to remote..."
            git push
            print "✅ Pushed to remote"
        }
    } else {
        print "\n💡 Tip: Use --commit to automatically commit changes"
        print "   Or run: git add .beads/issues.jsonl && git commit -m 'Update issues'"
    }
}
