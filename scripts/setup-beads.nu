#!/usr/bin/env nu
# Setup Steve Yegge's beads for this project

def main [] {
    print "🔮 Setting up beads for video-puller..."
    print ""

    # Check if bd (beads) is installed
    print "📦 Checking for bd (beads) installation..."
    let bd_path = (which bd | get path.0? | default "")

    if ($bd_path | is-empty) {
        print "⚠️  bd (beads) is not installed"
        print ""
        print "Would you like to see installation instructions? (y/n)"
        let response = (input)

        if $response == "y" {
            print "\n📋 Installation methods:"
            print "\n1. Homebrew (macOS/Linux):"
            print "   brew tap steveyegge/beads"
            print "   brew install bd"
            print "\n2. Go install:"
            print "   go install github.com/steveyegge/beads/cmd/bd@latest"
            print "\n3. Installation script:"
            print "   curl -fsSL https://raw.githubusercontent.com/steveyegge/beads/main/scripts/install.sh | bash"
            print "\nSee .claude/docs/beads-setup.md for more details"
            exit 1
        } else {
            print "❌ bd (beads) is required for this setup"
            exit 1
        }
    }

    let version = (bd version | parse "bd version {version} ({commit})" | get version.0)
    print $"✓ bd (beads) is installed: version ($version)"

    # Initialize beads in the project
    print "\n🔧 Initializing beads..."
    if not (".beads" | path exists) {
        bd init --quiet
        print "✓ beads initialized"
    } else {
        print "✓ beads already initialized"
    }

    # Install git hooks
    print "\n🪝 Installing git hooks..."
    bd hooks install
    print "✓ Git hooks installed"

    # Configure sync branch
    print "\n⚙️  Configuring sync branch..."
    let config_path = ".beads/config.yaml"
    if ($config_path | path exists) {
        let config = (open $config_path | str replace '# sync-branch: "beads-sync"' 'sync-branch: "beads-sync"')
        $config | save -f $config_path
        print "✓ Sync branch configured"
    }

    # Run health check
    print "\n🏥 Running health check..."
    bd doctor | lines | each { |line|
        if ($line | str starts-with " ├") or ($line | str starts-with " └") {
            if ($line | str contains "⚠") {
                print $"  (ansi yellow)($line)(ansi reset)"
            } else if ($line | str contains "✓") or ($line | str contains "OK") {
                print $"  (ansi green)($line)(ansi reset)"
            } else {
                print $"  ($line)"
            }
        }
    }

    # Create AGENTS.md if it doesn't exist
    print "\n📝 Checking agent documentation..."
    if not ("AGENTS.md" | path exists) {
        print "⚠️  AGENTS.md not found - run 'bd onboard' for setup instructions"
    } else {
        print "✓ AGENTS.md exists"
    }

    # Verify setup
    print "\n✅ Beads setup complete!"
    print ""
    print $"(ansi cyan)Available Commands:(ansi reset)"
    print "  bd ready                    # Show unblocked issues"
    print "  bd create 'Task' -t task   # Create new issue"
    print "  bd update <id> --status in_progress  # Claim work"
    print "  bd status                   # Database overview"
    print "  bd doctor                   # Health check"
    print ""
    print $"(ansi cyan)Nushell Scripts:(ansi reset)"
    print "  nu scripts/beads-ready.nu   # Check ready work"
    print "  nu scripts/beads-create.nu  # Interactive issue creation"
    print "  nu scripts/beads-status.nu  # Comprehensive status"
    print ""
    print $"(ansi cyan)Documentation:(ansi reset)"
    print "  .claude/docs/beads-setup.md  # Comprehensive guide"
    print "  AGENTS.md                     # Workflow guidelines"
    print ""
    print "💡 Tip: Run 'bd ready --json' to see actionable work"
}
