#!/usr/bin/env nu
# Setup Steve Yegge's beads for this project

def main [] {
    print "🔮 Setting up beads for video-puller..."
    print ""

    # Check if beads is installed
    print "📦 Checking for beads installation..."
    if (which beads | is-empty) {
        print "⚠️  beads is not installed"
        print ""
        print "Would you like to install beads now? (y/n)"
        let response = (input)

        if $response == "y" {
            print "📥 Installing beads..."
            # Add installation commands here once we research the proper method
            print "⚠️  Please refer to .claude/docs/beads-setup.md for installation instructions"
            exit 1
        } else {
            print "❌ beads is required for this setup"
            exit 1
        }
    }

    print "✓ beads is installed:" (beads --version)

    # Initialize beads in the project
    print "\n🔧 Initializing beads..."
    if not (".beads" | path exists) {
        beads init
        print "✓ beads initialized"
    } else {
        print "✓ beads already initialized"
    }

    # Configure beads for Gleam projects
    print "\n⚙️  Configuring beads for Gleam..."
    # Add beads configuration here once we research the options

    # Verify setup
    print "\n✅ Beads setup complete!"
    print ""
    print "Next steps:"
    print "  1. Review .beads configuration"
    print "  2. Run 'beads status' to check setup"
    print "  3. See .claude/docs/beads-setup.md for usage"
}
