#!/usr/bin/env nu
# Update dependencies and show changes

def main [] {
    print "📦 Checking for dependency updates..."

    # Backup current gleam.toml
    cp gleam.toml gleam.toml.bak

    print "\n🔄 Updating dependencies..."
    gleam update

    if $env.LAST_EXIT_CODE != 0 {
        print "❌ Update failed"
        mv gleam.toml.bak gleam.toml
        exit 1
    }

    print "\n✅ Dependencies updated!"
    print "\n📝 Changes:"
    diff gleam.toml.bak gleam.toml | lines

    # Clean up backup
    rm gleam.toml.bak

    print "\n🧪 Running tests with new dependencies..."
    gleam test

    if $env.LAST_EXIT_CODE != 0 {
        print "⚠️  Tests failed with updated dependencies"
        print "Consider reviewing the changes"
    } else {
        print "✅ All tests pass with new dependencies"
    }
}
