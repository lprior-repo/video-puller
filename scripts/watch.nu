#!/usr/bin/env nu
# Watch files and run tests on changes

def main [] {
    print "👀 Watching for file changes..."
    print "Press Ctrl+C to stop"

    # Check if watchexec is installed
    if (which watchexec | is-empty) {
        print "❌ watchexec is not installed"
        print "Install it with: cargo install watchexec-cli"
        exit 1
    }

    watchexec -e gleam -- gleam test
}
