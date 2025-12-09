#!/usr/bin/env nu
# Setup script for new developers

def main [] {
    print "🚀 Setting up video-puller development environment..."

    # Check if gleam is installed
    if (which gleam | is-empty) {
        print "❌ Gleam is not installed"
        print "Install it from: https://gleam.run/getting-started/installing/"
        exit 1
    }

    print "✓ Gleam is installed:" (gleam --version)

    # Download dependencies
    print "\n📦 Downloading dependencies..."
    gleam deps download

    if $env.LAST_EXIT_CODE != 0 {
        print "❌ Failed to download dependencies"
        exit 1
    }

    # Build the project
    print "\n🔨 Building project..."
    gleam build

    if $env.LAST_EXIT_CODE != 0 {
        print "❌ Build failed"
        exit 1
    }

    # Run tests
    print "\n🧪 Running tests..."
    gleam test

    if $env.LAST_EXIT_CODE != 0 {
        print "❌ Tests failed"
        exit 1
    }

    print "\n✅ Setup complete! You're ready to develop."
    print "\nUseful commands:"
    print "  just --list       - Show all available tasks"
    print "  gleam run         - Run the application"
    print "  gleam test        - Run tests"
    print "  nu scripts/dev.nu - Run full development checks"
}
