#!/usr/bin/env nu
# Clean build artifacts and caches

def main [] {
    print "🧹 Cleaning build artifacts..."

    if ("build" | path exists) {
        rm -rf build
        print "✓ Removed build directory"
    }

    if ("_build" | path exists) {
        rm -rf _build
        print "✓ Removed _build directory"
    }

    print "✅ Clean complete!"
}
