#!/usr/bin/env nu
# Generate and serve documentation locally

def main [] {
    print "📚 Generating documentation..."

    gleam docs build

    if $env.LAST_EXIT_CODE != 0 {
        print "❌ Documentation generation failed"
        exit 1
    }

    print "✅ Documentation generated"

    # Check if python is available for simple HTTP server
    if not (which python3 | is-empty) {
        print "\n🌐 Serving documentation at http://localhost:8000"
        print "Press Ctrl+C to stop"
        cd build/dev/docs
        python3 -m http.server 8000
    } else if not (which python | is-empty) {
        print "\n🌐 Serving documentation at http://localhost:8000"
        print "Press Ctrl+C to stop"
        cd build/dev/docs
        python -m http.server 8000
    } else {
        print "📁 Documentation is available in: build/dev/docs/"
        print "Open build/dev/docs/index.html in your browser"
    }
}
