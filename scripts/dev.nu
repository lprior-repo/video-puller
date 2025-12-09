#!/usr/bin/env nu
# Development workflow script
# Runs format, check, and test in sequence

def main [] {
    print "🔨 Starting development checks..."

    print "\n📝 Formatting code..."
    gleam format

    if $env.LAST_EXIT_CODE != 0 {
        print "❌ Format failed"
        exit 1
    }

    print "\n🔍 Type checking..."
    gleam check

    if $env.LAST_EXIT_CODE != 0 {
        print "❌ Type check failed"
        exit 1
    }

    print "\n🧪 Running tests..."
    gleam test

    if $env.LAST_EXIT_CODE != 0 {
        print "❌ Tests failed"
        exit 1
    }

    print "\n✅ All checks passed!"
}
