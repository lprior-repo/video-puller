# Beads Setup Guide

This guide covers setting up Steve Yegge's beads system for this project.

## What is Beads?

Beads is a development tool created by Steve Yegge. Before proceeding with setup, you should research the latest information about beads.

## Research Checklist

Use the `/research` slash command to gather information about:

- [ ] Official beads repository and documentation
- [ ] Installation methods and requirements
- [ ] Configuration options
- [ ] Integration with Gleam projects
- [ ] Best practices and common patterns
- [ ] Community resources and examples

## Installation

### Using the Setup Script

```bash
nu scripts/setup-beads.nu
```

### Manual Installation

1. **Research the latest installation method:**
   - Use `/research beads installation` in Claude Code
   - Check Steve Yegge's official repositories
   - Look for package managers (npm, cargo, etc.)

2. **Install beads:**
   ```bash
   # Installation command will vary - research first!
   # Example (verify this is correct):
   # npm install -g beads
   # or
   # cargo install beads
   ```

3. **Verify installation:**
   ```bash
   beads --version
   beads help
   ```

## Project Configuration

### Initialize Beads

```bash
cd /home/lewis/src/video-puller
beads init
```

### Configure for Gleam

Create or edit `.beads/config`:

```toml
# Configuration will depend on beads capabilities
# Research the available options first

[project]
name = "video-puller"
language = "gleam"

[build]
# Add Gleam-specific build configuration

[watch]
# Configure file watching patterns
patterns = ["src/**/*.gleam", "test/**/*.gleam"]
```

## Integration

### Justfile Integration

Add beads commands to your `justfile`:

```just
# Run beads check
beads-check:
    beads check

# Run beads with watch mode
beads-watch:
    beads watch

# Clean beads cache
beads-clean:
    beads clean
```

### CI/CD Integration

Add beads checks to `.github/workflows/test.yml`:

```yaml
- name: Run beads checks
  run: beads check
```

## Usage

### Common Commands

```bash
# Check project status
beads status

# Run beads analysis
beads check

# Watch for changes
beads watch

# Clean beads data
beads clean
```

### With Gleam Workflow

```bash
# Standard development workflow
just dev           # Run Gleam checks
beads watch       # Monitor with beads

# Before committing
just ci           # Run Gleam CI
beads check      # Run beads checks
```

## Features to Leverage

Research and document these beads features:

- [ ] Code analysis capabilities
- [ ] Integration with build systems
- [ ] Watch mode and live reload
- [ ] Caching mechanisms
- [ ] Team collaboration features
- [ ] IDE integration

## Troubleshooting

### Common Issues

**Beads not found:**
- Ensure beads is installed and in PATH
- Run `which beads` to verify installation

**Configuration errors:**
- Check `.beads/config` syntax
- Verify Gleam-specific settings
- Review beads documentation for schema

**Performance issues:**
- Review watch patterns
- Check cache configuration
- Adjust resource limits

## Resources

After researching, add links to:

- [ ] Official beads documentation
- [ ] Steve Yegge's blog posts about beads
- [ ] GitHub repository
- [ ] Community forums/Discord
- [ ] Tutorial videos
- [ ] Example projects

## Next Steps

1. Run `/setup-beads` to initiate comprehensive setup
2. Research beads thoroughly using `/research beads`
3. Follow this guide to complete setup
4. Test beads with the Gleam project
5. Document your specific configuration
6. Share findings with the team

---

**Note:** This is a template guide. Use `/setup-beads` to research and fill in the specific details about beads, then update this document with accurate information.
