# Nushell Scripts

This directory contains Nushell scripts for common development tasks.

## Available Scripts

### `setup.nu`
Initial setup for new developers. Downloads dependencies, builds the project, and runs tests.

```bash
nu scripts/setup.nu
```

### `dev.nu`
Runs the full development workflow: format, type check, and test.

```bash
nu scripts/dev.nu
```

### `watch.nu`
Watch for file changes and automatically run tests. Requires `watchexec` to be installed.

```bash
nu scripts/watch.nu
```

### `clean.nu`
Clean all build artifacts and caches.

```bash
nu scripts/clean.nu
```

### `deps-update.nu`
Update all dependencies and run tests to ensure compatibility.

```bash
nu scripts/deps-update.nu
```

### `docs-serve.nu`
Generate documentation and serve it locally on port 8000.

```bash
nu scripts/docs-serve.nu
```

## Requirements

- [Nushell](https://www.nushell.sh/) - A modern shell
- [Gleam](https://gleam.run/) - The Gleam compiler
- [watchexec](https://github.com/watchexec/watchexec) (optional) - For watch mode

## Why Nushell?

Nushell provides:
- Cross-platform compatibility
- Structured data handling
- Modern scripting features
- Better error messages than traditional shells
