# Project Instructions for Claude Code

**Note**: This project uses [bd (beads)](https://github.com/steveyegge/beads) for issue tracking. Use `bd` commands instead of markdown TODOs. See AGENTS.md for workflow details.

This is a Gleam project. Follow these guidelines:

## Code Style
- Use idiomatic Gleam patterns
- Prefer pattern matching over if/else chains
- Use `Result` and `Option` types for error handling
- Follow Gleam naming conventions (snake_case for functions and variables)
- Keep functions pure when possible

## Development Workflow
- Always run `gleam format` before committing
- Run `gleam check` to verify types
- Write tests for new functionality using gleeunit
- Use `just` for common tasks (see justfile for available commands)

## Dependencies
- Keep dependencies minimal and well-vetted
- Document why each dependency is needed
- Prefer packages from the official Gleam ecosystem

## Testing
- Write unit tests for all public functions
- Use descriptive test names
- Test both happy path and error cases
- Aim for high test coverage

## Architecture
- Keep modules focused and cohesive
- Use clear, descriptive names for functions and types
- Document complex logic with comments
- Export only what's necessary from each module

## Before Committing
- Run `just ci` to ensure all checks pass
- Update documentation if needed
- Review changes for any security concerns
