Research and register this project with the MCP agent-mail server. You should:

1. **Research MCP agent-mail server:**
   - Review the available MCP agent-mail tools and resources
   - Use the ListMcpResourcesTool to see what's available
   - Read MCP agent-mail documentation thoroughly
   - Understand the project registration workflow
   - Learn about agent coordination features

2. **Set up the project identity:**
   - Use `ensure_project` to create/verify project in agent-mail
   - Use the absolute path: `/home/lewis/src/video-puller`
   - Document the project key that's created

3. **Register the agent identity:**
   - Use `register_agent` to create your agent identity
   - Let the system auto-generate a valid agent name (adjective+noun)
   - Set program: "claude-code"
   - Set model: "sonnet-4.5"
   - Set task_description appropriately

4. **Configure coordination features:**
   - Set up file reservation patterns for Gleam files
   - Configure contact policy (suggest "auto" for development)
   - Set up build slot configuration if needed
   - Document agent coordination workflows

5. **Create helper scripts:**
   - Create a nushell script to initialize MCP sessions
   - Add justfile commands for common MCP operations
   - Create slash commands for frequent MCP tasks

6. **Document the setup:**
   - Create `.claude/docs/mcp-agent-setup.md`
   - Document the project key and agent identity
   - Provide examples of common coordination patterns
   - Document how to use file reservations
   - Document messaging between agents

7. **Test the setup:**
   - Verify you can send messages
   - Test file reservations
   - Ensure all MCP tools are accessible
   - Run through a sample coordination workflow

Be comprehensive - this is critical infrastructure for agent coordination. Use all available MCP resources and tools to ensure proper setup.
