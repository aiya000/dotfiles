---
allowed-tools: Bash(date:*), Write(*), Read(*)
description: Export and distill the current conversation into a memory file
---

## What you do with this command

This command is a thin wrapper around `/export`.
The goal is to distill the exported conversation into a compact, machine-readable memory file
that future Claude Code sessions can load to restore context.

### Steps

1. Use the exported conversation content provided via `$EXPORT` as source material
2. Determine current date and time:

!`date +%Y-%m-%d_%H:%M`

3. Create directory if needed: `mkdir -p ~/.dotfiles/.private/CLAUDE-MEMORY/`
4. Write (or append) to: `~/.dotfiles/.private/CLAUDE-MEMORY/YYYY-MM-DD.md`
    - If file doesn't exist: create with header `# Memory - YYYY-MM-DD`
    - If file exists: append a new entry

### How to distill the export

From the exported conversation, extract only what is worth remembering across sessions.
Write under a timestamped heading: `## [HH:MM] - <short topic title>`

Keep:
- What was decided or implemented (the outcome, not the process)
- User preferences or constraints discovered during the session
- Key facts about the project or codebase that are not obvious from reading the code
- Errors or pitfalls encountered and how they were resolved

Remove:
- Tool call inputs/outputs and command logs
- Verbose code listings (keep only the essential snippet if truly needed)
- Back-and-forth clarification exchanges
- Any content already derivable from the current code or git history
- Meta-conversation about the AI itself

The result should be concise — a future session should be able to read it in seconds and understand
what happened without replaying the full conversation.

## Memory File Location

- Directory: `~/.dotfiles/.private/CLAUDE-MEMORY/`
- Filename: `YYYY-MM-DD.md` (e.g., `2026-03-31.md`)
