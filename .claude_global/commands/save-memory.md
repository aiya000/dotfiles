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
    - Result for `date +%Y-%m-%d_%H:%M`: !`date +%Y-%m-%d_%H:%M`
3. Create the directory if it does not exist yet: `mkdir -p ~/.dotfiles/.private/CLAUDE-MEMORY/`
    - Result for `ls -d ~/.dotfiles/.private/CLAUDE-MEMORY/`: !`ls -d ~/.dotfiles/.private/CLAUDE-MEMORY/`
4. Write (or append) to: `~/.dotfiles/.private/CLAUDE-MEMORY/YYYY-MM-DD-{topic}.md`
    - If a file for the same or similar topic already exists: append or update it
    - Otherwise: create a new file with header `# Memory - YYYY-MM-DD`

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
- Filename: `YYYY-MM-DD-{topic}.md` (e.g., `2026-03-31-nvim-config.md`)
