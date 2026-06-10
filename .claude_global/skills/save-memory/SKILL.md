---
name: save-memory
description: Export and distill the current conversation into a memory file. Use when the user asks to save, remember, or note the current session for future context.
allowed-tools: Bash(date *), Bash(ls *), Write(~/.ai-memory/*), Read(~/.ai-memory/*)
context: fork
---

# save-memory

## What you do with this skill

This skill distills the current conversation into a compact, machine-readable memory file
that future Claude Code sessions can load to restore context.

### Steps

1. Use the current conversation content as source material
2. Determine current date and time by running `date +%Y-%m-%d_%H:%M`
3. Verify the memory directory is set up, using the Bash tool:
    - Run: `ls -ld ~/.ai-memory` and confirm the output starts with `l` (i.e. `~/.ai-memory` is a symlink)
    - If it is **not** a symlink (or does not exist), **stop and ask the user to set it up** by running `ln -s ~/.dotfiles/.private/AI-MEMORY/ ~/.ai-memory` (see `~/.dotfiles/.private/README.md`). Do not create it yourself
    - Otherwise, list existing files with: `ls ~/.ai-memory 2>/dev/null`
4. Write (or append) to: `~/.ai-memory/YYYY-MM-DD-{project}-{topic}.md`
    - If a file for the same or similar project+topic already exists: append or update it
    - Otherwise: create a new file with header `# Memory - YYYY-MM-DD`
    - `{project}` is the short name of the current project (e.g. `dotfiles`, `my-app`); omit if the topic is not project-specific

### How to distill

From the conversation, extract only what is worth remembering across sessions.
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

- Directory: `~/.ai-memory` (a symlink to `~/.dotfiles/.private/AI-MEMORY/`)
- Filename: `YYYY-MM-DD-{project}-{topic}.md` (e.g., `2026-03-31-dotfiles-nvim-config.md`)
- `{project}` can be omitted for agent-wide or cross-project notes (e.g., `2026-03-31-workflow-tips.md`)
