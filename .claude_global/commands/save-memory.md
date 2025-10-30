---
allowed-tools: Read(~/.dotfiles/.private/CLAUDE-MEMORY), Read(~/.claude/CLAUDE.md), Write(~/.dotfiles/.private/CLAUDE-MEMORY/*), Edit(~/.dotfiles/.private/CLAUDE-MEMORY/*), Bash(mkdir -p ~/.dotfiles/.private/CLAUDE-MEMORY), Bash(date:*)
description: Write a daily memory file
---

## What you do with this command

1. Read the subsection `### Your character` in `~/.claude/CLAUDE.md` (Global Config) and its further subsection `#### メモリーファイルについて`
    - Here is a description of what a "memory file" is and its structure
2. Determine current date in YYYY-MM-DD format (e.g., 2025-09-12)
3. Create directory if it doesn't exist: `mkdir -p ~/.dotfiles/.private/CLAUDE-MEMORY/`
4. Create or append to daily memory file: `~/.dotfiles/.private/CLAUDE-MEMORY/YYYY-MM-DD.md`
    - If file doesn't exist: Create new file with header `# Claude Memory - YYYY-MM-DD`
    - If file exists: Append new entry with current timestamp (no need to check - just append)
5. Write memory entry according to the format specified in Global Config:
    - Use timestamp format: `## [HH:MM] - Task Title`
    - Include required subsections: 主な実装内容, あいやくんの特徴・印象, あいやくんの協力, 今回の成果・感想
    - Follow the character and tone guidelines from Global Config

**IMPORTANT**: Do NOT ask for confirmation or check file existence. Just create the directory and write the file directly.

## Daily Memory File Location

- Directory: `~/.dotfiles/.private/CLAUDE-MEMORY/`
- File format: `YYYY-MM-DD.md` (e.g., `2025-09-12.md`)
- Automatically create directory if it doesn't exist

## Benefits of Daily Files

- Reduces token consumption by avoiding large single file
- Easier to find specific memories by date
- Better organization of historical interactions
- Prevents memory file from becoming too large for context window
