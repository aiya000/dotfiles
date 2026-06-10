---
name: read-agents-md
description: Read all AGENTS.md files from current directory to project root. Use when the user asks to load AGENTS.md instructions or refresh agent guidelines, or the first time you access a file in a directory whose files you have not read or written yet, to load that directory's AGENTS.md.
allowed-tools: Glob(**/AGENTS.md), Glob(AGENTS.md), Read(**/AGENTS.md), Read(AGENTS.md), Read(~/.dotfiles/AGENTS.global.md), Bash(git rev-parse:*)
---

# read-agents-md

## Reading AGENTS.md

Read the following files in order (skip any that do not exist):

1. `~/.dotfiles/AGENTS.global.md` — global agent instructions shared across all projects
2. All `AGENTS.md` files along the path from the current directory up to the project root (inclusive on both ends)

To find the files for step 2:

- Determine the project root with `git rev-parse --show-toplevel` (or treat the current directory as root if not in a git repo)
- Walk from the current directory up to the project root, collecting each directory that contains an `AGENTS.md`
- Read them from outermost (project root) to innermost (current directory)

**After reading all found files, say 'AGENTS.md loaded!'**

## On first access to a file in a directory

The first time you access (read or write) any file in a directory whose files you have not touched yet in this session, read that directory's `AGENTS.md` if it exists and has not been loaded yet.

Rules:

- "First access" means the first time in the session you read or write any file inside that directory
- Only the `AGENTS.md` directly in that immediate directory is loaded — do not search parent directories here
- If the directory has no `AGENTS.md`, nothing is loaded
- Each directory's `AGENTS.md` is loaded at most once per session
- When you load a new directory's `AGENTS.md` this way, also re-read every `AGENTS.md` you have already loaded in this session (including the current directory's), so all of them stay fresh in context
