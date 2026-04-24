---
allowed-tools: Glob(**/AGENTS.md), Glob(AGENTS.md), Read(**/AGENTS.md), Read(AGENTS.md), Read(~/.dotfiles/AGENTS.global.md), Bash(git rev-parse:*)
description: Read all AGENTS.md files from current directory to project root
---

## Reading AGENTS.md

Read the following files in order (skip any that do not exist):

1. `~/.dotfiles/AGENTS.global.md` — global agent instructions shared across all projects
2. All `AGENTS.md` files along the path from the current directory up to the project root (inclusive on both ends)

To find the files for step 2:
- Determine the project root with `git rev-parse --show-toplevel` (or treat the current directory as root if not in a git repo)
- Walk from the current directory up to the project root, collecting each directory that contains an `AGENTS.md`
- Read them from outermost (project root) to innermost (current directory)

**After reading all found files, say 'AGENTS.md loaded!'**