---
allowed-tools: Glob(**/AGENTS.md), Glob(AGENTS.md), Read(**/AGENTS.md), Read(AGENTS.md), Bash(git rev-parse:*)
description: Read all AGENTS.md files from current directory to project root
---

## Reading AGENTS.md

If any `AGENTS.md` files are found, read all of them from the current directory up to the project root:

- `./AGENTS.md` (in current directory)
- `./AGENTS.md` (in directories containing `package.json`)
- `./AGENTS.md` (in each subdirectory from current directory to project root)

Read all available `AGENTS.md` files to understand project-specific agent configurations and instructions.
**Also, say 'AGENTS.md loaded!' after reading them.**
