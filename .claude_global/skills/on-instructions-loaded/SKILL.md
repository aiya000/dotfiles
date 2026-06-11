---
name: on-instructions-loaded
description: Announce which instructions file was just loaded. Use immediately after reading any CLAUDE.md or AGENTS.md file (including AGENTS.global.md and subdirectory AGENTS.md files).
---

# on-instructions-loaded

Say the appropriate loaded message based on the path of the instructions file that was just read.

## Behavior

Determine the message based on the file path:

| Condition | Message |
|---|---|
| Path is `~/.claude/CLAUDE.md` or resolves to it | `Global CLAUDE.md loaded!` |
| Path is `{project-root}/.claude/CLAUDE.md` or any other CLAUDE.md | `Local CLAUDE.md loaded!` |
| Path is `~/AGENTS.md` or `~/AGENTS.global.md` | `Global AGENTS.md loaded!` |
| Path is any other AGENTS.md (project subdirectory, etc.) | `Local AGENTS.md loaded!` |

## Classification Rules

- **Global vs Local**: If the file lives under the user's home directory config (`~/.claude/`) or is the home-directory-level AGENTS.md / AGENTS.global.md → Global. Otherwise → Local.
- **CLAUDE vs AGENTS**: Determined by the file name (`CLAUDE.md` → CLAUDE, `AGENTS.md` or `AGENTS.global.md` → AGENTS).

## Does Not

- Read or modify any files
- Do anything other than say the single loaded message
