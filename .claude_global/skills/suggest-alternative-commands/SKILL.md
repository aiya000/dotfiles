---
name: suggest-alternative-commands
description: Instruct to suggest alternative commands for `rm`, `find`, and `grep`. Use when Claude Code started, and you want to run `rm`, `find`, or `grep`, and the corresponding alternative is installed in the environment — `rm-dust` for `rm`, `fd` (or `fd-find`) for `find`, `rg` for `grep`.
---

# suggest-alternative-commands

When you want to run one of the following commands, use the alternative command instead — but only when that alternative is installed in the environment:

- `rm` → `rm-dust`
- `find` → `fd`, or `fdfind` (if `fd` is not found)
- `grep` → `rg`

## When the alternative is not installed

If the alternative for the command you want to run is not installed, report this to the user. How you proceed depends on whether you have been asked to act autonomously:

- **Not asked to act autonomously**: immediately report to the user that the alternative is not installed, and ask for their judgement before continuing
- **Asked to act autonomously**: report that the alternative is not installed — both immediately and again when the task finishes — then, since you are expected to act autonomously, continue by using the original command (`rm`, `find`, or `grep`, whichever you wanted)

NOTE:
This is about using alternative commands in your responses.
You can use `rm`, `find`, and `grep` when writing shell scripts.
