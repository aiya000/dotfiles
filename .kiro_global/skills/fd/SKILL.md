---
name: fd
description: Alias entry point for the `suggest-alternative-commands` skill. Use when you want to run `find`, or when the user mentions `fd` / `fd-find` as the alternative to `find`.
---

# fd

Alias skill for using `fd` instead of `find`.

## Behavior

Activate the `suggest-alternative-commands` skill and follow its instructions.

- The actual replacement rule (`find` → `fd`, or `fdfind` when `fd` is not found) lives there
- How to handle a missing alternative is also defined there

## Notes

This skill is only an entry point to `suggest-alternative-commands` and holds no rules of its own.
Edit `suggest-alternative-commands` to change the behavior.
