---
name: rm-dust
description: Alias entry point for the `suggest-alternative-commands` skill. Use when you want to run `rm` or delete files, or when the user mentions `rm-dust` as the alternative to `rm`.
---

# rm-dust

Alias skill for using `rm-dust` instead of `rm`.

## Behavior

Activate the `suggest-alternative-commands` skill and follow its instructions.

- The actual replacement rule (`rm` → `rm-dust`) lives there
- How to handle a missing alternative is also defined there

## Notes

This skill is only an entry point to `suggest-alternative-commands` and holds no rules of its own.
Edit `suggest-alternative-commands` to change the behavior.
