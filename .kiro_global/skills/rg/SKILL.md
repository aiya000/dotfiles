---
name: rg
description: Alias entry point for the `suggest-alternative-commands` skill. Use when you want to run `grep`, or when the user mentions `rg` / ripgrep as the alternative to `grep`.
---

# rg

Alias skill for using `rg` instead of `grep`.

## Behavior

Activate the `suggest-alternative-commands` skill and follow its instructions.

- The actual replacement rule (`grep` → `rg`) lives there
- How to handle a missing alternative is also defined there

## Notes

This skill is only an entry point to `suggest-alternative-commands` and holds no rules of its own.
Edit `suggest-alternative-commands` to change the behavior.
