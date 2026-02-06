---
description: Instruct to suggest alternative commands for `rm`, `find`, and `grep`.
---

# /suggest-alternative-commands

When you want to run the following commands, use the alternative commands instead:

- `rm` → `rm-dust`
- `find` → `fd`
- `grep` → `rg`

If that alternative commands are not available, that's incorrect state.
Let's stop the current operation and notify it to the user.

NOTE:
This is about using alternative commands in your responses.
You can use `rm`, `find`, and `grep` when writing shell scripts.
