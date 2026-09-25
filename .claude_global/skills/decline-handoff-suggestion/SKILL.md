---
name: decline-handoff-suggestion
description: Decline the handoff that offer-handoff.sh offered at session start, so it is never offered again. Use when the user answers the "read the handoff?" question with no, or says 引継ぎはいい / 引継ぎ断る.
allowed-tools: Bash(ls *), Bash(git rev-parse *), Bash(touch *)
---

# decline-handoff-suggestion

The one-word answer to the question `offer-handoff.sh` asks at session start. It exists so the
user can say "no" with a completion instead of a sentence.

Declining leaves an empty `<name>.declined` next to `<name>.md`. The hook skips a handoff that has
one, so **that** handoff is not offered again -- a newer one still is. The marker does not end in
`.md`, so `read-handoff` can still read the file when asked.

## Finding the file

The same file the hook offered -- keep this in step with `.claude_global/hooks/offer-handoff.sh`:

1. The directory:
    1. `~/tmp/claude-handoff/` when `~/tmp` exists
    2. `${TMPDIR:-/tmp}/claude-handoff/`
2. The project:

   ```sh
   common=$(git rev-parse --path-format=absolute --git-common-dir 2>/dev/null)
   project=$( [ -n "$common" ] && basename "$(dirname "$common")" || basename "$PWD" )
   ```

3. Take the **last** of `<project>-*.md` sorted by name. `.dotfiles` handoffs start with a dot, so
   use `ls -a` or a glob, not a plain `ls`
4. If there is none for this project, say so and stop. **Do not fall back to another project's
   file** -- the hook never offers those, so there is nothing to decline

An argument overrides all of that: a path is taken directly, anything else is treated as a project
name.

## Marking it

1. `touch <dir>/<name>.declined` -- `<name>` is the handoff's file name without `.md`
2. **Do not read the handoff.** Declining is the whole job
3. Say in one line which handoff was declined. If it was already declined, say that instead; the
   `touch` is harmless either way
