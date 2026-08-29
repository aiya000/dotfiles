---
name: no-edit
description: Run the instructions that follow `/no-edit` in an investigation-only mode where writing/editing anywhere outside /tmp ($TMPDIR) is forbidden; any throwaway investigation script must be cleaned up with rm-dust when done. Use when the user's message starts with `/no-edit`.
allowed-tools: Write($TMPDIR/*), Write(/tmp/*), Edit($TMPDIR/*), Edit(/tmp/*)
---

# no-edit

Everything written after `/no-edit` (same line or following lines) is the actual task.
Perform it under the following investigation-only restriction: no writing or editing
anywhere except `/tmp` (or `$TMPDIR`).

## Behavior

1. **Editing/writing is forbidden outside `/tmp` (`$TMPDIR`)**
    - `Edit`/`Write` tool calls are restricted to `/tmp`/`$TMPDIR` paths for the duration
      of this skill (enforced via `allowed-tools` above)
    - Do not run any Bash command that creates, modifies, deletes, moves, or stages files
      outside `/tmp`/`$TMPDIR` — this also covers `git add`/`commit`/`push`, package
      installs, config edits, dotfile changes, etc.
    - Read-only tools and commands (Read, Grep, Glob, WebFetch, WebSearch, `git status`/
      `diff`/`log`, etc.) are unrestricted — investigate freely
2. **Investigation scripts go in `/tmp`/`$TMPDIR` only**
    - If the task needs a throwaway script (e.g. to parse logs, reproduce a bug, probe an
      API), write it under `/tmp`/`$TMPDIR`
3. **Clean up before reporting**
    - Before giving the final result, delete every file/script you created during the
      investigation with:
      ```
      ~/.dotfiles/bash-toys/bin/rm-dust <path>...
      ```
    - Never use plain `rm` for this — see the [[fd]] alternative-commands convention
    - Do this even if the task ended in an error, as long as files were created
4. Report the findings as usual

## When the task itself requires an edit outside /tmp

Do not silently perform it, and do not silently skip it either. Stop, tell the user that
`/no-edit` is blocking it, and ask how they'd like to proceed (e.g. drop `/no-edit` for
this request, or scope the write to `/tmp` instead).

## Does Not

- Does not persist beyond the current task — applies only to the instructions given
  directly after `/no-edit` in this invocation
- Does not restrict read-only investigation
- Does not modify sandbox or permission settings
