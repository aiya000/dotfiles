---
name: read-handoff
description: Find and follow the newest handoff prompt written by create-handoff, without being given a path. Use at the start of a session when the user asks to pick up where the last one left off, or says 引き継ぎ / 引継ぎプロンプト読んで.
allowed-tools: Bash(ls *), Bash(git rev-parse *), Read(~/tmp/claude-handoff/*), Read(/tmp/claude-handoff/*)
---

# read-handoff

Picks up where the last session stopped. **The user should not have to supply a path.**

## Finding the file

1. The directory, in the same order `create-handoff` uses:
    1. `~/tmp/claude-handoff/` when `~/tmp` exists
    2. `${TMPDIR:-/tmp}/claude-handoff/`
2. Work out the project the same way it does:

   ```sh
   common=$(git rev-parse --path-format=absolute --git-common-dir 2>/dev/null)
   project=$( [ -n "$common" ] && basename "$(dirname "$common")" || basename "$PWD" )
   ```

3. Take the **last** of `<project>-*.md` sorted by name -- the names carry the timestamp
4. If there is none for this project, take the newest `*.md` in the directory and **say which
   project it was written for** before acting on it
5. If there is nothing at all, say so and list what the directory does hold. Do not guess

An argument overrides all of that: a path is read directly, anything else is treated as a project
name.

## Then

Read the whole file and **treat it as the user's instructions for this session**, not as
background reading. Do what it says first -- usually reading `AGENTS.md` and getting the branch
level -- before touching anything else.

Two things the file cannot know:

- **It was true when it was written.** Check the branch, the open Issues, and anything it calls
  未解決 against the world as it is now, and say so where they have moved on
- **It may simply be wrong.** Where the code disagrees with it, the code wins

Say in a line or two which file was picked up and what it says the first job is. Do not replay the
whole thing back at the user -- they wrote it.
