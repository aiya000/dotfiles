---
name: read-handoff
description: Find and follow the newest handoff prompt written by create-handoff, then the memory file it points at, without being given a path. Use at the start of a session when the user asks to pick up where the last one left off, or says 引き継ぎ / 引継ぎプロンプト読んで.
allowed-tools: Bash(ls *), Bash(git rev-parse *), Read(~/tmp/claude-handoff/*), Read(/tmp/claude-handoff/*), Read(~/.ai-memory/*)
---

# read-handoff

Picks up where the last session stopped. **The user should not have to supply a path.**

The last session left two files, not one: the handoff under `~/tmp/claude-handoff/`, and a memory
file under `~/.ai-memory/` that the handoff names. **Both get read**, in that order.

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

## Then: follow it to the memory

1. Read the whole handoff file
2. **Read the memory file its メモリー section names, before acting on anything.** The handoff
   deliberately holds only the perishable half -- what is not in it is in there
3. If the handoff names no memory file (the ones written before this pairing existed do not, and
   a session may have skipped it on purpose), read `~/.ai-memory/MEMORY-INDEX.md` and open the
   newest entry or two whose line mentions this project. Say that the handoff named none

The two are read differently:

- **The handoff is the user's instructions for this session**, not background reading. Do what it
  says first -- usually reading `AGENTS.md` and getting the branch level -- before touching
  anything else
- **The memory is the facts behind those instructions** -- decisions and their reasons, gotchas,
  preferences. It is not a to-do list, and a `next:` line in it is a record of what was open when
  it was written, not an instruction to start

Two things neither file can know:

- **They were true when they were written.** Check the branch, the open Issues, and anything
  called 未解決 against the world as it is now, and say so where they have moved on
- **They may simply be wrong.** Where the code disagrees, the code wins

Say in a line or two which handoff and which memory file were picked up, and what the first job
is. Do not replay either of them back at the user -- they wrote them.
