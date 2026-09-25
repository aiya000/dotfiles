---
name: pop-handoff
description: Pick up the newest handoff exactly as read-handoff does -- then remove it from the ~/.ai-memory repository and push, so it is not offered again on this machine or in a cloud session. Like git stash pop next to read-handoff's apply. Use when the user asks to pop the handoff, or to read the handoff and clear it.
allowed-tools: Skill(read-handoff), Skill(sync-ai-memory), Bash(ls *), Bash(rm-dust *), AskUserQuestion
---

# pop-handoff

`read-handoff` is `git stash apply`: it reads the handoff and leaves it. This is `git stash pop`:
the same reading, and then **the handoff is gone** -- from the repository, so neither the local
machine nor a cloud session finds it again.

Handoffs are perishable. Once a session has picked one up, the next session should find the handoff
*that* session writes, not this one again.

## Steps

1. **Run the `read-handoff` skill**, with this skill's argument if one was given, and do all of it:
   the issue overview, finding the handoff, following it to the memory file. Note which handoff
   file it picked up
2. **Remove that handoff, and only that one**:
    - In `~/.ai-memory/handoff/`: with the `sync-ai-memory` skill,
      `drop 'handoff: pop <file>' handoff/<file>.md handoff/<file>.declined`. It is removed and
      pushed, with its `decline-handoff-suggestion` marker if it had one; the repository's history
      keeps it, so it can be brought back
    - In an old local place (`~/tmp/claude-handoff/`, `${TMPDIR:-/tmp}/claude-handoff/`): not in
      the repository, so `rm-dust <path>` and its `.declined` marker, if any (`rm` in a cloud
      session, where there is no `rm-dust`)
3. Say in one line which file was removed, and whether the removal was pushed. Then carry on with
   what the handoff says to do first -- the pop is not the end of the job

## When not to remove it

- **`read-handoff` fell back to another project's handoff** (none for this one). AskUserQuestion
  first -- "Remove it" / "Leave it" -- since it was written for a different session
- **The handoff could not be followed** -- the memory file it names is missing, or the user stopped
  partway. Leave it and say why, so it can be popped once it has actually been picked up
- The memory file the handoff points at is **never** removed. It is the durable half, and other
  sessions still read it
