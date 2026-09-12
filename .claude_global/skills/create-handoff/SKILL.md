---
name: create-handoff
description: Save the session's durable half as a memory file (save-memory), then write the perishable half -- the handoff prompt for the next session -- to a well-known file under ~/tmp, so the next session can pick the work up with read-handoff and nobody has to carry a path. Use when the user asks for a 引継ぎプロンプト or a handoff note, or when a long session is being wrapped up.
allowed-tools: Bash(date *), Bash(mkdir *), Bash(ls *), Bash(printf *), Bash(git rev-parse *), Write(~/tmp/claude-handoff/*), Write(/tmp/claude-handoff/*), Write(~/.ai-memory/*), Read(~/.ai-memory/*)
---

# create-handoff

Writes the handoff prompt for the next session, and puts it where `read-handoff` will find it on
its own. The point of the pair is that **no one has to carry a path between sessions**.

## First: save the memory, then write the handoff around it

**Run the `save-memory` skill before writing the handoff**, unless the user said not to
(`/create-handoff --no-memory`, 「メモリーは要らない」, and the like). The two files split the
session between them, and each fact is written once, in the file it belongs to:

- **The memory file (`~/.ai-memory/YYYY-MM-DD-{project}-{topic}.md`) carries the durable half** --
  decisions with their reasons, the gotchas that cost time, the user's preferences, what is still
  open. Dense English, read by any later session, still worth having in a month
- **The handoff file carries the perishable half** -- where the branch stands *right now*, what to
  do first, who is waiting on what, how the user is doing. Written for the very next session, and
  stale within days

**Never copy the memory's content into the handoff.** The handoff names the memory file and tells
the next session to read it; that is the whole of the overlap between them.

Note the path `save-memory` wrote to -- the handoff's first section needs it.

If `save-memory` was skipped, say so in that section, and then this handoff carries everything
itself, as it used to.

## Where the file goes

Work the directory out in this order, and create it if it is not there:

1. `~/tmp/claude-handoff/` -- whenever `~/tmp` exists
2. `${TMPDIR:-/tmp}/claude-handoff/` -- otherwise

Both are **deterministic on purpose**. A `mktemp -d` path cannot be found again by the next
session, which is the whole thing this pair exists to avoid. Only if neither can be created, fall
back to `mktemp -d` -- and then **say the path loudly** in your reply, because that is the one
case where the user does have to keep it.

⚠️ **`~/tmp` is read-only inside the Bash sandbox.** Both the `mkdir` and the write need
`dangerouslyDisableSandbox: true`, so write the file with a quoted-heredoc `cat > ... <<'EOF'`
rather than the Write tool, which has no such option. (Adding `~/tmp` to the sandbox's write
allowlist would remove the need -- see the `update-config` skill.)

File name: `<project>-<YYYY-MM-DD-HHMM>.md`

- `<project>` is the repository the work is in, **not** the current directory:

  ```sh
  common=$(git rev-parse --path-format=absolute --git-common-dir 2>/dev/null)
  project=$( [ -n "$common" ] && basename "$(dirname "$common")" || basename "$PWD" )
  ```

  `--git-common-dir` is what makes this right inside a **git worktree**: the directory there is
  called `develop`, and the project is not.
- `<YYYY-MM-DD-HHMM>` comes from `date +%Y-%m-%d-%H%M`. Never overwrite an earlier handoff. The
  names sort chronologically, and the last one is what `read-handoff` picks.

## What to write

**Address the next session directly.** It is not something to paste anywhere -- `read-handoff`
hands it over as instructions -- so drop any "paste this" preamble.

Write it in the language the session was held in.

The first section is the pointer at the memory; everything after it is the perishable half. Leave
out any that has nothing in it:

1. **メモリー** -- the memory file's path, and the instruction to read it before acting:
   「このセッションの主要な状態はメモリーファイルにある。まずこれを読むこと:
   `~/.ai-memory/<file>.md`。ここに書いていないことは、そちらにある」.
   Add a clause saying what is in it (which Issue, which subsystem), so the next session knows
   what it is about to load
2. **前提** -- working directory, branch, repository, and that `AGENTS.md` has to be read first
   (`read-agents-md`), because it overrides habits and defaults
3. **いちばん最初に** -- the exact commands to get level (`git fetch`, `--ff-only`), and where the
   branch stood when this was written: the SHA, and how far ahead of the release branch
4. **最優先の作業** -- what to do first, and what was already settled with the user (in their own
   words where they settled it) versus what is still open. The reasoning that outlives this week
   is in the memory; here, say what to do next and leave the reasons there
5. **未解決 / 返事待ち** -- anything blocked on the user or on the outside world, including the
   exact question that was put to them
6. **残っている Issue** -- number and one line each
7. **直近でやったこと** -- SHA or PR number and one line each, so the next session can read the
   real diff rather than trust a summary
8. **最後に** -- how the user is doing, and anything about their time and health

**コードの落とし穴** and **運用まわりの注意** no longer get a section of their own: they are what
the memory's `gotcha:` lines are for. What stays here is the part that is true only of this moment
-- a half-rebased branch, a test failing right now, a deploy in flight -- under 最優先の作業 or
未解決.

## Rules

- **Facts, with their sources.** A SHA, a PR number, a file path, a function name, so the next
  session can verify rather than believe. Say plainly when something is a guess
- **A result is worthless without the steps that produced it.** Never write "step 4 did not work"
  and leave the steps somewhere the next session cannot reach -- it reads as a fact and is actually
  unusable, and re-deriving the procedure costs more than writing it down did. If the session ran a
  procedure the user followed, the handoff carries the procedure, in full, next to its result.
  **This is the one durable-looking thing that stays in the handoff**: the memory's budget is forty
  telegraphic lines and will not hold a procedure. One that every future session will run belongs
  in `AGENTS.md` instead -- see the `apply-handoff-to-agents-md` skill
- **Record decisions together with the reason** -- in the memory, which is where reasons live now.
  A decision without its reason gets re-litigated
- **No secrets**, and no real names taken from screenshots or from the conversation
- Do not delete older handoffs

## After writing

Report both absolute paths -- the memory file and the handoff -- and say that the next session
only has to run `/read-handoff`, which reads the handoff and follows it to the memory on its own.
