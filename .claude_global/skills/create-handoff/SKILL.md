---
name: create-handoff
description: Write the handoff prompt for the next session to a well-known file under ~/tmp, so the next session can pick the work up with read-handoff and nobody has to carry a path. Use when the user asks for a 引継ぎプロンプト or a handoff note, or when a long session is being wrapped up.
allowed-tools: Bash(date *), Bash(mkdir *), Bash(ls *), Bash(git rev-parse *), Write(~/tmp/claude-handoff/*), Write(/tmp/claude-handoff/*)
---

# create-handoff

Writes the handoff prompt for the next session, and puts it where `read-handoff` will find it on
its own. The point of the pair is that **no one has to carry a path between sessions**.

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

Cover these, leaving out any that have nothing in them:

1. **前提** -- working directory, branch, repository, and that `AGENTS.md` has to be read first
   (`read-agents-md`), because it overrides habits and defaults
2. **いちばん最初に** -- the exact commands to get level (`git fetch`, `--ff-only`), and where the
   branch stood when this was written: the SHA, and how far ahead of the release branch
3. **最優先の作業** -- what to do first **and why**: the reasoning, what was already settled with
   the user (in their own words where they settled it), and what is still open
4. **未解決 / 返事待ち** -- anything blocked on the user or on the outside world, including the
   exact question that was put to them
5. **残っている Issue** -- number and one line each
6. **直近でやったこと** -- SHA or PR number and one line each, so the next session can read the
   real diff rather than trust a summary
7. **コードの落とし穴** -- what cost time this session and would cost it again
8. **運用まわりの注意** -- the tooling traps: which commands need what, which tests are flaky
9. **最後に** -- how the user is doing, and anything about their time and health

## Rules

- **Facts, with their sources.** A SHA, a PR number, a file path, a function name, so the next
  session can verify rather than believe. Say plainly when something is a guess
- **A result is worthless without the steps that produced it.** Never write "step 4 did not work"
  and leave the steps somewhere the next session cannot reach -- it reads as a fact and is actually
  unusable, and re-deriving the procedure costs more than writing it down did. If the session ran a
  procedure the user followed, the handoff carries the procedure, in full, next to its result
- **Record decisions together with the reason.** A decision without its reason gets re-litigated
- **No secrets**, and no real names taken from screenshots or from the conversation
- Do not delete older handoffs

## After writing

Report the absolute path, and say that the next session only has to run `/read-handoff`.
