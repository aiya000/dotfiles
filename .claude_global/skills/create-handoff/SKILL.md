---
name: create-handoff
description: Save the session's durable half as a memory file (save-memory), then write the perishable half -- the handoff prompt for the next session -- to a well-known place in the ~/.ai-memory repository and push it, so the next session, local or in the cloud, can pick the work up with read-handoff or pop-handoff and nobody has to carry a path, then close the session down -- release what is holding memory, shut down what this session opened, and report it. Use when the user asks for a 引継ぎプロンプト or a handoff note, or when a long session is being wrapped up.
allowed-tools: Skill(save-memory), Skill(sync-ai-memory), Bash(date *), Bash(mkdir *), Bash(ls *), Bash(printf *), Bash(git rev-parse *), Bash(pgrep *), Bash(pkill *), TaskStop, Write(~/tmp/claude-handoff/*), Write(/tmp/claude-handoff/*), Write(~/.ai-memory/**), Read(~/.ai-memory/**)
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

**`~/.ai-memory/handoff/`**, in the same git repository as the memory files. It is shared by the
local machine and Claude Code cloud sessions, so a handoff written on one side is picked up on the
other -- but only once it is pushed (see **After writing**).

1. Get the repository ready with the `sync-ai-memory` skill (`prepare`). If `save-memory` just ran,
   it already did this; running it again only pulls
2. `mkdir -p ~/.ai-memory/handoff`

The path is **deterministic on purpose**. A `mktemp -d` path cannot be found again by the next
session, which is the whole thing this pair exists to avoid.

**Only if `~/.ai-memory` is not set up** (`prepare` says so and the user does not want to set it up
now), fall back to the old local places, first that works: `~/tmp/claude-handoff/` when `~/tmp`
exists, then `${TMPDIR:-/tmp}/claude-handoff/`. Say plainly that this handoff **stays on this
machine** -- and in a cloud session, that it is gone with the container. (Locally `~/tmp` is
read-only inside the Bash sandbox: the `mkdir` and the write need `dangerouslyDisableSandbox: true`,
so write with a quoted heredoc rather than the Write tool.)

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
- In a cloud session the working directory is the cloned repository, so `<project>` comes out the
  same as it does locally.

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
- Do not delete older handoffs -- `pop-handoff` removes the one it reads

## Then: close the session down

**Running this skill means the session is about to end.** Nothing else is coming, so anything this
session started and is still holding is now waste -- memory the machine could have back, a daemon
the next session's build will trip over, a process nobody will read the output of.

So once the handoff is written, close it all:

- **Release what is holding memory.** A build daemon, a language server, a container this session
  brought up for itself. On this machine that is `./gradlew --stop` above all -- an idle daemon
  keeps several GB, and a session that leaves one behind is why the *next* one dies in R8
- **Close the applications this session opened.** Emulators, simulators, dev servers, browsers
  driven by a test, a tunnel or a port forward left listening
- **Stop the background tasks this session armed.** A `Monitor` on a log whose writer has already
  exited keeps its watch until it times out; `TaskStop` ends it now
- **Verify it actually went, by pid** -- not by the stopping command's own output, which says what
  it asked for rather than what happened
- **Leave alone what the next session needs and what this session did not start.** A container that
  was already up when the session began, and serves the fixture every run uses, stays up. Say that
  it was left up, and why

**What may still run after this skill, and must not be broken by the closing:** secondary work
only -- `save-memory`, this skill again, a report to the user, a question they asked in the same
breath. Real work (implementation, creating an Issue, a build, a test run) is not expected to
follow; if the user asks for some after all, say plainly what was closed and bring back what that
work needs.

## After writing

**Publish the handoff** with the `sync-ai-memory` skill:
`publish 'handoff: <project> <YYYY-MM-DD-HHMM>' handoff/<file>.md`. Until it is pushed, the other
machine cannot see it, and a cloud session loses it with the container. Do this **before** closing
the session down, while the network and the clone are still there.

Report both paths -- the memory file and the handoff, as `~/.ai-memory/...` -- whether each was
pushed, and that the next session, local or in the cloud, only has to run `/read-handoff` (or
`/pop-handoff`, which also removes it once read). Either reads the handoff and follows it to the
memory on its own.

**Also report what was closed**, one line each: what was shut down, what was left running on
purpose. The user is about to leave the machine and should not have to guess what is still on it.
