---
name: apply-handoff-to-agents-md
description: Fold the durable parts of a handoff note into the AGENTS.md files of the repository it was written about, each fact routed to the directory an agent will be standing in when they need it. Use when the user asks to reflect a handoff into AGENTS.md, or after read-handoff when the note turns out to hold repo knowledge rather than only next steps.
allowed-tools: Bash(git rev-parse:*), Bash(ls:*), Bash(cat:*), Bash(fd:*), Bash(rg:*), Read, Write, Edit, Grep, Glob, AskUserQuestion, Skill
---

# apply-handoff-to-agents-md

A handoff note is written to be **thrown away**: it tells the next session what to do next, and
next week that is wrong. But a handoff is also where the session's hard-won facts get written
down for the only time -- the trap that cost an hour, the reason a design went the way it did,
the command that has to be run a particular way.

This skill separates the two. **What is true about the repository moves into `AGENTS.md`, where
every future session reads it. What is true only about that moment stays in the handoff and
expires with it.**

## This is not `apply-to-agents-md-git-root`

That skill distils **the current conversation**, into **one** file at the repository root.

This one reads **a handoff file**, and spreads what it finds across
**`{repo-root}/[...children]/AGENTS.md`** -- root, `tests/e2e/`, `worker/`, wherever each fact
belongs. Use that one after doing the work; use this one after reading someone's note about it.

## Finding the handoff

If `/read-handoff` already ran this session, use the file it picked up -- do not go looking again.

Otherwise find it exactly the way `read-handoff` does (read that skill rather than reimplementing
the search). An argument to this skill overrides all of it: a path is read directly, anything else
is treated as a project name.

**Read the whole file before writing anything.** A fact near the bottom often explains why a
decision near the top was made, and only the pair is worth recording.

## Which repository

- The `AGENTS.md` files you will write live under `git rev-parse --show-toplevel` -- the
  **working tree**, which inside a git worktree is the worktree, not the main checkout. That is
  correct: `AGENTS.md` is a tracked file, so it is checked out here and commits from here
- Only the handoff's **file name** uses `--git-common-dir`. Do not use it to place files

**If the handoff was written about a different repository than the one you are in, stop and say
so.** Do not fold one project's facts into another's.

## What to keep

For each thing the handoff says, ask:

> **Would a session six months from now, working on something unrelated, still be wrong without
> knowing this?**

Keep it if yes. That is the whole test, and it is stricter than it sounds.

**Keep:**

- A trap that cost real time, together with **what it looked like when it bit** -- the error
  message, the test that hung, the silent no-op. The symptom is what makes it findable later
- A rule that is invisible in the code: "`requestSettingsOverride()` must not `await`, because the
  user gesture is what lets the OAuth window open"
- A decision **with its reason**. A decision recorded without its reason gets re-litigated, and
  the handoff is usually the only place the reason was ever written down
- A constraint from outside the code: a rate limit, a fixed port someone else registered, a
  branch that is the only one deploying somewhere
- Tooling that has to be invoked a particular way, and what happens when it is not

**Drop:**

- Everything about *what to do next*: the current SHA, what is in flight, open Issue numbers, who
  is waiting on whom. This is what the handoff is **for**, and it is exactly what goes stale
- The session's narrative -- what was tried, in what order, what turned out wrong
- Test counts, branch positions, "as of today" anything
- Anything about the user personally: their health, their hours, how the session felt. That is a
  kindness between sessions, not repository documentation
- Anything already in an `AGENTS.md`, `README.md`, or plain in the code. **Handoffs repeat
  themselves and repeat `AGENTS.md`** -- expect most of a note to be already known, and re-read
  every `AGENTS.md` on the path before deciding a fact is new

## Verify before you write

**A handoff was true when it was written. An `AGENTS.md` claims to be true now.** Promoting a fact
raises what it claims, so check it first:

- A file, function, or flag it names -- confirm it still exists
- A rule about the code -- read the code and confirm the code still does that
- A claim the handoff itself hedges ("たぶん", "説明しきれていない", "未確認") -- **do not promote a
  hedge into a rule.** Either verify it now and write what you verified, or leave it in the handoff
- Where the code disagrees with the note, **the code wins** -- and say so in your report, because
  it means the handoff was wrong and the user may care why

If verifying something is more work than this task warrants, leave it out rather than write it
down unchecked. A wrong `AGENTS.md` is worse than a short one: it is believed without checking.

## Where each fact goes

Route by **where an agent will be standing when they need it**, which is not the same as where the
code lives.

`read-agents-md` loads the files from the current directory up to the root, plus the immediate
directory of a file on first access. So a fact in `tests/e2e/AGENTS.md` is read only once someone
opens a file in `tests/e2e/`.

**That makes the routing question concrete:**

- Needed *before* going anywhere near that directory -- or when choosing whether to go at all --
  → **root**. "The Worker only deploys from `main`" has to be known before planning the work
- Needed *while editing files there*, and noise everywhere else → **that directory's
  `AGENTS.md`**. "Hide Next's dev overlay before clicking" is only ever true in `tests/e2e/`
- **When unsure, root.** A fact read too often costs a little attention; a fact never read costs
  the hour it was written to save

Create a child `AGENTS.md` only when there are at least a couple of facts that genuinely belong to
that directory alone. A one-line file is a file nobody remembers to look in.

**Child directories need no `CLAUDE.md` pointer** -- `read-agents-md` finds them. Only the
repository root gets one, and only when it does not already exist:

```markdown
Read `AGENTS.md` in this directory before doing anything else.
```

Never modify a `CLAUDE.md` that is already there.

## Writing

- **Merge, do not append.** Find the section the fact belongs to and put it there. A file that
  grows a new section per handoff becomes a log, which is the thing `AGENTS.md` must not be
- Write it as **how this repository works**, in the present tense. No dates, no SHAs, no "in the
  September session". If a SHA is the only way to point at something, the fact is too specific
- Follow the **language and formatting of the file you are writing into**. Many repositories
  require English in `AGENTS.md` while the handoff was written in Japanese -- translate. Check the
  repository's own rule before assuming
- Touch nothing you are not adding to

## Secret scan

Before writing, check the new content for:

- API keys and tokens: strings starting with `ghp_`, `gho_`, `AKIA`, `sk-`, `xox`, or matching
  `-----BEGIN.*PRIVATE KEY`
- Sensitive variable names holding a value: `API_KEY=`, `_SECRET=`, `_TOKEN=`, `PASSWORD=`
- Absolute home paths: `/Users/<name>/` or `/home/<name>/` -- prefer `~`
- Personal or organizational proper nouns identifying specific people

Handoffs are written for one reader and are looser about this than a tracked file can be.
**`AGENTS.md` is committed and pushed**, so a machine-local path or a name that was fine in
`~/tmp` is not fine here.

If anything is found, use `AskUserQuestion` to offer "Save as-is" / "Fix then save" / "Cancel",
and write only on "Save as-is".

## After

- **Do not delete or edit the handoff.** Its job -- what to do next -- is not this skill's job,
  and the next session still needs it
- Report, per file, what was added, and **name what you deliberately dropped and why** in a line
  or two. The user is the one who knows whether something you called ephemeral was actually the
  point
- Say which facts you could not verify and therefore left behind
- `AGENTS.md` is tracked. Committing it follows the repository's own rules -- do not invent a
  route to `git commit` here
