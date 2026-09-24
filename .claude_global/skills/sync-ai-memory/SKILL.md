---
name: sync-ai-memory
description: Make ~/.ai-memory -- the git repository that holds memory files and handoffs, shared by the local machine and Claude Code cloud sessions -- ready to read and write, and publish what was written. In a cloud session it clones the repository into the cache directory first. Used by save-memory, create-handoff, read-handoff and pop-handoff; use directly when the user asks to sync, pull, or push their memories or handoffs.
allowed-tools: Bash(bash *), Bash(ls *), mcp__Claude_Code_Remote__add_repo, ToolSearch
compatibility: Needs git, and the memory repository reachable from the session. In a cloud session, AI_MEMORY_REPO (owner/name) must be set in the environment settings and the Claude GitHub App installed on that repository.
---

# sync-ai-memory

`~/.ai-memory/` is a git repository. The local machine and every Claude Code cloud session read
and write the same one, so **each side pulls before it reads and pushes after it writes**. This
skill is that pull and that push; the skills that write memory call it rather than running git
themselves.

```
~/.ai-memory/
├── MEMORY-INDEX.md        # save-memory's index (merge=union, see below)
├── YYYY-MM-DD-*.md        # memory files (save-memory)
└── handoff/
    └── <project>-<YYYY-MM-DD-HHMM>.md   # handoffs (create-handoff / read-handoff / pop-handoff)
```

**Never write out the real target of `~/.ai-memory`** -- not in a reply, a commit message, or a
file. Refer to it as `~/.ai-memory/` and to the repository as "the memory repository". The script
never prints it either.

The script is `scripts/sync.sh` in this skill's directory.

## prepare -- before reading or writing

```sh
bash <this skill>/scripts/sync.sh prepare
```

- **Local**: `~/.ai-memory` is a symlink to a clone; this pulls it. If it is not a symlink, the
  script stops (exit 3): **ask the user to set it up, and do not create it**
- **Cloud session** (`CLAUDE_CODE_REMOTE=true`), when `~/.ai-memory` does not exist yet:
    1. `AI_MEMORY_REPO` must be set. If not, stop and tell the user to add it
       (`AI_MEMORY_REPO=<owner>/<name>`) to the cloud environment's environment variables
    2. Add the repository to the session with the `add_repo` tool (claude-code-remote MCP server;
       load it with `ToolSearch` if it is deferred), then call it with that owner and name and `access: "push"`. If it is refused,
       relay its reason as it is -- the usual one is that the Claude GitHub App is not installed on
       the memory repository -- and stop
    3. Run `prepare`, passing the clone URL `add_repo` returned, if it returned one:
       `sync.sh prepare <url>`. It clones into `${XDG_CACHE_HOME:-~/.cache}/ai-memory` and points
       `~/.ai-memory` at it. This is the one place a symlink at `~/.ai-memory` is created on the
       user's behalf: a fresh container has nothing to protect, and nothing else can work
- `prepare` is cheap on a second run (it only pulls). Run it once per skill that uses memory

The warning about `merge=union` means the one-time setup below has not been done yet. Pass it on to
the user once; it does not stop anything.

## publish -- after writing

```sh
bash <this skill>/scripts/sync.sh publish '<message>' <path>...
```

- Paths are relative to `~/.ai-memory`: `2026-09-24-dotfiles-foo.md MEMORY-INDEX.md`,
  `handoff/dotfiles-2026-09-24-2210.md`
- **Only the given paths are committed.** Anything else that happens to be dirty stays as it is
- A removed path is committed as a deletion (that is how `pop-handoff` deletes a handoff)
- It pulls with a rebase and retries the push, so the other side writing at the same time is
  fine. If the rebase itself fails, it aborts and stops: tell the user, do not force anything
- Message: `memory: <topic>`, `handoff: <project> <stamp>`, `handoff: pop <file>`. **No secrets
  and no real path** in it -- it is pushed

A push that fails leaves the commit in the clone. **In a cloud session that clone disappears with
the container**, so a failed publish there is not "saved": say so plainly, and show the file's
content so the user can keep it by other means.

## drop -- removing a file

```sh
bash <this skill>/scripts/sync.sh drop '<message>' <path>...
```

`git rm` on the paths, then the same commit and push as `publish`. It is how `pop-handoff` removes
the handoff it read -- the file stays in the repository's history, so a drop can be undone. Use
it rather than `rm`: locally `rm` is denied, and `rm-dust` would move the file out of the
repository without recording why.

## One-time setup (the user's, not this skill's)

Tell the user when it is missing; do not do it for them.

- `.gitattributes` in the memory repository with `MEMORY-INDEX.md merge=union`, so that two
  machines appending a line at the same time both keep their line instead of conflicting
- For cloud sessions: `AI_MEMORY_REPO=<owner>/<name>` in the environment's variables, and the
  Claude GitHub App installed on the memory repository
