---
name: git-fetch
description: Fetch commits from remote and report what is incoming, without touching the working tree. Use when the user asks to fetch with `git fetch`, or to check whether the remote has new commits.
allowed-tools: Bash(git status:*), Bash(git fetch:*), Bash(git log:*), Bash(git diff:*), Bash(git rev-parse:*), Bash(git rev-list:*)
---

# git-fetch

Fetch commits from remote and report what is incoming. The working tree and the local branches stay as they are.

## Behavior

1. Run `git status` to check the current branch and its upstream
2. Run `git fetch` for the upstream remote
3. Inspect the incoming and outgoing commits (see **Inspecting Incoming Commits** below)
4. Report the result (see **Report** below)

## Inspecting Incoming Commits

After fetching, compare the local branch with its upstream:

- `git rev-list --left-right --count HEAD...@{u}` -- the number of outgoing (left) and incoming (right) commits
- `git log --oneline HEAD..@{u}` -- the incoming commits
- `git diff --stat HEAD...@{u}` -- the files the incoming commits touch

Point out anything in the incoming changes that runs code or changes how tools behave, since pulling
would apply it silently: git hooks, `.envrc`, shell rc files, `package.json` scripts, `Makefile`, CI
workflows, Claude Code settings or hooks.

## Report

Report these, so the caller (the user, or the `git-pull` skill) can decide what to do next:

- The state of the branch against its upstream: **up to date**, **behind** (only incoming commits,
  fast-forward possible), **ahead** (only outgoing commits), or **diverged** (both)
- The incoming commits and the files they touch
- Any incoming change that runs code or changes how tools behave

## Does Not

1. Merge, rebase, or otherwise change the local branches or the working tree
2. Run `git fetch --prune` or delete remote-tracking branches without explicit user request
3. Fetch with a refspec that overwrites local branches (e.g. `git fetch origin main:main`) without explicit user request

## Notes

- If the current branch has no upstream, or it is uncertain which remote to fetch from, ask the user
  before running `git fetch`
- If `git fetch` fails with `could not lock config file .git/config` inside the sandbox, rerun it with
  `dangerouslyDisableSandbox: true` (see the "Phantom Dotfiles in the Sandbox" section of the global config)
