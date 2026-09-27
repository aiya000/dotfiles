---
name: git-pull
description: Pull commits from remote after reviewing what is incoming and checking the working tree. Use when the user asks to pull, update, or sync the current branch with `git pull`.
allowed-tools: Skill(git-fetch), Bash(git status:*), Bash(git diff:*), Bash(git stash:*), Bash(git pull:*)
---

# git-pull

Pull commits from remote after reviewing what is incoming and making sure local work is safe.

## Behavior

1. Run the `git-fetch` skill to fetch and learn the state of the branch against its upstream
2. If the branch is **up to date** or only **ahead**, report that there is nothing to pull and stop
3. Run `git status` to check for uncommitted changes (see **Uncommitted Changes** below)
4. Decide how to integrate (see **Choosing How to Integrate** below)
5. Pull, then run `git status` and report what changed, including anything `git-fetch` pointed out
   as running code or changing how tools behave

## Choosing How to Integrate

- **Behind (fast-forward possible)** -- run `git pull --ff-only`
- **Diverged** -- do not pull yet. Use AskUserQuestion to present these options:
    - "Rebase" -- run `git pull --rebase`
    - "Merge" -- run `git pull --no-rebase`
    - "Cancel" -- abort without further action

If the project's AGENTS.md, or the user, says which one to use, follow that instead of asking.

## Uncommitted Changes

If `git status` shows uncommitted changes, check whether any of them are in files the incoming commits
touch (the files `git-fetch` reported).

- **No overlap** -- proceed; git carries the changes across
- **Overlap** -- do not pull. Use AskUserQuestion to present these options:
    - "Stash, pull, then pop" -- `git stash push`, pull, then `git stash pop`; report any conflict from the pop
    - "Commit first" -- cancel so the user (or the `git-commit` skill) can commit the changes first
    - "Cancel" -- abort without further action

## Conflicts

If the pull stops with conflicts, **do not resolve them on your own and do not abort the merge or
rebase**. List the conflicted files (`git status`) and ask the user how to proceed.

## Does Not

1. Run `git reset --hard`, `git clean`, `git checkout -- <file>`, or anything else that discards local changes
2. Run `git merge --abort` or `git rebase --abort` without explicit user request
3. Pull into a worktree this session was not launched in -- it changes the branch under whoever is working there
4. Pull a branch other than the current one's upstream without explicit user request

## Notes

- If the current branch has no upstream, or it is uncertain which remote or branch to pull from, ask the
  user before running `git pull`
- If `git pull` fails with `could not lock config file .git/config` inside the sandbox, rerun it with
  `dangerouslyDisableSandbox: true` (see the "Phantom Dotfiles in the Sandbox" section of the global config)
