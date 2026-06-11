---
name: git-push
description: Push commits to remote after scanning for secrets. Use when the user asks to push commits with `git push`.
allowed-tools: Bash(git status:*), Bash(git log:*), Bash(git diff:*), Bash(git push:*)
---

# git-push

Push commits to remote after scanning for sensitive information.

## Behavior

1. Run `git log` and `git diff` to inspect the commits to be pushed
2. Scan the unpushed commits for potential secrets (see **Secret Scan** below)
3. Push with `git push` if nothing suspicious was found (or user confirms)

## Secret Scan

Before pushing, check the unpushed commits (diff against remote) for:

- API keys and tokens: strings starting with `ghp_`, `gho_`, `AKIA`, `sk-`, `xox`, or matching `-----BEGIN.*PRIVATE KEY`
- Variables with sensitive names holding a value: patterns like `API_KEY=`, `_SECRET=`, `_TOKEN=`, `PASSWORD=`
- Hardcoded absolute home paths: `/Users/<name>/` or `/home/<name>/` (prefer `~`)
- Personal or organizational proper nouns that could identify specific individuals or organizations

**If any of the above are found**, do not push. Use AskUserQuestion to present these options:

- "Push as-is" — proceed despite the finding
- "Fix then push" — cancel so the user can fix the issue first
- "Cancel" — abort without further action

Only proceed if the user selects "Push as-is", or if nothing suspicious was found.

## Does Not

1. Force push (`--force`) without explicit user request
2. Push to `main` or `master` without explicit user request

## Notes

- If uncertain which remote or branch to push to, ask the user before running `git push`
