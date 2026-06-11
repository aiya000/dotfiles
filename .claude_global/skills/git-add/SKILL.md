---
name: git-add
description: Stage files for commit after scanning for secrets. Use when the user asks to stage files with `git add`.
allowed-tools: Bash(git status:*), Bash(git diff:*), Bash(git add:*)
---

# git-add

Stage files for commit after scanning for sensitive information.

## Behavior

1. Run `git diff` and `git status` to inspect the changes to be staged
2. Scan the changes for potential secrets (see **Secret Scan** below)
3. Stage files with `git add` if nothing suspicious was found (or user confirms)

## Secret Scan

Before staging, check the unstaged diff for:

- API keys and tokens: strings starting with `ghp_`, `gho_`, `AKIA`, `sk-`, `xox`, or matching `-----BEGIN.*PRIVATE KEY`
- Variables with sensitive names holding a value: patterns like `API_KEY=`, `_SECRET=`, `_TOKEN=`, `PASSWORD=`
- Hardcoded absolute home paths: `/Users/<name>/` or `/home/<name>/` (prefer `~`)
- Personal or organizational proper nouns that could identify specific individuals or organizations

**If any of the above are found**, do not stage. Use AskUserQuestion to present these options:

- "Stage as-is" — proceed despite the finding
- "Fix then stage" — cancel so the user can fix the issue first
- "Cancel" — abort without further action

Only proceed if the user selects "Stage as-is", or if nothing suspicious was found.

## Notes

- Stage only the files the user specified; do not stage unrelated files
- If uncertain which files to stage, ask the user before running `git add`
