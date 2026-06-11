---
name: verify-git-identity
description: Verify that git user.name and user.email are configured. Use automatically before any git operation that requires identity (commits, rebases, cherry-picks, etc.), and when the user asks to check or verify their git identity.
allowed-tools: Bash(git config:*)
---

# verify-git-identity

Check that `git config user.name` and `git config user.email` are both set before proceeding with any identity-dependent git operation.

## Behavior

1. Run `git config user.name` and `git config user.email`
2. If **both** are set: report them to the user and proceed (the calling skill or task continues)
3. If **either is missing**: stop and use AskUserQuestion to present the following options:
    - "Configure now" — show the user the commands to run: `git config --global user.name "Your Name"` and `git config --global user.email "you@example.com"`
    - "Cancel" — abort the operation without further action

## Does Not

- Modify git config itself — only reads and reports
- Proceed with any git identity-dependent operation when identity is missing
