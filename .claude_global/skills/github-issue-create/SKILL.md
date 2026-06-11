---
name: github-issue-create
description: Create a GitHub issue with an auto-generated description. Use when the user asks to open an issue for a bug, feature request, or task.
allowed-tools: Bash(git status:*), Bash(git log:*), Bash(git config:*), Bash(gh issue:*), Bash(gh repo:*), Bash(gh auth:*)
---

# github-issue-create

Create a GitHub issue using `gh issue create` with an auto-generated title and description.

## Behavior

0. Check git/gh user consistency:
    - Run `git config user.name` to get the local git identity
    - Run `gh auth status` to identify the active GitHub user
    - If they differ:
        - If acting autonomously: run `gh auth switch --user "$(git config user.name)"`
            - If switch succeeds: continue
            - If switch fails: defer this skill. After all other work is done, report to the user: the git/gh user mismatch and that the switch failed
        - If not acting autonomously: stop the skill and ask the user to resolve the mismatch before continuing

1. Review the context provided by the user (bug description, feature request, task details, etc.)
2. Inspect the repository to understand context if needed:
    - `git log --oneline -10` — recent commits for context
    - `gh issue list` — existing issues to avoid duplicates
3. Scan for sensitive information before creating (see **Secret Scan** below)
4. Generate an appropriate title and description based on the issue type
5. Create the issue using `gh issue create`

## Issue Types and Labels

Select appropriate labels based on the issue type:

- `bug` — Something isn't working correctly
- `enhancement` — New feature or improvement request
- `documentation` — Documentation improvements
- `question` — Further information is requested
- `chore` — Maintenance or internal tasks

## Issue Description Format

The issue description should be written in English and follow this structure:

### Bug Report

```markdown
## Description

Brief description of the bug.

## Steps to Reproduce

1. Step one
2. Step two
3. Step three

## Expected Behavior

What should happen.

## Actual Behavior

What actually happens.

## Environment

- OS / version information if relevant
```

### Feature Request

```markdown
## Summary

Brief description of the requested feature (1-3 bullet points).

## Motivation

Why this feature is needed or what problem it solves.

## Proposed Solution

Description of the proposed implementation or behavior.
```

### Task / Chore

```markdown
## Summary

Brief description of the task.

## Details

- Specific actions or changes needed
- Each item as a separate bullet point
```

## Secret Scan

Before creating the issue, check the content (title, description, any referenced code snippets) for:

- API keys and tokens: strings starting with `ghp_`, `gho_`, `AKIA`, `sk-`, `xox`, or matching `-----BEGIN.*PRIVATE KEY`
- Variables with sensitive names holding a value: patterns like `API_KEY=`, `_SECRET=`, `_TOKEN=`, `PASSWORD=`
- Hardcoded absolute home paths: `/Users/<name>/` or `/home/<name>/` (prefer `~`)
- Personal or organizational proper nouns that could identify specific individuals or organizations

**If any of the above are found**, do not create the issue. Use AskUserQuestion to present these options:

- "Create as-is" — proceed despite the finding
- "Fix then create" — cancel so the user can fix the issue first
- "Cancel" — abort without further action

Only proceed if the user selects "Create as-is", or if nothing suspicious was found.

## Does Not

1. Create an issue if the title or description is empty or unclear — ask the user for more details
2. Create duplicate issues — check existing issues first
3. Add assignees or milestones without explicit user request

## Notes

- If uncertain about the issue title or description, ask the user before creating
- Use the issue type and context to select appropriate labels
- Do NOT add signatures or co-author tags to issue descriptions
- **Neovim-related issues must use 'Neovim: ' prefix** in the title (e.g., `Neovim: Fix autocmd issue`)
