---
name: github-pr-create
description: Create a pull request with an auto-generated description. Use when the user asks to open a PR for the current branch's changes.
allowed-tools: Skill(git-commit), Skill(git-push), Bash(git status:*), Bash(git diff:*), Bash(git log:*), Bash(git branch:*), Bash(git checkout:*), Bash(git config:*), Bash(gh issue:*), Bash(gh pr:*), Bash(gh auth:*)
---

# github-pr-create

Create a pull request using `gh pr create` with an auto-generated description.

## Behavior

0. Check git/gh user consistency:
    - Run `git config user.name` to get the local git identity
    - Run `gh auth status` to identify the active GitHub user
    - If `gh auth status` shows `X Failed to log in ... (keyring)` for all accounts (even though the user has authenticated):
        - This is a known keyring issue in some environments — treat it as a **keyring-unavailable environment**
        - Do NOT stop; proceed to the **Keyring-unavailable fallback** section below
    - If they differ:
        - If acting autonomously: run `gh auth switch --user "$(git config user.name)"`
            - If switch succeeds: continue
            - If switch fails: defer this skill. After all other work is done, report to the user: the git/gh user mismatch and that the switch failed
        - If not acting autonomously: stop the skill and ask the user to resolve the mismatch before continuing

1. Check current branch status by running `git status` and `git branch`
2. Inspect the commits ahead of the base branch by running (whichever base exists):
    - `git log main..HEAD --oneline`
    - `git log develop..HEAD --oneline`
    - `git log master..HEAD --oneline`
3. If on `main` or `master` branch, create a new feature branch
4. If there are uncommitted staged changes, use the `git-commit` skill to commit them first
5. Push the branch to remote using the `git-push` skill
6. Create a PR using `gh pr create` with auto-generated title and description

## Branch Naming Convention

When creating a new branch, use the following prefixes based on the changes:

- `feature/` - New features or enhancements
- `fix/` - Bug fixes
- `hotfix/` - Urgent fixes for production
- `refactor/` - Code refactoring
- `chore/` - Maintenance tasks

Example: `feature/add-notification-timing`, `fix/resolve-login-error`

## PR Description Format

The PR description should be written in English and follow this structure:

```markdown
## Summary

Brief description of what this PR does (1-3 bullet points)

## Changes

- List of specific changes made
- Each change as a separate bullet point

## Breaking Changes

(Only include this section if there are breaking changes)

- Description of breaking changes and migration steps
```

## Secret Scan

Before creating the PR, check the diff (commits ahead of base branch) for:

- API keys and tokens: strings starting with `ghp_`, `gho_`, `AKIA`, `sk-`, `xox`, or matching `-----BEGIN.*PRIVATE KEY`
- Variables with sensitive names holding a value: patterns like `API_KEY=`, `_SECRET=`, `_TOKEN=`, `PASSWORD=`
- Hardcoded absolute home paths: `/Users/<name>/` or `/home/<name>/` (prefer `~`)
- Personal or organizational proper nouns that could identify specific individuals or organizations

**If any of the above are found**, do not create the PR. Use AskUserQuestion to present these options:

- "Create as-is" — proceed despite the finding
- "Fix then create" — cancel so the user can fix the issue first
- "Cancel" — abort without further action

Only proceed if the user selects "Create as-is", or if nothing suspicious was found.

## Does Not

1. Create a PR if the current branch has no commits ahead of the base branch
2. Create a PR if there are uncommitted changes (will prompt to commit first)
3. Force push or modify existing PRs without explicit user request

## Example Generated PR

```markdown
## Summary

- Add timing options to notify-cascade snippets for better notification scheduling

## Changes

- Add `now` option to `notify_cascade_schedule_today` for immediate notification
- Add morning reminder option (`09:30`) to `notify_cascade_schedule_someday`
```

## Keyring-unavailable fallback

When `gh auth status` shows keyring errors for all accounts (the environment cannot use the system keyring), `gh pr create` will also fail silently or with auth errors. Use this fallback instead:

1. Write the PR body to a file:
    - Use `~/tmp/` if it exists, otherwise use `/tmp/`
    - Filename: `pr-body-<branch-name>.md` (e.g. `pr-body-feature-web.md`)
    - Write the full PR body (the markdown content that would be passed to `--body`) to that file
2. Tell the user the PR could not be created automatically due to the keyring issue, and provide the exact command to run manually:

```
gh pr create --title "<title>" --body-file <path-to-file> --base <base-branch>
```

3. Also mention: if `gh` itself is not authenticated, they may need to run `gh auth login` first

## Notes

- Always push the branch before creating the PR
- Use the repository's default branch as the base (usually `main` or `master`)
- If uncertain about the PR title or description, ask the user before creating
- Do NOT add signatures or co-author tags to PR descriptions
