---
allowed-tools: Skill(git-commit), Bash(git status:*), Bash(git diff:*), Bash(git log:*), Bash(git branch:*), Bash(git checkout:*), Bash(git push:*), Bash(gh:*)
description: Create a pull request with auto-generated description
---

# /create-pr

Create a pull request using `gh pr create` with an auto-generated description.

## Behavior

1. Check current branch status with `git status` and `git branch`
2. If on `main` or `master` branch, create a new feature branch
3. If there are uncommitted staged changes, use `/git-commit` to commit them first
4. Push the branch to remote with `git push -u origin <branch>`
5. Create a PR using `gh pr create` with auto-generated title and description

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

## Notes

- Always push the branch before creating the PR
- Use the repository's default branch as the base (usually `main` or `master`)
- If uncertain about the PR title or description, ask the user before creating
- Do NOT add signatures or co-author tags to PR descriptions
