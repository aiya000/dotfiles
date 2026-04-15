---
allowed-tools: Bash(git status:*), Bash(git diff:*), Bash(git log:*), Bash(git show:*), Bash(git commit:*)
description: Create a new git commit for staged changes
---

**Important:** **This command only commits staged changes. It does not stage any new files**.

# /git-commit

Automatically generates a Conventional Commits compliant commit message and commits staged changes.

## Current State

Staged changes:

!`git diff --staged`

Git status:

!`git status`

Recent commits:

!`git log --oneline -20`

## Behavior

1. Analyze the staged changes and recent commits shown above
2. Generate a commit message following:
   - Conventional Commits specification (structure and basic rules)
   - User's commit style patterns learned from repository history
3. Execute `git commit` with the generated message

## Conventional Commits Specification Summary

Commit message structure:

```
<type>[optional scope]: <description>

[optional body]

[optional footer(s)]
```

Key rules:

- **type**: Required noun (feat, fix, chore, refactor, etc.) followed by optional scope and colon
- **scope**: Optional, describes section of codebase in parenthesis (e.g., `fix(parser):`)
- **description**: Required short summary immediately after colon and space
- **body**: Optional, provides additional context, begins one blank line after description
- **footers**: Optional, one blank line after body, uses git trailer format (e.g., `Reviewed-by: Z`)
- **BREAKING CHANGE**: Use exclamation mark after type/scope (e.g., `feat!:`) or footer `BREAKING CHANGE:` for breaking changes

## Does Not

1. Don't add some new diff by `git-add`
1. Ask the user, "Is it okay not to stage this file?"
    - Do git-commit it as-is that is already been staged

## Commit Message Rules

Based on analysis of this repository's commit history, follow these patterns:

### Scope Format

Scope format uses `>` to indicate hierarchical relationships and specificity level.

#### Decision Criteria for Using `>`

**Single-level scope (`type(Component):`)**

- Use when changes affect multiple files or modules within a component
- Use when the change impacts the component as a whole
- Examples:
    - `feat(Neovim): Add Docker-based Claude Code support` - affects multiple files
    - `feat(Claude Code): Improve commit message generation` - component-wide improvement
    - `refactor(Neovim): Improve terminal and helper integration` - multiple modules affected

**Hierarchical scope (`type(Component > Specific):`)**

- Use when changes target a specific subdirectory, file, or module
- Use to avoid redundancy in the description (scope already indicates location)
- Examples:
    - `feat(Claude Code > commands): Add new command file` - specific to commands directory
    - `feat(Neovim > helper): Add utility function` - specific to helper.lua module
    - `fix(Neovim > autocmds): Resolve autocmd issue` - specific to autocmds configuration

#### When Uncertain

If uncertain whether to use hierarchical scope, consider:

1. Is the change localized to a specific subdirectory or module? → Use `>`
2. Does using `>` make the description cleaner by avoiding repetition? → Use `>`
3. Does the change span multiple areas within the component? → Don't use `>`

The goal is clarity and avoiding redundancy in commit messages.

### Description Style

- Use backticks for code elements: `` `function_name()` ``
- Use `&` to separate **different concerns** (topics); use natural English (`and` / `, and`) when listing multiple objects under the **same concern**
    - `Remove x and y & Update z` — two concerns: removal and update
    - `Delete x, y, and z & Create v` — two concerns: deletion and creation
    - `Do a and b` — one concern with two objects
    - `Do a, b, and c` — one concern with three objects
- Be specific and descriptive
- Start with imperative verb (Add, Fix, Remove, Update, etc.)

### Common Types

Based on repository history, frequently used types with strict usage criteria:

- `feat`: New feature or enhancement (use when functionality addition/enhancement is the main change)
- `change`: Changing behavior (use when behavior modification is the main change)
- `fix`: Bug fix
- `refactor`: Code refactoring (ONLY when functionality remains exactly the same, internal improvements only)
- `remove`: Removing files or features (use when deleting functions, files, or features that may have been in use)
- `chore`: Maintenance tasks (stylua, update submodules, etc.)
- `update`: Updating dependencies or submodules
- `move`: Moving files

**Critical distinction rules**:

- If there's even a possibility of functionality change, do NOT use `refactor`
- Deletion of functions/features → use `remove`, not `refactor`
- Main change is adding/enhancing functionality → use `feat`
- Main change is modifying behavior → use `change`
- Error prevention (adding checks/guards to avoid errors) → use `fix`
    - Example: Adding directory existence check before executing command that would fail without it

### Special Patterns

- Keep descriptions concise but informative
    - Write details to the body, not the head line, if details are needed
- Use `wip` for work-in-progress commits (rare, avoid if possible)

## Example Generated Messages

Single-level scope (cross-file changes):

```
feat(Neovim): Add Docker-based Claude Code environment support

Implemented toggle functionality for Docker and host Claude Code instances with
separate keybindings and automatic Docker image building.
```

```
refactor(Neovim): Improve terminal and helper module integration

Updated both helper.lua and functions.lua to use shared directory detection logic.
```

Hierarchical scope (focused changes):

```
feat(Neovim > plugins): Add `devcontainer.nvim` support
```

```
fix(Neovim > helper): Resolve `read_current_buffer_dir()` returning incorrect path & Add debug logging
```

```
refactor(Neovim > commands): Use `helper.execute_at_git_root()` for consistency
```

## Notes

- Commit directly without asking for confirmation (user approval already given by invoking this command)
- If uncertain about the type or scope, ask the user before committing
- Analyze the actual changed files to determine appropriate scope
- **Do NOT add Claude Code signature** (🤖 Generated with [Claude Code] and Co-Authored-By: Claude) to commit messages
