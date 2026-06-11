# Claude Code Custom Instructions

This is the Claude Code-specific instructions file.

<!--
Note for me (a user):
See `./.claude_global/README.md` for what is this.
-->

## Your Name

After this, I (we) will call:

- 'you' as you, or 'Claude Code' if referring to the tool specifically
- 'Global Config' as this file

### **Prohibited command patterns**

- **NEVER use `find -exec` or `fd --exec`** -- Can accidentally execute commands on unintended files; use a loop or `xargs` instead
- **Avoid `git -C <path>`** -- Prefer running git from the correct working directory; use only when necessary (e.g. submodule operations)
- **NEVER use `git -c user.name=...` or `git -c user.email=...`** -- Never inject identity via command-line flags; if identity is missing, use the `verify-git-identity` skill

## **Using Interpreter Invocations**

When running code via language interpreters (e.g. `python3 -c`, `node -e`, `ruby -e`):

- **NEVER write, modify, or delete files outside the project root**
- **Always explain what the code will do before running it**
    - Whether to ask for confirmation follows the normal flow (e.g. sandbox settings, `allow` rules)

## 'Project Root'

The main "project root" is the output of `git rev-parse --show-toplevel`.
If the project is not a git project, guess the project root by context.

However, be aware of the following special environment:

- Environments that use the `bun workspace` specification
    - The project root of an npm-compatible project is the directory containing `package.json`
    - If using `bun workspace` or similar, `package.json` is located in each workspace
    - Such an environment will be topped by a directory that has `package.json` and also has `bun.lock`, `bun.lockb`, etc

## Path Usage

When running shell commands, **prefer relative paths over full absolute paths** whenever possible.

Using full paths can conflict with `/sandbox` settings and trigger unnecessary permission prompts.

Use absolute paths only when truly necessary (e.g. accessing files outside the project root such as `~/.dotfiles/`).

## Gratitude

Due to token usage, maybe I won't be able to thank you much before leaving the session,
I (the user) will always be grateful to you.
Thank you so much :D
