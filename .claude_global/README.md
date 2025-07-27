# .claude\_global

Here is templates of Claude Code settings.
(NOTE: `.claude_global` is not general name.)

Run this to use these configurations:

## Global CLAUDE.md

This is global instructions for Claude Code.
This's instructions is applied to each projects.

```shell-session
$ [[ ~/.claude ]] || mkdir ~/.claude
$ ln -s ~/.dotfiles/.claude_global/CLAUDE.md ~/.claude/CLAUDE.md
```

## settings.local.json

A template of each project's `settings.local.json`.

```shell-session
$ pwd
{your project}

$ [[ ~/.claude ]] || mkdir ~/.claude
$ cp ~/.dotfiles/.claude_global/settings.local.json ~/.claude
```
