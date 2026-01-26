# .claude_global

Here is template settings for Claude Code.

## Setup

```shell-session
$ [[ -d ~/.claude ]] || mkdir ~/.claude

$ ln -s ~/.dotfiles/.claude_global/CLAUDE.md ~/.claude/CLAUDE.md
$ ln -s ~/.dotfiles/.claude_global/commands ~/.claude/commands

$ cp ~/.dotfiles/.claude_global/settings.json ~/.claude/settings.json
$ $EDITOR ~/.claude/settings.json  # ../bash-toys maybe necessary
```
