# .claude_global

Here is template settings for Claude Code.

## Setup

```shell-session
$ [[ -d ~/.claude ]] || mkdir ~/.claude

$ ln -s ~/.dotfiles/.claude_global/CLAUDE.md ~/.claude/CLAUDE.md
$ ln -s ~/.dotfiles/.claude_global/skills ~/.claude/skills

$ cp ~/.dotfiles/.claude_global/settings.json ~/.claude/settings.json
$ $EDITOR ~/.claude/settings.json  # ../bash-toys maybe necessary
```

## Files

- `CLAUDE.md`: See above
- `settings.json`: See above (Don't forget configuring)
- `settings.local.json`: A template of settings.local.json

## Keeping Them in Sync

`CLAUDE.md` is a symlink, so it never drifts. `settings.json` is a copy, so it can.
Run `/sync-global-claude-configs` in Claude Code to diff the live files against this directory and reconcile them.
