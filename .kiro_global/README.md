# .kiro_global

Here is template settings for Kiro.

## Setup

```shell-session
$ [[ ! -f ~/.kiro ]] && mkdir ~/.kiro
$ pushd ~/.kiro ; ln -s ~/.dotfiles/.kiro_global/steering . ; popd
$ pushd ~/.kiro ; ln -s ~/.dotfiles/.kiro_global/skills . ; popd
```

## Files

- `steering/`: Kiro steering files. `MUST-READ-FIRST.md` is a symlink to `../../AGENTS.global.md`
- `skills/`: Kiro skills. Most entries are symlinks to the matching `../../.claude_global/skills/<name>`, so Claude Code and Kiro share one source of truth

## Adding a skill

Skills live in `.claude_global/skills/` and are exposed to Kiro by symlink:

```shell-session
$ pushd ~/.dotfiles/.kiro_global/skills
$ ln -s ../../.claude_global/skills/<name> <name>
$ popd
```

## Kiro-only skills

Kiro requires the `name:` in `SKILL.md` to match its directory name, so Claude Code's alias symlinks
(`fd`, `rg`, `rm-dust` -> `suggest-alternative-commands`) cannot be linked as-is.
They exist here as real directories instead: thin stubs that just activate
`suggest-alternative-commands`. The rules themselves stay in `suggest-alternative-commands` only.

## Intentionally not migrated

These Claude Code skills are **not** exposed to Kiro, because Kiro covers them natively:

- `read-agents-md`: Kiro loads AGENTS.md as steering on its own
- `notify-instruction-file-loaded`: the announcement is already specified in `AGENTS.global.md` (loaded as `steering/MUST-READ-FIRST.md`)
