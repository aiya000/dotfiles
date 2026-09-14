# .kiro_global

Here is template settings for Kiro.

## Setup

```shell-session
$ [[ ! -f ~/.kiro ]] && mkdir ~/.kiro
$ pushd ~/.kiro ; ln -s ~/.dotfiles/.kiro_global/steering . ; popd
$ pushd ~/.kiro ; ln -s ~/.dotfiles/.kiro_global/skills . ; popd
```

For the `agents/` (kiro-cli only), symlink each agent config into `~/.kiro/agents/`
and set the default agent:

```shell-session
$ [[ ! -d ~/.kiro/agents ]] && mkdir -p ~/.kiro/agents
$ pushd ~/.kiro/agents ; ln -s ~/.dotfiles/.kiro_global/agents/nayu.json . ; popd
$ kiro-cli agent set-default --name nayu
```

`agent set-default` writes `chat.defaultAgent` to `~/.kiro/settings/cli.json`, which is
machine-local and not tracked in dotfiles, so run it once per machine.

## Files

- `steering/`: Kiro steering files. `MUST-READ-FIRST.md` is a symlink to `../../AGENTS.global.md`
- `skills/`: Kiro skills. Most entries are symlinks to the matching `../../.claude_global/skills/<name>`, so Claude Code and Kiro share one source of truth
- `agents/`: kiro-cli agent configs (JSON). This is the CLI equivalent of `settings/permissions.yaml`
- `settings/permissions.yaml`: trust rules for **Kiro IDE only**. kiro-cli does not read this file; it uses `agents/` instead

## agents/ vs settings/permissions.yaml

`settings/permissions.yaml` only applies to Kiro IDE. kiro-cli ignores it and reads
agent config JSON under `~/.kiro/agents/` instead. `agents/nayu.json` mirrors the intent of
`permissions.yaml` in the CLI's format:

- `shell` allow list -> `toolsSettings.shell.allowedCommands` (regex; the YAML glob `foo *` becomes `foo( .*)?`)
- `shell` deny (`find`, `grep`, `rm`) -> `toolsSettings.shell.deniedCommands`
- `fs_read` -> `toolsSettings.read.allowedPaths`
- `fs_write` -> `toolsSettings.write.allowedPaths`
- `web_fetch` allow -> `toolsSettings.web_fetch.trusted`
- `web_search` / `subagent` -> listed in `tools`

The built-in `kiro_default` agent cannot be overridden by a same-named config, so the CLI agent
uses a distinct name (`nayu`) and is selected via `agent set-default`.

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
