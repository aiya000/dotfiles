# Adapting a local skill to the cloud

What changes between the local machine and a skill uploaded to claude.ai, and how to rewrite each
kind of finding `scripts/audit.sh` reports. One section per audit category.

## What the cloud is

An uploaded skill runs in two places:

- **Claude Code cloud sessions** (claude.ai/code, the mobile app) -- the main target. The skill is
  synced to `~/.claude/skills/synced/<bucket>/<name>/` in a fresh Linux container, as root, with the
  repository cloned into the working directory. `CLAUDE_CODE_REMOTE=true` is set there
- **claude.ai chat with code execution** -- a smaller sandbox: no repository, no git credentials,
  and usually no outbound network. A skill that needs any of those should say so in
  `compatibility:` and stop early there

What that container does **not** have, and a skill must not assume:

- **Nothing of the user's home.** No `~/.dotfiles`, no `~/tmp`, no `~/.ai-memory`, no
  `~/.claude/CLAUDE.md`, no `~/.claude/settings.json`, no local hooks, no `bash-toys`
- **Nothing that outlives the session.** The container is reclaimed when the session goes idle or
  ends. Anything not committed and pushed, or published, is gone
- **No GUI, no devices, no Windows.** Nothing to open a browser in, no `adb` device, no
  `powershell.exe`
- **No personal CLIs.** `gh` and `glab` are not installed and cannot be authenticated; GitHub goes
  through the `mcp__github__*` tools instead
- **No environment variables of the user's shell.** Only what the cloud environment's settings
  define (its environment variables and secrets) is there

What it **does** have: `git`, `python3`, `jq`, `rg`, `curl`, `zip`, the usual coreutils, a
per-session scratchpad directory named in the system prompt, the GitHub MCP tools, the Artifact
tools, and whatever the environment's setup script installed.

## Principles

1. **Detect, then branch; do not delete.** Prefer adding a short "In a cloud session" paragraph
   next to the local instruction over rewriting it away. Then the converted skill still works
   locally, and the change can often go back into the source skill (see SKILL.md, step 7)
2. **Check before relying.** Every command that is not in the list above gets a
   `command -v <tool>` first, and a stated fallback -- or a clear stop that says what is missing
   and where the user adds it (the environment's setup script)
3. **Fail loudly and early.** A skill that cannot work in the cloud says so in its first step,
   naming what is missing, rather than half-running
4. **Keep the intent, change the mechanism.** "Open this URL" becomes "show this URL as a link";
   "save to `~/.ai-memory`" becomes "save somewhere that survives this container"
5. **Never widen what the skill is allowed to do.** A conversion swaps a mechanism for an
   equivalent one; it does not add a push, a publish, or a destructive step the source skill did
   not have

## FRONTMATTER

claude.ai rejects the upload outright when these are broken, so they are not optional:

- Keys: only `name`, `description`, `allowed-tools`, `compatibility`, `license`, `metadata`.
  Claude Code-only keys (`argument-hint`, `model`, `disable-model-invocation`, `user-invocable`,
  `context`, `agent`, `hooks`, ...) are dropped, or moved under `metadata:` if the value is worth
  keeping as a note
- `name`: kebab-case, <= 64 chars, **equal to the folder name**
- `description`: <= 1024 chars, no `<` or `>`
- `compatibility` (<= 500 chars) is the place to say what the skill needs, for example
  `Needs git and network access to api.example.com; not for claude.ai chat.`
- Exactly one `SKILL.md`, at the top of the folder. Another `SKILL.md` deeper in (a template, an
  example) must be renamed

`allowed-tools` patterns that name local paths (`Read(~/.ai-memory/*)`, `Bash(wslview:*)`) do no
harm but grant nothing: rewrite them for the new mechanism, and drop the ones that are gone.

## STATE -- files meant to outlive the session

The biggest class. Decide first **how long the file has to live**, then pick the place:

| Needed until...                       | Put it in                                                                         |
|---------------------------------------|-----------------------------------------------------------------------------------|
| the end of this command               | `mktemp` / `${TMPDIR:-/tmp}` (unchanged)                                          |
| the end of this session               | the session scratchpad (from the system prompt), else `${XDG_CACHE_HOME:-~/.cache}/<skill>/` |
| the next session, or longer           | somewhere outside the container -- see below                                      |

Write the directory choice as an ordered fallback, so the same skill still works locally:

```sh
# 1. the local directory the skill has always used, when it exists
# 2. otherwise a cache directory, when one exists
# 3. otherwise /tmp
if [ -d ~/tmp ]; then dir=~/tmp/<skill>
elif [ -d "${XDG_CACHE_HOME:-$HOME/.cache}" ]; then dir="${XDG_CACHE_HOME:-$HOME/.cache}/<skill>"
else dir="${TMPDIR:-/tmp}/<skill>"; fi
mkdir -p "$dir"
```

**A cache directory in the cloud only lasts as long as the container.** It is the right answer
for scratch that one session reuses (a downloaded file, a lock, a cookie jar), and the wrong
answer for anything a later session must read. For that, the skill has to leave the container,
and **it must say which way it went**:

- **Commit it to the repository** on the session's branch -- the natural place for notes about
  that repository. The cloud session's own git rules (designated branch, push) apply
- **Publish it** with the Artifact tool, or a Claude Docs document, and report the link
- **Hand it to the user** with `SendUserFile`, when it is theirs to keep
- **Claude's memory tools**, when the session has them (`memory_write`, `mcp__memory__*`)

Never let a skill silently write "durable" state into the container and report success -- that
reads as saved and is not. If none of the ways out fits, the skill says the file will be lost when
the session ends, and gives its path.

Examples from this repository:

- `save-memory` / `create-handoff` / `read-handoff`: `~/.ai-memory` and `~/tmp/claude-handoff`
  do not exist. `save-memory` stops when `~/.ai-memory` is not a symlink -- keep that stop
  locally, and in the cloud offer the ways out above instead. `read-handoff` in a fresh container
  will find nothing; say so and ask where the handoff went
- `vrchat-force-purge-all-favorite-worlds`: fixed `/tmp` paths are fine -- they only need to
  last one run

## HOME-PATH -- the machine's own layout

- `~/.dotfiles/...` → the repository itself when the session runs in the dotfiles repo
  (`git rev-parse --show-toplevel`), otherwise not available: say so
- `~/.claude/CLAUDE.md`, `~/.claude/settings.json` → not there. A skill that edits them
  (`apply-to-claude-md-home-root`, `sync-global-claude-configs`) is **local-only**
- `/mnt/c/...`, `/Users/<name>/...`, `/home/<name>/...` → local-only; there is no such disk
- A skill's own files: never `~/.claude/skills/<name>/...` -- the synced path is different.
  Refer to them relative to the skill (`scripts/foo.sh` "in this skill's directory"), which is
  what Claude resolves against the path it loaded `SKILL.md` from
- A `~/` that is only an **example** (`inspect-malicious-code` listing what malware reads) is not
  a finding. Leave it

## FORGE-CLI -- `gh` / `glab`

- `gh issue|pr|api|repo ...` → the matching `mcp__github__*` tool: `issue_write`, `list_issues`,
  `search_issues`, `create_pull_request`, `pull_request_read`, `get_me`, ... They are deferred,
  so the skill says to load them with `ToolSearch` first
- `gh auth status` / `gh auth switch` identity checks → `mcp__github__get_me`; the git identity
  is set by the environment, not by the skill
- `--body-file` fallbacks for a broken keyring → not needed; drop them in the cloud branch
- The session's scope decides which repositories are reachable; a repository outside it goes
  through `add_repo`, not a clone with a token
- GitLab has no MCP tool in the cloud: a `glab` path is local-only unless the environment
  installs and authenticates `glab`

## HOST-ONLY -- the user's OS and devices

- Opening a URL (`wslview`, `xdg-open`, `open`) → print it as a Markdown link; the user taps it.
  `open-weburl` then becomes the same as `show-weburl`
- `powershell.exe`, Windows event logs, `/mnt/c` → local-only
- `adb`, emulators, `scrcpy` → local-only
- Notifications (`notify-send`, the `notify` hook) → `PushNotification` when available,
  otherwise nothing
- Clipboard (`pbcopy`, `clip.exe`) → show the text in a code block

A skill that is **only** these is not converted. Report it as local-only.

## LOCAL-TOOL -- the user's own toolbox

- `rm-dust` → `rm` in the cloud (there is no trash to protect and no hook denying `rm`). Keep
  `rm-dust` locally: `command -v rm-dust >/dev/null && rm-dust ... || rm ...`
- `fd` → `find`, or `rg --files -g '<glob>'` (`rg` is installed); `fd` usually is not
- `claude -p` (e.g. `save-memory/count-tokens.sh`) → not available inside a session; drop the
  step or make it optional
- The "use `rg`/`fd`/`rm-dust` instead" skill (`suggest-alternative-commands`) exists because of
  a local hook -- local-only
- Rules from the local global `CLAUDE.md` that a skill leans on (one command per Bash call,
  allow-rule wording, the sandbox's phantom dotfiles) do not apply; a skill that repeats them may
  drop them in its cloud branch

When a tool really is needed, the skill checks with `command -v`, and on a miss **stops and names
it**, pointing at the environment's setup script as the place to install it. It does not
`apt-get install` on its own: the network policy may block it, and the user decides what the
environment carries.

## ENV -- environment variables

Nothing from the user's shell reaches the container. For each variable the skill reads:

- **A secret** (`VRC_USER`, `VRC_PASS`, API tokens) → the skill checks it is set, and when it is
  not, stops and tells the user to add it as a secret in the cloud environment's settings.
  **Never ask for a secret in the chat** -- the conversation is not a secret store
- **A preference with a sensible default** → give it the default: `${FOO:-default}`
- `TMPDIR`, `HOME`, `PWD` → fine as they are

## SANDBOX-HOOK -- instructions for the local sandbox and hooks

- `dangerouslyDisableSandbox`, "read-only inside the sandbox", "the hook rejects ..." → drop in
  the cloud branch. The cloud session has its own sandbox rules; the local workarounds do not
  apply and can mislead
- A skill whose job is a hook (`save-memory`'s PreCompact gate, `offer-handoff.sh`) loses that
  automatic part: hooks are not uploaded with a skill. Say what now has to be done by hand

## NETWORK -- outbound hosts

The cloud environment has a network policy (none / trusted hosts / full, chosen per environment).
A skill that `curl`s an API:

- names the host in `compatibility:` (`Needs network access to api.vrchat.cloud.`)
- on a connection refused or a 403 from the proxy, stops and says the host has to be allowed in the
  environment's network settings -- it does not retry around it

## SKILL-DEP -- skills calling skills

A skill that runs another (`Skill(git-commit)`, "run the `save-memory` skill") only works in the
cloud if that one is uploaded too. SKILL.md's step 3 closes over these. When a dependency is
local-only, the dependent skill needs its own cloud branch for that step, or becomes local-only
itself.

## The cloud session's own rules come first

A cloud session's system prompt already fixes some things, and a skill must not fight them:

- **git**: the designated branch, `git push -u origin <branch>`, commit attribution lines, and
  "no PR unless asked". `git-push` / `github-pr-create` defer to those in their cloud branch
  rather than choosing a branch or opening a PR on their own
- **Files the user can open**: only the working directory and the scratchpad. A skill that
  produces something for the user writes it there, or sends it with `SendUserFile`
- **Persona and language**: the user's claude.ai preferences apply; a skill does not need to
  restate the character from the local `CLAUDE.md`
