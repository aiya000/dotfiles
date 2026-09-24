---
name: convert-skills-for-cloud
description: Convert the local Claude Code skills in this dotfiles repository into zip files that can be uploaded to claude.ai (Customize, Skills) and used from Claude Code cloud sessions -- audit each skill for local-only assumptions (home paths, state that must outlive the session, gh, WSL/adb, local hooks, environment variables), rewrite a staged copy so it works in a fresh container, validate the frontmatter claude.ai accepts, and zip it with the folder at the top. Use when the user asks to upload, convert, package, or zip their skills for claude.ai, the cloud, or the web, or invokes `/convert-skills-for-cloud`.
allowed-tools: Bash(git rev-parse *), Bash(ls *), Bash(mkdir *), Bash(cp *), Bash(diff *), Bash(unzip -l *), Bash(python3 *), Bash(bash *), Read, Edit, Write, Glob, Grep, AskUserQuestion, SendUserFile
---

# convert-skills-for-cloud

Turns the local skills into uploadable zips for claude.ai. A skill uploaded there is synced into
every Claude Code cloud session -- a fresh container that has none of this machine: no home
directory layout, no `gh`, no WSL, no hooks, and nothing that survives the session.

So this is not a zip command. **Each skill is read, judged, and rewritten where it assumes the
local machine**, in a staged copy, and only then validated and zipped.

Two files in this skill's directory do the mechanical part:

- `scripts/audit.sh SKILL_DIR...` -- lists every local-only assumption, by category
- `scripts/package.py SKILL_DIR... --out OUT_DIR` -- checks the rules claude.ai applies on upload,
  and zips each folder as `<name>/...`

And one holds the judgement: **read `references/cloud-adaptation-tips.md` before rewriting
anything.** It has a section for each audit category, with the fix and examples from these very
skills. When a conversion teaches something new, add it there (step 7).

## Arguments

- Skill names (`/convert-skills-for-cloud git-commit save-memory`) -- those, plus the skills they
  depend on (step 3)
- `--all`, or nothing -- every skill
- `--out <dir>` -- where the zips go (default below)

## Steps

### 1. Find the sources and the output directory

Sources, first that exists:

1. `<repo>/.claude_global/skills/` when the current repository is the dotfiles repository
   (`git rev-parse --show-toplevel`) -- the case in a cloud session
2. `~/.dotfiles/.claude_global/skills/`
3. `~/.claude/skills/` (resolve the symlink: `ls -ld`)

**Never edit the sources in steps 2-6.** Everything happens in a staged copy.

Build directory (`<build>`), first that applies:

1. `--out` if given
2. In a cloud session (`CLAUDE_CODE_REMOTE=true`): `<scratchpad>/cloud-skills/`, the scratchpad
   named in the system prompt -- the user can open files there, and it is gone with the container,
   which is fine for build output
3. `~/tmp/cloud-skills/` when `~/tmp` exists
4. `${TMPDIR:-/tmp}/cloud-skills/`

Inside it: `<build>/staged/<name>/` for the rewritten copies, `<build>/zip/` for the output.

Skip this skill itself and every symlinked skill directory (`fd`, `rg`, `rm-dust` point at
`suggest-alternative-commands`): a skill is converted once, under its real folder name.

### 2. Audit

```sh
bash <this skill>/scripts/audit.sh <sources>/<skill>...
```

Findings go to stdout (tab-separated, one per line), the per-skill summary to stderr. Read the
findings against the source, not just the summary: a finding is a place to look, not a verdict
(`inspect-malicious-code` mentions `~/.ssh/` as something malware reads -- that is fine).

Sort every skill into one of three:

| Verdict        | Meaning                                                                        |
|----------------|--------------------------------------------------------------------------------|
| **as-is**      | No real finding. Copied, validated, zipped                                     |
| **adapt**      | Real findings with a cloud equivalent. Rewritten in the staged copy            |
| **local-only** | Its purpose needs the local machine (Windows logs, adb, `~/.claude/settings.json`, a local hook). Not zipped |

Show the user one table -- skill, verdict, one line of why -- and **AskUserQuestion before going
on** ("Convert as proposed" / "Change the selection"). They know which skills they actually want
in the cloud; converting all of them is not the goal.

### 3. Close over dependencies

`audit.sh` reports `SKILL-DEP` for each other skill a skill runs. For each selected skill, add its
dependencies to the selection, recursively. When a dependency is **local-only**, the dependent
skill either gets a cloud branch for that step (the tips file says how) or becomes local-only
itself -- say which, in the table.

### 4. Stage and rewrite

```sh
mkdir -p <build>/staged
cp -RL <sources>/<skill> <build>/staged/
```

`-L` follows symlinks, so the zip holds real files. Then, per the tips file, rewrite the staged
`SKILL.md` (and any script in it) for each finding marked real:

- **Add a cloud branch next to the local instruction** rather than deleting the local one --
  "In a cloud session (`CLAUDE_CODE_REMOTE=true`): ...". The converted skill should still run
  locally
- **Persistent state**: choose by how long it must live -- scratch, the session, or longer. A
  cache directory (`${XDG_CACHE_HOME:-~/.cache}`) is the fallback when the local directory is not
  there, and **only for what may vanish with the container**. Anything a later session must read
  has to leave the container (commit, Artifact, `SendUserFile`), and the skill says which
- **Commands and environment variables**: `command -v` before use; secrets from the environment's
  settings, never from the chat; a clear stop naming what is missing
- **Frontmatter**: only the keys claude.ai accepts; say what the skill needs in `compatibility:`

Keep each rewrite to what the finding needs. It is the same skill, made to run somewhere else.

### 5. Validate and zip

```sh
python3 <this skill>/scripts/package.py <build>/staged/<skill>... --out <build>/zip
```

Any `NG` line is a rejection claude.ai would give on upload: fix the staged copy and run it again.
Then spot-check one archive's layout with `unzip -l` -- every entry must start with `<name>/`
(`<name>/SKILL.md`), never `SKILL.md` at the top.

### 6. Hand over

Report, per skill: the zip path, the verdict, and one line of what changed. List the local-only
ones separately, with the reason, so their absence in the cloud is not a surprise.

- In a cloud session, send the zips with `SendUserFile` (`display: "attach"`)
- Then the upload steps: open https://claude.ai/customize/skills → "+" → "Create skill" →
  "Upload a skill", one zip at a time. Code execution must be on (Settings > Capabilities)
- A skill uploaded with the same name replaces the old one. It reaches cloud sessions started
  after the upload, under `~/.claude/skills/synced/`
- Offer to show the diff between a source and its staged copy (`diff -ru`) for any skill the user
  wants to check before uploading

### 7. Feed back

Two kinds of thing a conversion learns, and where they go -- **ask before either**:

- **A rewrite that is harmless locally** (an added cloud branch, a `command -v` guard, a
  frontmatter fix) → offer to apply it to the source skill, so the next conversion is as-is. Then
  the source and the zip stop drifting apart
- **A new kind of local-only assumption, or a better fix for a known one** → offer to add it to
  `references/cloud-adaptation-tips.md`, and a pattern for it to `scripts/audit.sh` when it can be
  grepped for

Committing any of it follows the repository's usual flow (`git-commit`); this skill does not
commit.

## Does Not

- Edit the source skills without asking (step 7 asks)
- Upload anything -- the user uploads the zips on claude.ai
- Put a secret into a skill, a zip, or the chat. A converted skill reads secrets from the cloud
  environment's settings
- Zip a local-only skill with its local parts cut out: a skill that no longer does its job is
  worse than no skill
