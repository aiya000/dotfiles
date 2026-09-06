---
name: apply-to-claude-md-home-root
description: Append general, cross-project information learned this session into the global ~/.claude/CLAUDE.md. Use when the user asks to save something globally, or when apply-to-agents-md-git-root identifies high-importance general information and the user confirms it should also go global.
allowed-tools: Read(~/.claude/CLAUDE.md), Edit(~/.claude/CLAUDE.md), Write(~/.claude/CLAUDE.md), AskUserQuestion
---

# apply-to-claude-md-home-root

Append durable, general-purpose information — not specific to any one project — to the user's global `~/.claude/CLAUDE.md`.

## Steps

1. Identify the information to add:
    - If invoked with a specific excerpt (e.g. handed off from `apply-to-agents-md-git-root`), use that
    - Otherwise, review the current conversation for information that is general-purpose (true across projects — e.g. a Claude Code / sandbox behavior, a tool quirk, a workflow or communication preference) and durable
    - If nothing meets this bar, tell the user there's nothing worth recording globally and stop here

2. Read `~/.claude/CLAUDE.md`

3. **Secret scan** the content before writing (see below)

4. Merge the new information into the most relevant existing section, or add a new section if none fits
    - Keep it concise: the fact, and where non-obvious, why it matters
    - Do not duplicate information already present

5. Write the updated file

6. Report to the user what was added and where

## Secret Scan

Before writing, check the content for:

- API keys and tokens: strings starting with `ghp_`, `gho_`, `AKIA`, `sk-`, `xox`, or matching `-----BEGIN.*PRIVATE KEY`
- Variables with sensitive names holding a value: patterns like `API_KEY=`, `_SECRET=`, `_TOKEN=`, `PASSWORD=`
- Hardcoded absolute home paths: `/Users/<name>/` or `/home/<name>/` (prefer `~`)
- Personal or organizational proper nouns that could identify specific individuals or organizations

If any of the above are found, use AskUserQuestion to present:

- "Save as-is" — proceed despite the finding
- "Fix then save" — cancel so the user can fix it first
- "Cancel" — abort without writing

Only proceed if the user selects "Save as-is", or if nothing suspicious was found.

## Does Not

- Add project-specific information — that belongs in the project's `AGENTS.md` instead
- Rewrite or reorganize unrelated existing sections
