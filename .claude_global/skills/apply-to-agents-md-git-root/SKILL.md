---
name: apply-to-agents-md-git-root
description: Distill non-obvious, reusable information from the current session into {git-root}/AGENTS.md for future agents working in this repo, creating a minimal {git-root}/CLAUDE.md pointer if one is missing. Use when the user asks to save session learnings to AGENTS.md, or record project facts learned this session.
allowed-tools: Bash(git rev-parse:*), Read, Write, Edit, AskUserQuestion, Skill
---

# apply-to-agents-md-git-root

Distill useful information from the current session into the project's `AGENTS.md`, so future agents working in this repo start already knowing it.

## Steps

1. Determine `{git-root}`: run `git rev-parse --show-toplevel`
    - If not in a git repository, stop and tell the user this skill requires a git repo

2. Review the current conversation for information that is:
    - Non-obvious and not derivable by reading the code (a gotcha, a hidden dependency between components, a startup/recovery procedure, a networking quirk, an infra fact)
    - Durable — true beyond this one conversation, not a one-off task detail
    - Relevant to *this* repository specifically

    Do not extract:
    - Ephemeral task details, or a chronological blow-by-blow of what was done
    - Anything already documented in `{git-root}/AGENTS.md`, `README.md`, or derivable by reading the code / git log
    - Secrets or credentials

    If nothing meets this bar, tell the user there's nothing worth recording and stop here

3. Read `{git-root}/AGENTS.md` if it exists

4. **Secret scan** the new content before writing (see below)

5. Write the distilled information into `{git-root}/AGENTS.md`:
    - Organize by topic/section — this is a living reference, not a chronological log
    - Merge into existing sections where topically related; add new sections otherwise
    - Keep entries concise — a fact, plus (where non-obvious) the reason it matters

6. If `{git-root}/CLAUDE.md` does **not** already exist, create it with exactly this content and nothing else:

    ```markdown
    Read `AGENTS.md` in this directory before doing anything else.
    ```

    - If `{git-root}/CLAUDE.md` already exists, leave it untouched — do not modify or overwrite it

7. Review what was just added to `AGENTS.md`. If any of it is **general-purpose** (useful beyond this one repo — e.g. a Claude Code / sandbox behavior, a tool quirk, a workflow preference) **and** high-importance, use AskUserQuestion to ask whether to also apply it globally via the `apply-to-claude-md-home-root` skill. If the user agrees, invoke that skill, handing it the relevant excerpt

8. Report to the user what was written and to which file(s)

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

## Notes

- `AGENTS.md` is a reference document, not a log — write it as "how this repo works", not "what happened on this date"
- Never overwrite unrelated existing content in `AGENTS.md` or `CLAUDE.md`
