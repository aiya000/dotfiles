---
name: sync-global-claude-configs
description: Reconcile the live global Claude Code configs under `~/.claude/` (`CLAUDE.md`, `settings.json`) with their version-controlled sources under `~/.dotfiles/.claude_global/`, by showing the diff and either copying whole files, porting chosen hunks, or handing the diff to the user to edit by hand. Use when the user asks to sync, mirror, or reconcile the global CLAUDE.md or settings.json with the dotfiles copy, or invokes `/sync-global-claude-configs`.
allowed-tools: Bash(readlink *), Bash(stat *), Bash(diff *), Bash(cp *), Bash(ln *), Bash(mkdir *), Bash(cat *), Bash(jq *), Bash(git diff *), Bash(git status *), Read(~/.claude/CLAUDE.md), Read(~/.claude/settings.json), Read(~/.dotfiles/.claude_global/*), Edit(~/.claude/CLAUDE.md), Edit(~/.claude/settings.json), Edit(~/.dotfiles/.claude_global/CLAUDE.md), Edit(~/.dotfiles/.claude_global/settings.json), Write(~/tmp/claude-sync-global-claude-configs/*), AskUserQuestion
---

# sync-global-claude-configs

Keep the live global Claude Code configs and their dotfiles sources from drifting apart.

| Pair          | HOME (what Claude Code loads) | DOTFILES (version-controlled)            | Expected relation                    |
|---------------|-------------------------------|------------------------------------------|--------------------------------------|
| `CLAUDE.md`   | `~/.claude/CLAUDE.md`         | `~/.dotfiles/.claude_global/CLAUDE.md`   | HOME is a symlink to DOTFILES        |
| `settings.json` | `~/.claude/settings.json`   | `~/.dotfiles/.claude_global/settings.json` | DOTFILES is a **template**; HOME is a configured copy |

See `~/.dotfiles/.claude_global/README.md` for how the pair was set up.

`settings.json` is expected to differ in machine-specific places: `{INPUT-YOUR-HOME-DIR}` placeholders,
`sandbox.filesystem.allowWrite` paths, `autoMode.environment`, `model`, and similar. Those differences
are not drift. Drift is a *general* change — a new allow rule, a new hook, a renamed skill — that
landed on one side and not the other.

## Steps

Run the steps for each pair in the table, `CLAUDE.md` first.

1. **Check the link.** `readlink -f <HOME>` and `stat -c '%F' <HOME>`
    - If HOME is a symbolic link resolving to DOTFILES, report "linked, nothing to sync" and move to the
      next pair. There is exactly one file
    - If HOME does not exist, treat it as direction **DOTFILES → HOME** in step 4 (whole file)

2. **Diff.** `diff -u <DOTFILES> <HOME>`
    - Identical: report it and move on
    - Different: continue

3. **Show and ask.** Print the unified diff to the user as a fenced block, plus each file's mtime from
   `stat -c '%y'`. For `settings.json`, say which hunks look machine-specific (expected) and which look
   like general drift. Then use AskUserQuestion with these options:
    - **"Hunk by hunk"** (Recommended for `settings.json`) — go to step 4a
    - **"Whole file: DOTFILES → HOME"** — go to step 4b
    - **"Whole file: HOME → DOTFILES"** — go to step 4b
    - **"I'll edit by hand"** — go to step 4c

   Recommend the newer file as the source when suggesting a whole-file direction, but never choose on
   the user's behalf, and never merge silently

4. **Apply.**
    - **4a. Hunk by hunk.** Walk the hunks in order. For each one, use AskUserQuestion: "port to HOME",
      "port to DOTFILES", "leave (expected difference)". Apply the chosen ports with the Edit tool,
      keeping the template placeholders intact on the DOTFILES side (never write a real home path or
      machine-specific value into the template). Batch consecutive hunks into one question when
      they clearly belong together
    - **4b. Whole file.** `mkdir -p ~/.claude` if needed, then `cp <source> <target>`. For
      `settings.json` in the DOTFILES → HOME direction, warn first that placeholders will land in
      the live file and must be filled in afterwards
    - **4c. By hand.** Write the unified diff to
      `~/tmp/claude-sync-global-claude-configs/<name>.diff` and tell the user:
        - the path of that diff file
        - a command to open both files side by side, for their own terminal:
          `nvim -d ~/.claude/<name> ~/.dotfiles/.claude_global/<name>`
        - to say "done" (or re-run `/sync-global-claude-configs`) when finished

      Then stop and wait. Do not edit either file yourself in this mode

5. **Verify.** After 4a or 4b, or when the user comes back from 4c, run `diff -u` again. For
   `CLAUDE.md` the goal is no output. For `settings.json` the goal is that only the expected
   machine-specific hunks remain — list them so the user can confirm. For `settings.json` also run
   `jq empty` on both files, since a broken settings file silently disables everything in it

6. **Offer the symlink** (`CLAUDE.md` only). If HOME is still a regular file and the two are now
   identical, ask whether to replace HOME with
   `ln -sf ~/.dotfiles/.claude_global/CLAUDE.md ~/.claude/CLAUDE.md`. Only after step 5 confirms
   they match, so nothing is lost. Never symlink `settings.json`: it holds machine-specific values

7. **Report.** For each pair, say what was done. If any DOTFILES file changed, remind the user it is
   uncommitted in `~/.dotfiles` and offer the `git-commit` skill. Do not commit on your own

## Does Not

- Decide a direction, or merge hunks, without asking
- Write real paths or machine-specific values into the DOTFILES template
- Symlink `settings.json`
- Touch `settings.local.json` templates or the `skills/` directory, which have their own setup
- Commit or push anything
