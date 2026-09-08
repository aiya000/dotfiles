---
name: sync-global-kiro-skils
description: Reconcile the live global Kiro permissions under ~/.kiro/settings with their version-controlled template under ~/.dotfiles/.kiro_global/settings, while verifying the global skills and steering links. Use when the user asks to sync, mirror, or reconcile global Kiro settings with dotfiles, or invokes /sync-global-kiro-skils.
compatibility: Requires macOS, Kiro, diff, stat, readlink, a YAML parser such as Ruby Psych, and permission to read and update the selected files under ~/.kiro and ~/.dotfiles.
---

# Sync global Kiro settings

Keep the live global Kiro permissions and their dotfiles template from drifting apart without copying Kiro runtime data or replacing linked Skill trees.

## Scope

| Target        | HOME, loaded by Kiro                | DOTFILES, version-controlled                         | Expected relation                                          |
| ------------- | ----------------------------------- | ---------------------------------------------------- | ---------------------------------------------------------- |
| Skills root   | `~/.kiro/skills`                    | `~/.dotfiles/.kiro_global/skills`                    | HOME is a symlink to DOTFILES; verify only                 |
| Steering root | `~/.kiro/steering`                  | `~/.dotfiles/.kiro_global/steering`                  | HOME is a symlink to DOTFILES; verify only                 |
| Permissions   | `~/.kiro/settings/permissions.yaml` | `~/.dotfiles/.kiro_global/settings/permissions.yaml` | DOTFILES is a portable template; HOME is a configured copy |

The Skills root contains this Skill itself. Never copy, mirror, replace, or recursively synchronize either Skills directory. When the link is healthy, both paths already refer to one tree.

## Expected differences

The permissions files are expected to differ in machine-specific places. These differences are not necessarily drift:

- `{YOUR-HOME}` placeholders in DOTFILES versus the real home path in HOME
- Workspace-specific `fs_read` and `fs_write` paths
- Machine-specific executable paths
- Temporary, project-specific, or exact shell permission rules generated during local work
- Comments or quoting differences that do not change the YAML value

Drift is a general rule that should apply across machines but exists on only one side, such as a reusable shell rule, a global Skill permission, or a global capability setting.

## Excluded data

Do not synchronize or delete any of the following:

- `~/.kiro/settings/cli.json` unless a future version of this Skill explicitly adds a DOTFILES template
- `feed_state.json`, `survey_state.json`, `argv.json`, or `.trust-migration.json`
- `logs/`, `sessions/`, `session-index/`, `workspace-roots/`, `extensions/`, or `powers/`
- Workspace-local `.kiro/` directories
- Any file outside the table in this Skill

## Workflow

### 1. Preflight

1. Confirm that `~/.kiro` and `~/.dotfiles/.kiro_global` exist and are readable
2. Confirm that HOME Skills and Steering paths are symbolic links
3. On macOS, inspect each link with `/usr/bin/readlink` and compare its resolved destination with the corresponding DOTFILES path
4. Classify each link as one of the following:
   - Healthy link to the expected DOTFILES directory
   - Broken link
   - Link to a different destination
   - Regular file or directory instead of a link
5. If either link is not healthy, show its type, current destination when available, and expected destination, then stop before changing anything
6. Never repair, replace, move, or delete a malformed Skills or Steering path automatically
7. Confirm that both permissions paths exist as readable regular files
8. Check `git status --short -- .kiro_global/settings/permissions.yaml .kiro_global/skills/sync-global-kiro-skils/SKILL.md` from `~/.dotfiles` and report pre-existing changes without modifying them

If the permissions HOME file is missing, treat DOTFILES to HOME as a candidate whole-file direction, but still obtain explicit approval before creating it.

### 2. Inspect the permissions difference

1. Run `/usr/bin/diff -u <DOTFILES> <HOME>` and handle its exit status correctly:
   - `0`: files are identical
   - `1`: files differ; continue
   - Greater than `1`: the comparison failed; report the error and stop
2. Read each file's modification time with macOS-compatible `/usr/bin/stat -f`
3. If the files are identical, report that no permissions synchronization is needed and continue to verification
4. If they differ, show the complete unified diff in a fenced code block and show both modification times
5. Classify each hunk as one of the following:
   - Expected machine-specific difference
   - General drift suitable for the DOTFILES template
   - General drift suitable for the live HOME file
   - Ambiguous and requiring user judgment
6. Do not treat a newer modification time as authority. It may be mentioned only as context

### 3. Ask how to reconcile

Present these choices and wait for an explicit user selection:

- **Hunk by hunk**: recommended; decide the destination of each logical change
- **Whole file: DOTFILES to HOME**
- **Whole file: HOME to DOTFILES**
- **Manual edit**
- **Cancel without changes**

Never choose a direction, merge a hunk, or write either permissions file without user approval.

### 4. Apply the selected method

#### Hunk by hunk

Walk the hunks in order. For each logical group, show enough surrounding context and ask the user to choose one action:

- Port to HOME
- Port to DOTFILES
- Keep as an expected difference
- Skip pending clarification

When porting to DOTFILES:

- Never write a real home directory, private path, machine identifier, project checkout, or other machine-specific value into the template
- Replace portable home-directory values with `{YOUR-HOME}` only after showing the proposed normalized value and receiving approval
- Do not generalize a project-specific permission unless the user explicitly requests it

When porting to HOME, preserve the configured real paths and do not insert unresolved placeholders.

#### Whole file

Before a whole-file replacement, show the source, destination, complete consequences, and ask for a second explicit confirmation.

- DOTFILES to HOME can place unresolved placeholders into the live configuration and remove machine-specific permissions
- HOME to DOTFILES can expose real home paths, private paths, machine-specific rules, or local project paths

For HOME to DOTFILES, do not perform a literal whole-file replacement when sensitive or machine-specific values are present. Offer a normalized proposed template or return to hunk-by-hunk mode.

Use file reading and writing tools or an atomic temporary-file replacement. Do not rely on interactive shell aliases. Confirm that the destination write completed before continuing.

#### Manual edit

1. Write the unified diff to `~/tmp/kiro-sync-global-kiro-skils/permissions.yaml.diff`
2. Tell the user the diff path
3. Provide this command for the user's own terminal:

   ```shell
   nvim -d ~/.kiro/settings/permissions.yaml ~/.dotfiles/.kiro_global/settings/permissions.yaml
   ```

4. Stop and wait for the user to say `done` or invoke `/sync-global-kiro-skils` again
5. Do not edit either permissions file in manual mode

### 5. Validate and verify

After an automated edit, or after the user returns from manual editing:

1. Explain that the YAML validation command only reads the two files and does not modify them
2. Parse both permissions files with an available safe YAML parser. On this macOS environment, Ruby Psych may be used without writing files
3. If no YAML parser is available, do not claim successful validation. Report syntax validation as not run and ask the user how to proceed
4. Re-run the unified diff and classify every remaining hunk
5. The synchronization is complete only when the files are identical or every remaining hunk is an explicitly accepted machine-specific difference
6. Re-check the Skills and Steering links and confirm that they were not changed
7. Re-run targeted `git status` in `~/.dotfiles`

If validation fails, do not commit, push, reload, or continue making unrelated changes. Show the parser error and restore only with explicit user approval.

### 6. Report

Report all of the following:

- Skills link status and resolved destination
- Steering link status and resolved destination
- Permissions reconciliation method selected
- Files and hunks changed
- Remaining accepted machine-specific differences
- YAML validation result
- Any skipped or unresolved item
- Targeted `~/.dotfiles` Git status

If a DOTFILES file changed, state that the change is uncommitted and offer the `git-commit` Skill. Do not commit or push automatically.

## Safety rules

- Do not copy or recursively synchronize `~/.kiro/skills` or `~/.kiro/steering`
- Do not use `rsync --delete`, `rm`, `rm-dust`, or any deletion operation
- Do not repair links automatically
- Do not write before showing the diff and receiving explicit approval
- Do not place real home paths, secrets, private paths, or machine-specific values in the DOTFILES template
- Do not overwrite live machine-specific permissions with placeholders without a second confirmation
- Do not modify excluded Kiro settings or runtime data
- Do not stage, commit, or push changes
