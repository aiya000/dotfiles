---
name: refine-write-tech-note
description: Fold a review of an article written with `write-tech-note` (Zenn's AI review, a human comment, the user's own edits to the draft) and the lessons from producing it back into the `write-tech-note` skill. Use when the user pastes a review of such an article, asks to improve `write-tech-note` from feedback, or invokes `/refine-write-tech-note`.
allowed-tools: Read, Write, Edit, Glob, Grep, Bash(ls:*), Bash(rg:*), Bash(cat:*), Bash(git diff:*), Bash(git log:*), Bash(git status:*), Bash(wc:*)
---

# refine-write-tech-note

Turn one review into durable changes to `~/.claude/skills/write-tech-note/`, without letting that skill grow into a diary.

The skill being refined consists of:

- `SKILL.md` — hard requirements, workflow, self-review checklist, notes
- `references/style-guide.md` — the user's voice, numbered sections (§1–§7)
- `references/article-patterns.md` — the five skeletons (型A–E)
- `references/review-log.md` — one entry per review, the only place that keeps review text verbatim

## Inputs

Collect these before changing anything. Ask only for what cannot be found.

1. **The review.** From `ARGUMENTS`, the user's message, or a file path. Keep the reviewer's words exactly; they go into the log verbatim. Identify the reviewer: Zenn's AI review, a human (record the role, never the name), or the user's own 推敲 (then the review is `git diff` of the article since the draft commit, and every hunk is a comment).
2. **The article.** The most recent article written with `write-tech-note` in this session, or the path the user names. If neither is known, list the article repositories (`~/Repository/zenn-old/articles/`, `~/Repository/zenn-articles/`, `~/Repository/qiita-draft/`) by modification time and ask. Read it whole, and note its pattern (A–E), register, and line count.
3. **What happened while writing it.** If the article was written in this session, list the production failures from the conversation: output that was typed before it was run, personal identifiers found in captured output, commands the permission system denied, links that had to become `TODO:`, claims that did not reproduce. Reviews never see these, and they are the most reusable lessons. If the article was not written in this session, skip this and say so in the report.

## Classify every point

For each praised point, each criticised point, and each production failure, decide one of:

- **Reinforce** — an existing rule produced this praise. Name the rule (`style-guide §5-9`, `SKILL.md 要件4`, `型C はじめに`). No wording change needed unless the rule is vague; then sharpen it with the concrete example from this article, in one clause.
- **New rule** — nothing in the skill covers it. Decide where it lives by what kind of thing it is:
    - it changes what the article must contain → `SKILL.md` hard requirements (only for things that make an article unpublishable: fabrication, persona leak, personal info)
    - it changes how to produce the article (a command, an order of steps, a sandbox trick) → `SKILL.md` workflow
    - it is something to check before saving → `SKILL.md` self-review checklist, as a runnable `rg` line where possible
    - it is a habit of the user's voice or formatting → `style-guide.md`, in the matching §, as a Bad / Good pair when the review gave both sides
    - it is a section that a pattern should have or drop → `article-patterns.md`, inside that pattern's skeleton, with a one-line 「（〜）」 stage direction
- **Contradiction** — the review conflicts with an existing rule. Do not silently pick a side. Put both in the report with the rule's location, and ask the user, unless the reviewer is the user's own edit (then the user's edit wins and the rule changes).
- **Not generalisable** — specific to this one article (a typo, a fact about the tool). Log it, change no rule.

A review that says 「改善点: 特にありません」 is not a no-op. Every praised point is a **Reinforce**, and the rules it names become harder to trim later. Record the mapping.

## Write the changes

1. Append an entry to `references/review-log.md` in the format its header describes: 記事 / レビュアー / 判定 / 褒められた点 → ルール / 指摘された点 → 直したこと / 制作時の失敗. One line per point, with the rule location on the right-hand side. Points that were classified **Contradiction** and are waiting on the user get a `TODO:` prefix.
2. Apply **New rule** and sharpened **Reinforce** changes to the three rule files. Rules:
    - Before adding, `rg` the three files for the key term. If a rule exists, edit it; never add a second one that says the same thing in other words
    - Each rule carries one concrete example at most, from the article that earned it, in parentheses with the article name and date: `（ps-mem, 2026-09-06: …）`. When a rule already has an example, replace it only if the new one is shorter or clearer
    - Verbatim review text lives in `review-log.md` only. `SKILL.md` notes summarise reviews in one bullet per pattern, pointing at the log; when a note grows past ~6 sub-bullets, distil it and move the detail to the log
    - Do not renumber `style-guide.md` sections or `SKILL.md` requirements; other files point at those numbers. Add sub-bullets, not new numbers, unless the thing is genuinely new
    - Keep the persona out of every file (「なのです」「わたし」「♪」 never appear in the skill; the skill is instructions, not conversation)
    - Do not touch the `allowed-tools` line of `SKILL.md` from this skill; if a new workflow step needs a tool that is not allowed, say so in the report and let the user add it
3. Re-read the edited files once and check: `rg -n 'なのです|ますです|♪|わたし|あいやくん' ~/.claude/skills/write-tech-note/` returns nothing; every `[[`-free cross-reference (`§5-9`, `要件4`, `Workflow 7`, `型C`) still points at something that exists.

## Report

Reply with:

- the review entry as written to `review-log.md` (path and heading)
- each rule file changed, and for each change: the classification (Reinforce / New rule / Contradiction / Not generalisable), the one-line rule, and where it landed
- open questions, each with both sides and the rule location
- anything the user has to do by hand (an `allowed-tools` change, a rule you were not permitted to edit)

If nothing changed apart from the log, say so plainly. Three reviews in a row with no improvements means the rules are working, not that this skill did nothing.

## Notes

- First run (2026-09-06, `ps-mem`): the review had no improvements; all changes came from production failures (sandbox capture with `unshare -Ur`, typing debug output before running it, guessed article slugs, a denied inline `sleep`). Expect the same ratio on future no-improvement reviews and ask about production failures explicitly.
- The user's own 推敲 is the highest-signal review. When the article has been committed and then edited, `git log --format=%h -- <article>` finds the draft commit and `git diff <draft>..HEAD -- <article>` is the review. Every deleted sentence is a rule about what not to write; every rewritten sentence is a Bad / Good pair for `style-guide.md` §3 or §6.
