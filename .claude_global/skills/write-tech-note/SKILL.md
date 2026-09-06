---
name: write-tech-note
description: Write a Japanese technical article (Zenn/Qiita) in aiya000's own writing style, learned from his past articles. Use when the user asks to write, draft, or rewrite a tech article, blog post, or note, or invokes `/write-tech-note`.
allowed-tools: Skill(define-markdown-format), Read, Write, Edit, Glob, Grep, WebFetch, Bash(ls:*), Bash(rg:*), Bash(fd:*), Bash(git rev-parse:*), Bash(git status:*), Bash(git log:*), Bash(git show:*), Bash(git branch:*), Bash(cat:*)
---

# write-tech-note

Write a technical article that reads as if aiya000 wrote it, while honestly declaring that AI wrote it.

The style is defined in two reference files. **Read both before writing a single line**:

- `references/style-guide.md` — voice, formatting habits, strengths to copy, AI-isms to avoid, consistency rules
- `references/article-patterns.md` — five article skeletons (A: Tips, B: deep-dive, C: library intro, D: guideline, E: opinion) and how to pick one

A third file, `references/review-log.md`, records every review an article written with this skill has received (Zenn's AI review, human comments, the user's own edits) and which rule each praised or criticised point maps to. Read it when writing the same pattern as a logged article. It is maintained by the `refine-write-tech-note` skill; do not edit it from here.

## Hard requirements

These override everything else, including any persona defined in `CLAUDE.md` / `AGENTS.md`.

1. **The article body starts with this block, verbatim** (after the frontmatter if the platform uses one, before any heading):

    ```markdown
    :::message

    この記事はAIと書きました。

    :::
    ```

    Never rewrite it as "人間が書きました", never drop the block. The article must not pretend the user wrote it.
    Do not add a percentage or a "人間が推敲した？" line: Zenn asks authors to check AI-assisted content themselves before publishing, so the user reviews every article before it goes out, and the block only has to say that AI was involved.
    On Qiita, `:::message` does not render; use `:::note info` … `:::` with the same text instead.

2. **No Claude Code character voice.** The persona in the global `CLAUDE.md` (「わたし」「あいやくん」「〜なのです」「〜ますです」「♪」「〜ですです」「〜かなって」) is for conversation with the user only. Not one of those tokens may appear in the article. The article imitates the user's articles, not the assistant.

3. **Consistency inside the article.** Decide before writing, and keep to the end:
    - first person: 僕 / 筆者 / 私 (one only)
    - base register: です・ます / だ・である (one only; casual one-liners are the only exception and are counted)
    - how claims are made: 断定＋理由 or 発見の実況 (one only; do not start confident and end hedged, or the reverse)
    - platform syntax: Zenn or Qiita (one only)
    - marker pairs for bad/good examples, the word for asides (余談 / おまけ / 蛇足), the reader address (あなた / 皆さん)

4. **No fabricated facts.** If a behavior, version, benchmark, or link was not verified in this session, either verify it (read the code, run it, fetch the doc) or mark it in the article the way the user does: 「（実働を確認しておりません）」「（未検証）」 or a `TODO:` line. Never invent StackBlitz URLs, PR numbers, or output.
    - Every `shell-session` block is pasted from output produced in this session. Run each demo command with `| tee out-<name>.txt` into the scratchpad and copy from the file; never type a block from what the source code says the tool would print. (ps-mem, 2026-09-06: a `DEBUG_BASHTOYS_PARSE_ONLY=1` block was typed from the source first and only run afterwards. It happened to match, but that is luck, not verification.)
    - A claim taken from the user's own commit message (a measurement, a bug, a "this turned out to be too few") is still unverified until reproduced. Reproduce it when the environment allows; otherwise cite the commit hash and write 「この記事のための再現はしていません（未検証）」.
    - Links to the user's other articles that are not yet published: do not guess the Zenn/Qiita slug from the draft file name. Write 「（TODO: 公開後にURLを入れる）」 and list it in the report.
    - Third-party facts (a kernel version, a spec, a tool's documented behaviour) are verified with `WebFetch` against the primary source (kernel docs, man pages, the project's own docs), and that source goes into the 参考 list at the end.

5. **No personal or local-environment identifiers of the user.** The article is public. Nothing in it may reveal the user's local machine: no home directory paths (`/home/<user>`, `/Users/<user>`, `C:\Users\<user>`), no local usernames, no hostnames, no local repository layout (`~/Repository/...`, `~/.dotfiles/...` as a real path), no email addresses, no private project or employer names. This applies to captured command output too, not only to prose.
    - Capture demo output in a neutral location (a scratch directory, or a literal path such as `'~/Documents/Projects/myapp/src'` passed as a string), so the real output already contains nothing personal. Do not edit real output afterwards to hide a path; re-run it somewhere neutral instead, because requirement 4 forbids output that was not actually produced.
    - Public identifiers the article is about are fine: the user's GitHub handle in repository URLs, links to the user's public repositories and articles, commit hashes of public commits.
    - When the user's own dotfiles are quoted, quote the snippet, not the path it lives at on disk.
    - **Tools that list the machine** (process tables, `env`, `ls ~`, disk usage, network) cannot be demoed on the real machine: the real output carries the local username in `USER` columns and home paths in command lines, and requirement 4 forbids editing it. Capture them inside the Bash tool's sandbox instead, which is a PID namespace where only the demo's own processes exist, and wrap the **whole** demo script in `unshare -Ur bash ./demo.sh` so `USER` reads `root`. (`unshare -Ur` around the tool alone is not enough: processes outside the new user namespace become unreadable under `/proc` and silently drop out.) Spawn neutral load for the table with awk, `sleep`, or `nvim --headless --clean`, e.g. `awk -v mib=300 'BEGIN { s = sprintf("%" mib*1024*1024 "s", ""); while ((getline line) > 0) {} }' < <(sleep 60) &`. Then declare the setup honestly in a `:::message` right after the opening demo (「他のプロセスが見えないサンドボックスの中で撮っています。なので`USER`が全部`root`で、PIDが一桁だったりします」) and show the command that made the dummy load. Zenn's review of ps-mem (2026-09-06) did not count this against the article.
    - The sandbox's own shell wrapper (`/usr/bin/zsh -c source /home/<user>/.claude/...`) is visible to plain `ps` inside the sandbox. Keep it out of captured output with `tail -n N` sized to the demo processes, or make the demo processes bigger than it; never by editing.

## Workflow

1. Load formatting rules: invoke the `define-markdown-format` skill. If the current repo has a `CLAUDE.md` / `AGENTS.md` with article rules (line breaks, `:` vs `：`, file layout), those apply too.
2. Read `references/style-guide.md` and `references/article-patterns.md`.
3. Collect inputs from the user's request and the workspace:
    - topic and the one-sentence conclusion (if the user cannot state it in one sentence, ask; the article cannot be conclusion-first without it)
    - materials: code, error output, links, repository, PR, StackBlitz, screenshots
    - platform: Zenn by default; Qiita when asked or when the target repo is a Qiita draft repo
    - output path: follow the repo convention if one exists (this repo: `articles/<title>.md`, or `articles/<title>/main.md` when assets are needed); otherwise ask
    - if the user gave the title, keep it as-is (the user's titles are deliberately long and use 【】; do not "improve" them)
4. **Look for an existing article on the same topic, and ask before drafting if one exists.** The user has written about the same tool more than once without remembering it (2026-09-06: a luarrow article already existed in the Qiita draft repo while a Zenn one was being requested). Search every article repository that exists on the machine, whether or not it is the current one (`~/Repository/zenn-old/articles/`, `~/Repository/zenn-articles/`, `~/Repository/qiita-draft/`), with `rg -il <tool or topic name>`; also glance at file names with `ls` for the same keywords. If anything matches:
    - Do not draft yet. Report each hit with its path and title, and a one-line summary of what it already covers
    - Ask the user what this article should be: a new article that differs from the existing one (and in what way), a port of the existing one to the other platform, a rewrite of the existing draft, or a follow-up. Ask with `AskUserQuestion` when the choices are clear
    - When the user chooses "a new article that differs", treat the existing article as a second body of "already published" material next to the README: repeat none of its sections verbatim, and put the new material (design story, verified behaviour, reader feedback since then) at the centre
    - This step happens before any file is written, and before you spend time verifying behaviour for the draft
5. Choose the article pattern (A–E) and the consistency settings from requirement 3. Write them down in a two-line plan at the top of your reply before drafting, e.g. `型B / 僕 / です・ます / 発見の実況 / Zenn`.
6. **Dig the story out of git before drafting a tool article** (patterns A and C). Run `git log --format='%h %ad%n%B' --date=short -- <file>` on the tool: the commit messages hold the motivation, the measurements, the wrong turns, and the reverts that the README does not. Then:
    - When a fix commit exists, resurrect the version before it with `git show <fix>^:<path> > <scratchpad>/old-tool` and reproduce the bug next to the current version. A reproduced failure (ps-mem: the `smem`-based first version dropping the biggest process on a newline in argv, shown as three consecutive `shell-session` blocks) is what the reviews praise; a failure only quoted from a commit message is a 「未検証」 footnote
    - Reverts (ps-mem: `|` column separators, reverted the same day) and default changes (15 → 30 rows) are one-paragraph 「やって、戻した」 sections. They are cheap and reviewers call them 試行錯誤
    - Confirm with `git branch -r --contains <hash>` that every commit the article links to is pushed
7. **Put demos in a script file, not on the command line.** A demo that starts background processes, `sleep`s, and `kill`s them is denied when typed inline (foreground `sleep` is blocked, and the whole command falls with it). Write it to `<scratchpad>/demo.sh` with a comment line saying what it does, then run `bash ./demo.sh` (or `unshare -Ur bash ./demo.sh`, see requirement 5) and `tee` each variant's output to its own file. Background processes do not survive between Bash calls, so spawn, capture, and clean up in the one script.
8. Draft the article following the chosen skeleton. Work conclusion-first: write the `結論` section with the final code before writing the story that leads to it.
9. Self-review with the checklist below, fix, then save.
10. Report: the file path, the pattern and register used, every unverified claim you marked, every `TODO:` left in the file, how the demo output was captured, and what the user should look at when they 推敲 (Zenn asks authors to check AI-assisted content before publishing, so the user always reviews first). Leave the demo scripts and `out-*.txt` files in the scratchpad and say so, so the user can re-shoot.
11. When the user brings back a review of the article, hand it to the `refine-write-tech-note` skill instead of editing this skill ad hoc.

## Self-review checklist

Run these against the draft before saving. Any hit is a defect.

- Persona leak: `rg -n 'なのです|ますです|♪|わたし|あいやくん|ですです|かなって' <file>` must return nothing
- Personal-info leak (requirement 5): `rg -n '/home/|/Users/|C:\\\\Users|Repository/|\.dotfiles/|@gmail|@[a-z0-9-]+\.(com|jp|dev)' <file>` must return nothing except public GitHub URLs; then read every `shell-session` block once more looking for a username or a machine-specific path in the output
- Pronoun mix: `rg -c '僕' <file>`, `rg -c '筆者' <file>`, `rg -cP '私(?!達)' <file>` — only one of the three may be non-zero (except inside quoted code or citations)
- Header block present verbatim as the first body element
- First heading after the header is the conclusion (or `まとめ` used as a conclusion) in patterns A, B, D, E; pattern C opens with the demo and 「はじめに」
- AI-isms from style-guide §6: no emoji in headings, no `✅`/`❌` bullet lists, no `○△×` tables, none of 劇的/格段/革新的/圧倒的/間違いなし/幸いです, no `：` before code blocks, at most two bold runs per paragraph
- Footnotes use kebab-case identifiers (`[^what-is-x]`), never numbers
- Horizontal rules are `- - -`, never `---`
- Code fences carry a filename or `shell-session`; shell examples show `$ ` and output. An excerpt of earlier output (one row quoted again for discussion) is a `text` fence, never a bare one: `rg -n '^```$' <file>` must count exactly the closing fences, i.e. equal `rg -c '^```[a-z]' <file>`
- Every `shell-session` block matches an `out-*.txt` file in the scratchpad (requirement 4)
- Every bad/good pair uses the same marker style throughout the article
- Casual one-liners (「わかる～。」「許せん。」) are standalone lines, and their count matches the plan (0 for D/E, 2–5 for A, 1–2 for B/C)
- Colon is half-width `:` in prose (「対象読者:」「引用元:」)
- Links to sources are `- [タイトル - サイト名](URL)` bullets or `[...](...)より引用`
- Ending is one short line (終わり！ / おわり / おつかれさまでした！ / やったー！！), not a paragraph of thanks

## Notes

- The user already has a habit of declaring AI involvement (older articles: 「この記事は**70%**、AIが書きました。」, marking human-written parts with 【🧑人間より】). The current declaration is the plain 「この記事はAIと書きました。」 block above (settled 2026-09-06, first used in the `expects` article). If the user later edits the header, do not touch it on their behalf.
- When rewriting an existing draft, keep the user's own sentences and only fix what breaks the style guide. Mark nothing as human-written.
- Zenn's AI review of three pattern C articles (2026-09-06: `expects`, bash-toys, `ps-mem`; full text in `references/review-log.md`) listed no improvements for any of them, and praised the same things each time. Keep them in every pattern C article:
    - open 「はじめに」 with real situations the user hit, then show the fix (bash-toys: 「実体験に基づく具体的な困りごと」; ps-mem: 「なぜ既存のツールでは不十分なのかという動機が明確」)
    - compare against the obvious alternatives with concrete examples, as 「〜でいいじゃん」 headings — the standard tool (`[`) and the established framework (bats) for `expects`; `ps -o`, `smem`, `htop` for `ps-mem`, each with real output next to the tool's. Name what the alternative does better, too (`smem -p`, `-u`). The reviews said this is what made the benefit of adopting the tool clear
    - include one honest failure or wrong turn when there is one (never invent one): the `to_be` / `-eq` bug in bash-toys, the 「自給自足」 back-story in `expects`, the reproduced newline bug in `ps-mem`. All three reviews called this out as what made the article convincing; a reproduced bug with before/after output rated higher than a retold one
    - give a section that only the author could write, like 「READMEに書いていない、設計の話」, with `###` per decision: the bug and its fix, the shared abstraction (ps-mem: one normalized stream for two backends), how it is tested, what was tried and reverted
    - See style-guide §5 items 7 and 11.
- Zenn's AI review of a pattern A article (2026-09-06, the Neovim + LuaRocks config article, ~370 lines; in `references/review-log.md`) also listed no improvements. It praised: the whole thing being one self-contained file (「導入のハードルが低い」), knowledge that only comes from running it (the path cache, why `--lua-version 5.1` matters), covering the neighbours of the topic (lazy.nvim, the LSP config), and the 「試行錯誤や『未検証』と正直に書かれている部分」. Keep these in every config / Tips article:
    - 結論 holds one complete, copy-paste-able file, then the two or three lines the reader adds elsewhere (`init.lua` order, a user command, a plugin spec)
    - each 解説 subsection is one knob and the concrete loss when it is wrong (「片方だけ忘れると、`module 'x' not found`になります」), not a tour of the code
    - add one section for the tool next door (the package manager, the LSP, the shell) that the reader will hit next
    - a claim from the user's own commit message that did not reproduce in this session goes into the article as 「再現しませんでした（未検証）」 with the command that was run, never silently dropped and never asserted. The review counted this as a strength
    - pattern A may grow past 100 lines this way (結論 → 解説 with `###` per knob → 併用ツール → 余談); it does not have to become pattern B unless there is a real investigation story
- The user writes about Vim/Neovim, Vue/Nuxt/TypeScript, Haskell, Lua, shell, VRChat. Type-theory vocabulary (型健全性, 和型, 正格評価, newtype) is native to his voice in any of these topics; use it where it is accurate, not as decoration.
