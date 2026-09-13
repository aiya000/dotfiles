---
name: write-tech-note
description: Write a Japanese technical article for Zenn or Qiita in aiya000's own writing style, learned from his past articles, with the target platform decided from the current directory. Use when the user asks to write, draft, or rewrite a tech article, blog post, or note, or invokes `/write-tech-note`.
allowed-tools: Skill(define-markdown-format), Read, Write, Edit, Glob, Grep, WebFetch, AskUserQuestion, Bash(ls:*), Bash(rg:*), Bash(fd:*), Bash(git rev-parse:*), Bash(git status:*), Bash(git log:*), Bash(git show:*), Bash(git branch:*), Bash(git remote:*), Bash(cat:*), Bash(wc:*), Bash(printf:*)
---

# write-tech-note

Write a technical article that reads as if aiya000 wrote it, for **Zenn or Qiita**.

Which of the two is decided from the current directory, not by a default — see `references/platforms.md` and Workflow 3. The platform then decides the output path, the file format, the title limit, the notation, and part of the self-review, so resolve it before drafting.

The style is defined in three reference files. **Read all three before writing a single line**:

- `references/style-guide.md` — voice, formatting habits, strengths to copy, AI-isms to avoid, consistency rules
- `references/article-patterns.md` — five article skeletons (A: Tips, B: deep-dive, C: library intro, D: guideline, E: opinion) and how to pick one
- `references/platforms.md` — how to tell Zenn from Qiita by the current directory, and every difference that follows from it

A fourth file, `references/review-log.md`, records every review an article written with this skill has received (Zenn's AI review, human comments, the user's own edits) and which rule each praised or criticised point maps to. Read it when writing the same pattern as a logged article. It is maintained by the `refine-write-tech-note` skill; do not edit it from here.

## Hard requirements

These override everything else, including any persona defined in `CLAUDE.md` / `AGENTS.md`.

1. **No Claude Code character voice.** The persona in the global `CLAUDE.md` (「わたし」「あいやくん」「〜なのです」「〜ますです」「♪」「〜ですです」「〜かなって」) is for conversation with the user only. Not one of those tokens may appear in the article. The article imitates the user's articles, not the assistant.

2. **Consistency inside the article.** Decide before writing, and keep to the end:
    - first person: 僕 / 筆者 / 私 (one only)
    - base register: です・ます / だ・である (one only; casual one-liners are the only exception and are counted)
    - how claims are made: 断定＋理由 or 発見の実況 (one only; do not start confident and end hedged, or the reverse)
    - platform syntax: Zenn or Qiita (one only; resolved in Workflow 3, never mixed — `references/platforms.md` §3)
    - marker pairs for bad/good examples, the word for asides (余談 / おまけ / 蛇足), the reader address (あなた / 皆さん)

3. **No fabricated facts.** If a behavior, version, benchmark, or link was not verified in this session, either verify it (read the code, run it, fetch the doc) or mark it in the article the way the user does: 「（実働を確認しておりません）」「（未検証）」 or a `TODO:` line. Never invent StackBlitz URLs, PR numbers, or output.
    - Every `shell-session` block is pasted from output produced in this session. Run each demo command with `| tee out-<name>.txt` into the scratchpad and copy from the file; never type a block from what the source code says the tool would print. (ps-mem, 2026-09-06: a `DEBUG_BASHTOYS_PARSE_ONLY=1` block was typed from the source first and only run afterwards. It happened to match, but that is luck, not verification.)
    - A claim taken from the user's own commit message (a measurement, a bug, a "this turned out to be too few") is still unverified until reproduced. Reproduce it when the environment allows; otherwise cite the commit hash and write 「この記事のための再現はしていません（未検証）」.
    - Links to the user's other articles that are not yet published: do not guess the Zenn/Qiita slug from the draft file name. Write 「（TODO: 公開後にURLを入れる）」 and list it in the report.
    - Third-party facts (a kernel version, a spec, a tool's documented behaviour) are verified with `WebFetch` against the primary source (kernel docs, man pages, the project's own docs), and that source goes into the 参考 list at the end.

4. **No personal or local-environment identifiers of the user.** The article is public. Nothing in it may reveal the user's local machine: no home directory paths (`/home/<user>`, `/Users/<user>`, `C:\Users\<user>`), no local usernames, no hostnames, no local repository layout (`~/Repository/...`, `~/.dotfiles/...` as a real path), no email addresses, no private project or employer names. This applies to captured command output too, not only to prose.
    - Capture demo output in a neutral location (a scratch directory, or a literal path such as `'~/Documents/Projects/myapp/src'` passed as a string), so the real output already contains nothing personal. Do not edit real output afterwards to hide a path; re-run it somewhere neutral instead, because requirement 3 forbids output that was not actually produced.
    - Public identifiers the article is about are fine: the user's GitHub handle in repository URLs, links to the user's public repositories and articles, commit hashes of public commits.
    - When the user's own dotfiles are quoted, quote the snippet, not the path it lives at on disk.
    - **Tools that list the machine** (process tables, `env`, `ls ~`, disk usage, network) cannot be demoed on the real machine: the real output carries the local username in `USER` columns and home paths in command lines, and requirement 3 forbids editing it. Capture them inside the Bash tool's sandbox instead, which is a PID namespace where only the demo's own processes exist, and wrap the **whole** demo script in `unshare -Ur bash ./demo.sh` so `USER` reads `root`. (`unshare -Ur` around the tool alone is not enough: processes outside the new user namespace become unreadable under `/proc` and silently drop out.) Spawn neutral load for the table with awk, `sleep`, or `nvim --headless --clean`, e.g. `awk -v mib=300 'BEGIN { s = sprintf("%" mib*1024*1024 "s", ""); while ((getline line) > 0) {} }' < <(sleep 60) &`. Then declare the setup honestly in the platform's message box right after the opening demo (`:::message` on Zenn, `:::note info` on Qiita — `references/platforms.md` §3): 「他のプロセスが見えないサンドボックスの中で撮っています。なので`USER`が全部`root`で、PIDが一桁だったりします」, and show the command that made the dummy load. Zenn's review of ps-mem (2026-09-06) did not count this against the article.
    - The sandbox's own shell wrapper (`/usr/bin/zsh -c source /home/<user>/.claude/...`) is visible to plain `ps` inside the sandbox. Keep it out of captured output with `tail -n N` sized to the demo processes, or make the demo processes bigger than it; never by editing.

## Workflow

1. Load formatting rules: invoke the `define-markdown-format` skill.
2. Read `references/style-guide.md`, `references/article-patterns.md`, and `references/platforms.md`.
3. **Resolve the platform and the output repository from the current directory, before anything else is decided.** There is no default platform; Zenn is not assumed. Follow `references/platforms.md` §1:
    - `git rev-parse --show-toplevel` (or the cwd, outside git), then match that root against the markers in §1 — not against a remembered path
    - **Inside an article repository** the platform is already decided by where the user launched you. Do not ask. The one exception is an explicit instruction from the user to write for the other platform; then that wins, and the output goes to the other repository, never into the cwd
    - **Outside one** — the usual case, because tool articles get written from the tool's own repository — the cwd is the *material*, not the platform. Take `git log`, README, code and demos from here, and ask with `AskUserQuestion` which platform to write for. Do that only after step 5, so the existing-article search can inform the choice
    - Write the resolved platform, output repository and output path into the two-line plan of step 6
    - Then read the **output repository's** `CLAUDE.md` / `AGENTS.md` / `how-to-write-*.md` (`references/platforms.md` §4) — the cwd's instruction files govern the code, not the article. Their rules on line breaks, `:` vs `：`, title limits and file layout apply to the draft
4. Collect inputs from the user's request and the workspace:
    - topic and the one-sentence conclusion (if the user cannot state it in one sentence, ask; the article cannot be conclusion-first without it)
    - materials: code, error output, links, repository, PR, StackBlitz, screenshots
    - output path: the layout for the resolved platform (`references/platforms.md` §2) — Zenn `articles/<title>.md`, Qiita `<topic>/<title>.md`, each becoming `<...>/main.md` in a directory of its own when the article carries images. On Qiita, `ls` the topic directories and pick one before writing; create a new one only when nothing fits
    - if the user gave the title, keep it as-is (the user's titles are deliberately long and use 【】; do not "improve" them). On Zenn only, check it against the 70-character limit with `printf '%s' '<title>' | wc -m`, and if it is over, propose cuts and let the user choose
5. **Look for an existing article on the same topic, and ask before drafting if one exists.** The user has written about the same tool more than once without remembering it (2026-09-06: a luarrow article already existed in the Qiita draft repo while a Zenn one was being requested). Search every article repository that exists on the machine, whether or not it is the current one (`~/Repository/zenn-old/articles/`, `~/Repository/zenn-articles/`, `~/Repository/qiita-draft/`), with `rg -il <tool or topic name>`; also glance at file names with `ls` for the same keywords. If anything matches:
    - Do not draft yet. Report each hit with its path and title, and a one-line summary of what it already covers
    - Ask the user what this article should be: a new article that differs from the existing one (and in what way), a port of the existing one to the other platform, a rewrite of the existing draft, or a follow-up. Ask with `AskUserQuestion` when the choices are clear
    - When the user chooses "a new article that differs", treat the existing article as a second body of "already published" material next to the README: repeat none of its sections verbatim, and put the new material (design story, verified behaviour, reader feedback since then) at the centre
    - This step happens before any file is written, and before you spend time verifying behaviour for the draft
6. Choose the article pattern (A–E) and the consistency settings from requirement 2. Write them down in a two-line plan at the top of your reply before drafting, with the platform resolved in step 3 and the output path it implies, e.g. `型B / 僕 / です・ます / 発見の実況 / Zenn` and `→ zenn-old:articles/<title>.md（cwd が zenn-old なので Zenn）` (Qiita: `型C / 僕 / です・ます / 断定＋理由 / Qiita` and `→ qiita-draft:Neovim/<title>/main.md（画像あり）`).
7. **Dig the story out of git before drafting a tool article** (patterns A and C). Run `git log --format='%h %ad%n%B' --date=short -- <file>` on the tool: the commit messages hold the motivation, the measurements, the wrong turns, and the reverts that the README does not. Then:
    - When a fix commit exists, resurrect the version before it with `git show <fix>^:<path> > <scratchpad>/old-tool` and reproduce the bug next to the current version. A reproduced failure (ps-mem: the `smem`-based first version dropping the biggest process on a newline in argv, shown as three consecutive `shell-session` blocks) is what the reviews praise; a failure only quoted from a commit message is a 「未検証」 footnote
    - Reverts (ps-mem: `|` column separators, reverted the same day) and default changes (15 → 30 rows) are one-paragraph 「やって、戻した」 sections. They are cheap and reviewers call them 試行錯誤
    - Confirm with `git branch -r --contains <hash>` that every commit the article links to is pushed
    - Link each 「設計の話」 section to the commit that made the decision, as one bullet at the end of the section: `- [コミット: <the commit title>](<url>)`. Zenn's review of CopyMenu (2026-09-12) counted these as what let the reader follow the background
8. **Put demos in a script file, not on the command line.** A demo that starts background processes, `sleep`s, and `kill`s them is denied when typed inline (foreground `sleep` is blocked, and the whole command falls with it). Write it to `<scratchpad>/demo.sh` with a comment line saying what it does, then run `bash ./demo.sh` (or `unshare -Ur bash ./demo.sh`, see requirement 4) and `tee` each variant's output to its own file. Background processes do not survive between Bash calls, so spawn, capture, and clean up in the one script.
9. Draft the article following the chosen skeleton. Work conclusion-first: write the `結論` section with the final code before writing the story that leads to it.
10. Self-review with the checklist below, fix, then save.
11. Report: the platform and which marker in the current directory decided it (or that the user chose it), the file path, the pattern and register used, every unverified claim you marked, every `TODO:` left in the file, how the demo output was captured, and what the user should look at when they 推敲 (the user reviews and reworks every article before it goes out, so point them at the weak spots). Leave the demo scripts and `out-*.txt` files in the scratchpad and say so, so the user can re-shoot. Report any staleness you found in the tool's own README or docs while verifying — it belongs both in the report and, for pattern C, in the article's 「ドキュメントの古い箇所も見つけた」 section.
12. When the user brings back a review of the article, hand it to the `refine-write-tech-note` skill instead of editing this skill ad hoc.

## Self-review checklist

Run these against the draft before saving. Any hit is a defect.

- Platform: the file is where `references/platforms.md` §2 puts it for the resolved platform, and the §5 checks for that platform pass (Zenn: frontmatter present, no `# ` H1, title ≤ 70 chars, no `:::note` / `<details>` / emoji shortcodes; Qiita: `# ` H1 present, no frontmatter, no `:::message` / `:::details`, no trailing-`  ` hard breaks). The other platform's checks are not run
- Persona leak: `rg -n 'なのです|ますです|♪|わたし|あいやくん|ですです|かなって' <file>` must return nothing
- Personal-info leak (requirement 4): `rg -n '/home/|/Users/|C:\\\\Users|Repository/|\.dotfiles/|@gmail|@[a-z0-9-]+\.(com|jp|dev)' <file>` must return nothing except public GitHub URLs; then read every `shell-session` block once more looking for a username or a machine-specific path in the output
- Pronoun mix: `rg -c '僕' <file>`, `rg -c '筆者' <file>`, `rg -cP '私(?!達)' <file>` — only one of the three may be non-zero (except inside quoted code or citations)
- The first heading is the conclusion (or `まとめ` used as a conclusion) in patterns A, B, D, E; pattern C opens with the demo and 「はじめに」
- AI-isms from style-guide §6: no emoji in headings, no `✅`/`❌` bullet lists, no `○△×` tables, none of 劇的/格段/革新的/圧倒的/間違いなし/幸いです, no `：` before code blocks, at most two bold runs per paragraph
- Footnotes use kebab-case identifiers (`[^what-is-x]`), never numbers
- Horizontal rules are `- - -`, never `---`
- Code fences carry a filename or `shell-session`; shell examples show `$ ` and output. An excerpt of earlier output (one row quoted again for discussion) is a `text` fence, never a bare one: `rg -n '^```$' <file>` must count exactly the closing fences, i.e. equal `rg -c '^```[a-z]' <file>`
- Every `shell-session` block matches an `out-*.txt` file in the scratchpad (requirement 3)
- Every bad/good pair uses the same marker style throughout the article
- Casual one-liners (「わかる～。」「許せん。」) are standalone lines, and their count matches the plan (0 for D/E, 2–5 for A, 1–2 for B/C)
- Colon is half-width `:` in prose (「対象読者:」「引用元:」)
- Links to sources are `- [タイトル - サイト名](URL)` bullets or `[...](...)より引用`
- Ending is one short line (終わり！ / おわり / おつかれさまでした！ / やったー！！), not a paragraph of thanks

## Notes

- **The article carries no note about how it was written** (settled 2026-09-12). Older articles opened with 「この記事は**70%**、AIが書きました。」 or a 「この記事はAIと書きました。」 block, and marked human-written parts with 【🧑人間より】; none of that goes in any more. Do not add such a block, a percentage, or a 【🧑人間より】 marker on your own initiative. If the user puts one in by hand, leave it exactly as they wrote it.
- When rewriting an existing draft, keep the user's own sentences and only fix what breaks the style guide. If the draft still carries an old declaration block, remove it.
- Zenn's AI review of five pattern C articles (2026-09-06: `expects`, bash-toys, `ps-mem`; 2026-09-12: chotto.lua, CopyMenu; full text in `references/review-log.md`) listed no improvements for any of them, and praised the same things each time. Keep them in every pattern C article:
    - open 「はじめに」 with real situations the user hit, then show the fix (bash-toys: 「実体験に基づく具体的な困りごと」; ps-mem: 「なぜ既存のツールでは不十分なのかという動機が明確」)
    - compare against the obvious alternatives with concrete examples, as 「〜でいいじゃん」 headings — the standard tool (`[`) and the established framework (bats) for `expects`; `ps -o`, `smem`, `htop` for `ps-mem`, each with real output next to the tool's. Name what the alternative does better, too (`smem -p`, `-u`). The reviews said this is what made the benefit of adopting the tool clear
    - include one honest failure or wrong turn when there is one (never invent one): the `to_be` / `-eq` bug in bash-toys, the 「自給自足」 back-story in `expects`, the reproduced newline bug in `ps-mem`. All three reviews called this out as what made the article convincing; a reproduced bug with before/after output rated higher than a retold one
    - give a section that only the author could write, like 「READMEに書いていない、設計の話」, with `###` per decision: the bug and its fix, the shared abstraction (ps-mem: one normalized stream for two backends), how it is tested, what was tried and reverted
    - the 「設計の話」 `###` do not have to be bugs. They are equally 「the platform hit me」 records (CopyMenu: the clipboard is unreadable in `onCreate`, a transparent activity hides under the IME, the release APK was signed with the debug key) and 「what I decided not to build」 (chotto.lua: no method chaining; `.parse()` became `:parse()` and the old form was deleted). Each ends with a link to its commit
    - the tool's own limit is a section, not an omission: verify the README's own claim and write it when reality is worse (chotto.lua: `--check` output showing a wrong type where the README promised `unknown`). The review called that section 「ユニークで実践的」. CopyMenu's counterpart is the mistake, not the limit — the release APK signed with the debug key, kept as its own `###`
    - See style-guide §5 items 7 and 11, and `references/review-log.md`.
- Zenn's AI review of a pattern A article (2026-09-06, the Neovim + LuaRocks config article, ~370 lines; in `references/review-log.md`) also listed no improvements. It praised: the whole thing being one self-contained file (「導入のハードルが低い」), knowledge that only comes from running it (the path cache, why `--lua-version 5.1` matters), covering the neighbours of the topic (lazy.nvim, the LSP config), and the 「試行錯誤や『未検証』と正直に書かれている部分」. Keep these in every config / Tips article:
    - 結論 holds one complete, copy-paste-able file, then the two or three lines the reader adds elsewhere (`init.lua` order, a user command, a plugin spec)
    - each 解説 subsection is one knob and the concrete loss when it is wrong (「片方だけ忘れると、`module 'x' not found`になります」), not a tour of the code
    - add one section for the tool next door (the package manager, the LSP, the shell) that the reader will hit next
    - a claim from the user's own commit message that did not reproduce in this session goes into the article as 「再現しませんでした（未検証）」 with the command that was run, never silently dropped and never asserted. The review counted this as a strength
    - pattern A may grow past 100 lines this way (結論 → 解説 with `###` per knob → 併用ツール → 余談); it does not have to become pattern B unless there is a real investigation story
- **Every review in `references/review-log.md` so far is Zenn's AI review.** Qiita has no equivalent, so a Qiita article gets no automated feedback: the user's own 推敲 is the only review it will ever receive. The rules the Zenn reviews produced (型C の「〜でいいじゃん」比較節、正直な失敗談、`###` ごとの設計の話、型A の完結した1ファイルと「未検証」の明示) are about the article's substance, not about Zenn, so apply them on Qiita too. Only the notation and layout differ (`references/platforms.md`).
- The user writes about Vim/Neovim, Vue/Nuxt/TypeScript, Haskell, Lua, shell, VRChat. Type-theory vocabulary (型健全性, 和型, 正格評価, newtype) is native to his voice in any of these topics; use it where it is accurate, not as decoration.
