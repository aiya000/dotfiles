---
name: save-memory
description: Export and distill the current conversation into a memory file. Use when the user asks to save, remember, or note the current session for future context.
allowed-tools: Bash(date *), Bash(ls *), Bash(printf *), Write(~/.ai-memory/*), Read(~/.ai-memory/*)
---

# save-memory

Distills the current conversation into a compact memory file that a future session loads to
restore context. **Only an AI reads these files, never a person.** Write for a model that will
pay tokens for every character: dense, English, no prose.

## Steps

1. Use the current conversation as the source
2. Get the timestamp: `date +%Y-%m-%d_%H:%M`
3. Verify the memory directory (Bash):
    - `ls -ld ~/.ai-memory` must start with `l` (a symlink). If not a symlink, or missing,
      **stop and ask the user to set it up**. Do not create it
    - `ls ~/.ai-memory` to see existing files; reuse a file for the same project+topic
4. **Secret scan** the content you are about to write:
    - Keys/tokens: `ghp_`, `gho_`, `AKIA`, `sk-`, `xox`, `-----BEGIN.*PRIVATE KEY`
    - Sensitive assignments: `API_KEY=`, `_SECRET=`, `_TOKEN=`, `PASSWORD=`
    - Absolute home paths (`/home/<name>/`, `/Users/<name>/`) -- write `~` instead
    - Proper nouns identifying a person or organisation

    On any hit, AskUserQuestion: "Save as-is" / "Fix then save" / "Cancel". Write only on
    "Save as-is" or a clean scan
5. Write the file (format below) to `~/.ai-memory/YYYY-MM-DD-{project}-{topic}.md`
    - Same project+topic already exists -> append a new `HH:MM` block to it
    - `{project}`: short repo name; omit for cross-project notes
    - **Use the Write tool, not a Bash heredoc.** The Bash hook rejects any command whose
      text contains the three substituted command names, and a heredoc puts the whole memory
      body into the command text
6. **Update the index** `~/.ai-memory/MEMORY-INDEX.md` -- mandatory; an unindexed memory is
   unfindable
    - Append one line (format below) at the end. Chronological, newest last
    - Existing file updated -> refresh its line if the summary drifted
    - Append with `printf '%s\n' '<line>' >> ~/.ai-memory/MEMORY-INDEX.md`; the same hook
      caveat applies to the line's text

## Memory file format (v2)

```
# M YYYY-MM-DD proj=<project>
HH:MM <topic, <=8 words>
did: <what changed; sha/PR/path where one exists>
why: <the reason the code does not show>
decided: <decision> (<reason>; user: "<their words if the wording matters>")
gotcha: <what cost time and would again; the fix>
pref: <how the user wants to work, learned this session>
next: <open item / waiting on whom>
ref: <sha | path | AGENTS.md#section | handoff path | url>
```

Rules:

- **English, telegraphic.** Drop articles and pronouns; `->`, `=`, `!=`, `~`, `x2` are fine;
  backtick identifiers. No sentences that only join facts, no markdown emphasis or tables
- **One fact per line, <=120 chars.** A tag line may repeat (`gotcha:` x3); omit tags with
  nothing in them
- **Only what the repo cannot tell you.** Git log, `AGENTS.md`, code comments and a handoff
  note already hold their own content: point at them with `ref:` instead of restating
- **Budget: <=40 lines, ~2.5 KB per session.** Over budget means it is a log, not a memory --
  cut the timeline down to the facts a future session would act on
- Keep a user's quote in its original language only when the exact wording carries the
  decision; <=1 line
- Never write raw tool output, command logs, code listings, or the conversation's turn-taking

## Index line format (v2)

```
- [YYYY-MM-DD proj topic](file.md) — kw: k1, k2, k3; gotcha: <one clause>
```

- **<=160 chars.** The index is read whole at the start of a search, so every line is a tax
  on every future session
- `kw:` are retrieval keywords (subsystems, issue numbers, commands), English, 3-6 of them
- `gotcha:` the single most expensive trap in the file, or omit
- The legend for both formats lives in the index header (`<!-- legend ... -->`); do not
  repeat it in memory files. Entries above the `<!-- v2 entries start here -->` marker are
  v1 (Japanese narrative) and stay as they are

## Why this shape

Earlier memories were Japanese narrative paragraphs of 4-11 KB each, and index lines of
500-1000 chars (118 KB of index for 58 entries): a future session paid to read a diary.
English telegraphic lines tokenize at a fraction of the cost, one-fact-per-line lets a reader
stop at the tag it needs, and the caps keep both files from growing past what a session can
afford to load.
