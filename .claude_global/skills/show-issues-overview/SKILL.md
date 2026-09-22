---
name: show-issues-overview
description: Show a table-formatted overview of the current repository's issues, from GitHub Issues, GitLab Issues, or a TODO.md-like file when the project has neither. Use when the user asks what issues/tasks are open, wants to decide what to work on next, or invokes `/show-issues-overview`.
allowed-tools: Bash(git rev-parse:*), Bash(git remote:*), Bash(git config --get:*), Bash(gh issue:*), Bash(gh api:*), Bash(gh auth:*), Bash(gh repo:*), Bash(glab issue:*), Bash(glab api:*), Bash(glab auth:*), Bash(glab repo:*), Bash(fd:*), Bash(rg:*), Read
---

# show-issues-overview

## Description

Print an **overview table** of the issues of the project the current directory belongs to.

This is a read-only briefing. It exists so the user can look at everything that is open in one
glance and pick what to do next -- so the output is one table, short enough to read without
scrolling, not a dump of every field the forge knows.

The issue source depends on what kind of project this is:

| Project hosting | Source of issues |
| --- | --- |
| GitHub | GitHub Issues of the remote repository |
| GitLab | GitLab Issues of the remote repository |
| Neither | `TODO.md` or a similarly named file in the repository |

## Arguments

All optional, and more than one may be given at once (`list all`, `table 10`, `list bug`). Pass
them through to the forge CLI where they map naturally.

**Which issues to show:**

- `<none>` -- open issues, up to 50
- `all` -- include closed issues as well
- `closed` -- closed issues only
- `<number>` -- use that as the maximum number of issues to list, instead of 50
- anything else -- treat it as a label / keyword filter, and say in the output that the list is filtered

**How to print them:**

- `table`, or no format word at all -- **Output format: table**
- `list` -- **Output format: list**

`table` and `list` are *format* words. Consume them as the output shape and never pass them on as a
label or keyword filter, even though the fallback rule above would otherwise swallow them. An issue
genuinely labelled `list` is still reachable with an explicit `--label list`, and that collision is
rare enough to be worth the plain argument.

## Step 1: Decide what kind of project this is

1. Locate the project root with `git rev-parse --show-toplevel`
    - If this fails, the directory is not a git repository -- go straight to **Step 4 (no forge)**
2. List remotes with `git remote -v`
    - Prefer the `origin` remote; if there is no `origin`, use the first remote listed
    - If there is no remote at all, go to **Step 4 (no forge)**
3. Read the host out of the remote URL (both `https://host/owner/repo.git` and `git@host:owner/repo.git` forms)
    - host is `github.com`, or starts with `github.` -- **Step 2 (GitHub)**
    - host is `gitlab.com`, or contains `gitlab` -- **Step 3 (GitLab)**
    - any other host -- it may still be a self-hosted GitHub Enterprise or GitLab instance.
      Run `gh repo view --json name 2>&1` once; if it succeeds, treat it as GitHub. Otherwise run
      `glab repo view 2>&1` once; if it succeeds, treat it as GitLab. If both fail, go to
      **Step 4 (no forge)**

State the detected kind in one short line above the table (e.g. `GitHub: aiya000/dotfiles`), so the
user can tell where the rows came from.

## Step 2: GitHub

Fetch the issues as JSON, so the table can be built without scraping the human-readable output:

```
gh issue list --state open --limit 50 --json number,title,labels,assignees,updatedAt,url
```

- `all` -- `--state all`
- `closed` -- `--state closed`
- a number argument -- pass it as `--limit`
- a label / keyword argument -- add `--label <arg>`, and if that returns nothing, retry with
  `--search <arg>` before concluding there is nothing to show

If `gh` fails, see **Authentication failures** below.

### Step 2b: the pull requests linked to each issue

`gh issue list --json` has no field for linked pull requests, so fetch them in **one** extra
GraphQL call, covering every issue number the table is about to show -- never one call per issue:

```
gh api graphql -f query='
fragment F on Issue {
  number
  closedByPullRequestsReferences(first:10, includeClosedPrs:true){
    nodes{ number state isDraft }
  }
}
query($owner:String!,$name:String!){
  repository(owner:$owner,name:$name){
    i103: issue(number:103){ ...F }
    i97:  issue(number:97){ ...F }
  }
}' -F owner=<owner> -F name=<repo>
```

- Build one alias per issue -- `i<number>` -- from the numbers `gh issue list` returned, and join
  the result back onto the rows by `number`
- `closedByPullRequestsReferences` is the set of **development-linked** PRs: the ones opened with
  `Closes #N` in the body, or attached through the issue's Development sidebar. A PR that merely
  mentions the issue in a comment is deliberately *not* in here -- that is a cross-reference, not
  "the PR for this issue"
- `state` is `OPEN` / `CLOSED` / `MERGED`, and `isDraft` is separate from it
- If this call fails for any reason, **still print the table**, with `-` in the `PR` column and one
  short line saying linked PRs could not be fetched. A missing `PR` column must never cost the user
  the issue list itself

## Step 3: GitLab

The same shape as GitHub, using `glab`:

```
glab issue list --output json --per-page 50
```

- `all` -- `--all`
- `closed` -- `--closed`
- a number argument -- pass it as `--per-page`
- a label argument -- add `--label <arg>`

`glab` is **not necessarily installed**. If `glab` is missing, do not improvise with `curl` and a
private token -- say that the project is on GitLab but `glab` is not installed, suggest installing it
(`brew install glab`), and stop there.

If `glab` is installed but fails, see **Authentication failures** below.

GitLab's equivalent of a linked PR is a **related merge request**, and `glab issue list` does not
report those either. Fetch them per issue with the REST API:

```
glab api "projects/:id/issues/<iid>/related_merge_requests"
```

- This is one call per row, so only do it when the table is small -- roughly 20 rows or fewer.
  Above that, leave the `PR` column as `-` and say once that there were too many rows to look up
- Take `iid` and `state` (`opened` / `merged` / `closed`) out of each result
- As with GitHub, a failure here costs the `PR` column, never the table

## Step 4: No forge -- locate a TODO file

Search inside the project root only, and keep the search cheap.

1. Look for the obvious names at the top level first:

    ```
    fd --max-depth 1 --ignore-case '^todo\.(md|markdown|txt|org)$'
    ```

2. If nothing matched, widen the net once -- still bounded:

    ```
    fd --max-depth 3 --ignore-case '(todo|task|backlog|roadmap)'
    ```

3. If step 2 returned several candidates, pick the one that looks like a task list (a file with
   Markdown checkboxes), and say which file the table was built from
4. **If nothing turns up, or the search is turning into a hunt -- stop.** Report that this project
   has no GitHub/GitLab remote and no TODO-like file, so there is no issue list to show, and name
   the places that were checked. Do not read the whole repository looking for tasks, and do not
   invent issues out of `TODO:` code comments unless the user asks for that

Once a file is chosen, read it and **extract its items into a numbered list** -- the file has no
issue numbers of its own, so this skill assigns them:

- A Markdown checkbox line (`- [ ]` / `- [x]`) is one row
- The nearest heading above it becomes its `Section`
- `- [ ]` is open, `- [x]` is done; with no argument, show only the open ones, and mention how many
  done items were hidden
- If the file has no checkboxes, treat each top-level list item as one open row
- **If the file has no list at all** (only prose, or headings with paragraphs under them), take each
  paragraph or line that reads like a task as one row, and say that the rows were read out of prose
- Number the rows `1`, `2`, `3` ... in the order the items appear in the file, counting only the
  rows actually shown. These numbers are **provisional**: they are a handle for saying "let's do 3"
  in this session, not an identifier the file carries. Add one line under the table saying so
- The one exception: if the file carries its own identifiers (`#12`, `TODO-3`, `[A-1]`), put those
  in `#` verbatim instead of counting, and then the numbers are not provisional

## Output format

Two shapes, chosen by the format argument: a **table** (the default) and a **list**. They carry the
same fields in the same order -- only the layout differs, and **Rules for the values** below governs
the contents of both.

Either shape: open issues first, most recently updated first.

### Output format: table

One Markdown table.

**The first column is always `#`, and it is always the number the user can refer a task by.**

For GitHub / GitLab -- `#` is the **issue number the forge assigned** (`42`, `103`), never a row
counter. Keep the forge's numbers as they are, gaps and all, and never renumber them:

**The last two columns are always `PR` and then `URL`** -- the pull requests linked to that issue
(see **Step 2b** / **Step 3**), so the user can see at a glance which rows are already being worked
on, and then the issue's own URL so a row can be opened straight from the table:

| # | Title | Labels | Assignee | Updated | PR | URL |
| --- | --- | --- | --- | --- | --- | --- |
| 103 | Neovim: `<C-f>`, `<C-b>`, `<C-u>`, `<C-d>`などをするまで、markdownコードハイライトが別のハイライトになっている | invalid | aiya000, Copilot | 2025-12-23 | #105, #104 (closed) | https://github.com/aiya000/dotfiles/issues/103 |
| 97 | Neovim: normal-modeのdsBキーがうまく動いてない | bug | aiya000, Copilot | 2025-12-17 | #98 | https://github.com/aiya000/dotfiles/issues/97 |
| 92 | Neovim: utils.functions.readonly と readonly_value のテストを追加する | - | - | 2025-12-10 | - | https://github.com/aiya000/dotfiles/issues/92 |

For a TODO file -- `#` is the sequential number this skill assigned while extracting the list
(see **Step 4**), so it is provisional unless the file carried its own identifiers:

| # | Task | Section | Status |
| --- | --- | --- | --- |
| 1 | Migrate ftdetect files | Neovim | open |
| 2 | Drop the vital.vim shim | Neovim | open |

A TODO file has no pull requests and no per-item URL, so its table keeps the four columns above
and gains neither a `PR` nor a `URL` column.

### Output format: list

Given `list`, print one block per issue instead of a table, so that no single line has to be wide
enough to hold every field at once. This is the shape to reach for when the titles are long -- on a
wide screen a table row wraps into something hard to read, and a block never does.

- `- #<number>` is a block's own line; nothing else goes on it
- Under it, one nested bullet per field, **in the same order as the table's columns**: the title
  first and bare, then `Labels:`, `Assignee:`, `Updated:`, `PR:`, and the URL last and bare
- The title and the URL take no `Title:` / `URL:` prefix -- neither can be mistaken for anything
  else, and a prefix would only push the longest values further right
- **An empty field is left out of the block entirely.** A table has to write `-` to keep its columns
  lined up; a list has no columns to keep, so an issue with no labels simply has no `Labels:` line.
  Never write `Labels: -`
- One blank line between blocks, and none after the last one

```
- #103
  - Neovim: `<C-f>`, `<C-b>`, `<C-u>`, `<C-d>`などをするまで、markdownコードハイライトが別のハイライトになっている
  - Labels: invalid
  - Assignee: aiya000, Copilot
  - Updated: 2025-12-23
  - PR: #105, #104 (closed)
  - https://github.com/aiya000/dotfiles/issues/103

- #92
  - Neovim: utils.functions.readonly と readonly_value のテストを追加する
  - Labels: enhancement
  - Assignee: aiya000
  - Updated: 2025-10-23
  - https://github.com/aiya000/dotfiles/issues/92
```

A TODO file in this shape keeps its own fields -- the task, then `Section:` and `Status:`, and no
URL line.

### Rules for the values (both shapes)

- **`Title` is written in Japanese.** When the forge's title is in English, translate it, and put
  only the translation in the cell -- the original English is not repeated
    - It is a translation, not a rewrite: do not summarise it, do not make it friendlier, and do
      not add anything the title did not say
    - Leave identifiers alone -- file paths, option names, function names, plugin names, commands,
      key sequences (`<C-f>`), and a prefix such as `Neovim: ` all stay exactly as written
    - A title that is already Japanese is copied through untouched, and so is a mixed title's
      Japanese part
    - When at least one row was translated, add one short line under the table saying the English
      titles are shown translated -- otherwise the user is surprised when the issue page says
      something else
- **Never truncate `Title`.** The user writes long, detailed titles on purpose -- the detail is
  often the whole point of the row, and a `...` throws away exactly the part that would have told
  them what the issue is. Print the title in full, however long it is, and let the terminal wrap
  the cell. This holds for a translated title too
    - The only thing ever removed from a title is a trailing newline
    - If the table then looks wide, that is fine -- a wide table is cheaper than a lost sentence
- Join multiple `Labels` / `Assignee` values with `, `, and when there are more than two, keep the
  first two and append `+N`
    - Empty in the table: `-`, never a blank cell. Empty in the list: no line at all
- `Updated` is a plain date (`YYYY-MM-DD`), not a timestamp and not "3 days ago"
- `PR` holds the linked pull request numbers with a `#`, newest first, and marks anything that is
  not a plain open PR: `#98`, `#105 (draft)`, `#104 (merged)`, `#101 (closed)`. Put open and draft
  PRs before merged and closed ones, since those are the ones still in flight
    - No linked PR is `-` in the table, and no `PR:` line at all in the list
    - With more than two, keep the first two and append `+N`, the same as `Labels` and `Assignee`
- `URL` is the issue's own web address, written out in full and bare -- no Markdown link, no
  shortening, no `...`, so the terminal can make it clickable
    - GitHub: the `url` field `gh issue list --json` already returns. GitLab: `web_url`
    - Never rebuild the URL by hand out of the owner, repo and number -- use the field the forge
      gave, or the row would lie on a renamed or transferred repository
    - A TODO-file row has no URL, so that table has no `URL` column and that block has no URL line
      -- do not fill either with `-`
- After the table, or after the last block, add one short line with the total count, plus the
  filter when one was applied

When there are no issues at all, say so in one line instead of printing an empty table or an empty
list.

## Authentication failures

`gh auth status` / `glab auth status` may report `Failed to log in ... (keyring)` in environments
where the system keyring is unavailable, even though the user is authenticated. In that case the
listing command fails too.

When the listing fails for an auth reason:

1. Say plainly that the issues could not be fetched, and quote the error line
2. Give the user the command to run themselves, prefixed with `!` so it runs in this session:

    ```
    ! gh issue list --state open --limit 50
    ```

3. Do not fall back to guessing the issue list from commit messages or from a TODO file -- a GitHub
   project's issues live on GitHub

## Does Not

1. Create, edit, close, or comment on any issue -- this skill only reads
2. Open a browser -- that is `open-weburl`
3. List issues of a repository other than the current project's remote, unless the user names one
4. Keep searching for a task file after **Step 4** has already come up empty
