---
name: show-weburl
description: Show URLs from numbered references, repository names, or the context, without opening them. Use when the user asks to see, print, or copy URLs or GitHub repositories mentioned in the conversation, or invokes `/show-weburl`.
---

# show-weburl

## Description

The display-only twin of `open-weburl`: resolves the same references, but **prints the URLs
instead of opening them**. Nothing is executed -- no `wslview`, `xdg-open`, or `open`.

Use it when the user wants a URL to copy, paste somewhere else, or check before opening.

## Usage

- `show-weburl 1, 2, 3` - Shows URLs that were referenced as "1.", "2.", "3." in the previous conversation
- `show-weburl foo.nvim bar.nvim nvim-baz` - Shows GitHub URLs for the specified repository names
- `show-weburl` - Shows the URLs the most recent conversation was about

## Implementation

When this skill is used:

1. Parse the arguments (comma-separated or space-separated)
2. For each argument:
   - If it's a number: Look for recent URLs that were referenced with that number in the conversation
   - If it's a repository name: Convert to GitHub URL format
3. For missing references, report "URL for [X] not found"
4. **Print** the resolved URLs as a markdown list, one per line, each as a bare URL so it can be
   copied or clicked as-is
5. **Do not run any command.** No tool call is needed; the answer is the list itself

## Examples

### Numbered References

```
User: show-weburl 1, 2, 3
Assistant:
- 1: https://github.com/mtth/scratch.vim
- 2: https://github.com/LintaoAmons/scratch.nvim
- 3: https://github.com/0x00-ketsu/scratchpad.nvim
```

### Repository Names

```
User: show-weburl telescope.nvim lazy.nvim
Assistant:
- telescope.nvim: https://github.com/nvim-telescope/telescope.nvim
- lazy.nvim: https://github.com/folke/lazy.nvim
```

### Mixed/Missing

```
User: show-weburl telescope.nvim, nonexistent-software, lazy.nvim
Assistant:
- telescope.nvim: https://github.com/nvim-telescope/telescope.nvim
- lazy.nvim: https://github.com/folke/lazy.nvim
Also Assistant tells 'nonexistent-software is not found' to the user.
```

### No Arguments (Reading Context)

Assuming the last conversation mentioned to `telescope.nvim` and `lazy.nvim`:

```
User: show-weburl
Assistant:
- telescope.nvim: https://github.com/nvim-telescope/telescope.nvim
- lazy.nvim: https://github.com/folke/lazy.nvim
```

When the context is ambiguous, the assistant should ask to the user for clarification.

## URL Resolution Logic

Identical to `open-weburl`; keep the two in step when either changes.

### For numbered references:

1. Search **最直近の会話**で番号付きリストとして言及されたもの
2. リスト形式で提示されたURLやリポジトリ名を優先
3. Issue番号よりも、直前に説明した項目を優先する
4. 時系列順序を正確に判断する

### For repository names:

1. Common repository name patterns:
   - `telescope.nvim` → `https://github.com/nvim-telescope/telescope.nvim`
   - `lazy.nvim` → `https://github.com/folke/lazy.nvim`
   - `nvim-*` → Search common nvim- prefixed repositories
2. Use known repository mappings or search patterns
3. Default to `https://github.com/search?q=[name]` if exact match not found

## Error Handling

- Report missing URLs clearly
- Show only valid URLs
- Handle malformed arguments gracefully
- Provide helpful error messages

## Does Not

- Open anything in a browser -- that is `open-weburl`
- Run any shell command
