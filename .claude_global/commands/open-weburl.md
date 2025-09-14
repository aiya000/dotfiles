# /open-weburl

## Description
Opens URLs using wslview. Can handle numbered references from previous conversation or direct repository names.

## Usage
- `/open-weburl 1, 2, 3` - Opens URLs that were referenced as "1.", "2.", "3." in the previous conversation
- `/open-weburl foo.nvim bar.nvim nvim-baz` - Opens GitHub repositories for the specified names

## Implementation

When this command is used:

1. Parse the arguments (comma-separated or space-separated)
2. For each argument:
   - If it's a number: Look for recent URLs that were referenced with that number in the conversation
   - If it's a repository name: Convert to GitHub URL format
3. For missing references, report "URL for [X] not found"
4. Open available URLs using wslview in background with the format:
   ```bash
   ( wslview URL1 ; wslview URL2 ; wslview URL3 ) &
   ```

## Examples

### Numbered References
```
User: /open-weburl 1, 2, 3
Assistant: Opening URLs from recent list:
- 1: https://github.com/mtth/scratch.vim
- 2: https://github.com/LintaoAmons/scratch.nvim
- 3: https://github.com/0x00-ketsu/scratchpad.nvim
```

### Repository Names
```
User: /open-weburl telescope.nvim lazy.nvim
Assistant: Opening repositories:
- telescope.nvim: https://github.com/nvim-telescope/telescope.nvim
- lazy.nvim: https://github.com/folke/lazy.nvim
```

### Mixed/Missing
```
User: /open-weburl telescope.nvim, nonexistent-software, lazy.nvim
Assistant: Opening only available URLs:
- telescope.nvim: https://github.com/nvim-telescope/telescope.nvim
- lazy.nvim: https://github.com/folke/lazy.nvim
Also Assistant tells 'nonexistent-software is not found' to the user.
```

## URL Resolution Logic

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
- Only open valid URLs
- Handle malformed arguments gracefully
- Provide helpful error messages

## Background Execution
Always execute wslview commands in background using `&` to prevent blocking the conversation.
