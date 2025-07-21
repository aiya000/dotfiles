# CLAUDE.md

This file provides guidance on how Claude Code (claude.ai/code) handles the code in this repository.

## Conversations

Use Japanese for conversations with developer.

## Writing CLAUDE.md

Use English in this file.

Add a blank line between the title and the first list item if adding new sections with list items.
Also do not add trailing dot to list items like `- An item.`
For example:

```markdown
## Section Title

- First item
- Second item
```

## Vim to Neovim Migration Guidelines

When migrating Vim configuration to Neovim:

### Complete Migration Philosophy

- **Migrate ALL files**: Every file in the Vim configuration directory structure must be migrated faithfully to Neovim
- **No function removal**: Never comment out or remove functionality due to missing dependencies or different concepts between Vim and Neovim
- **Implement missing concepts**: When Neovim lacks a Vim concept (like autoload), implement a better custom solution rather than removing functionality
- **Preserve all keymappings**: All keymappings must be preserved and functional in the migrated configuration

### Directory Structure Mapping

```
Vim (.vim/)                     → Neovim (.config/nvim/)
├── .vimrc                      → init.lua
├── autoload/**/*.vim           → lua/**/*.lua (custom implementation)
├── plugin/**/*.vim             → plugin/**/*.lua
├── ftdetect/**/*.vim           → ftdetect/**/.lua
├── after/ftplugin/**/*.vim     → after/ftplugin/**/*.lua
├── syntax/**/*.vim             → syntax/**/*.lua
└── indent/**/*.vim             → indent/**/*.lua
```

### Conversion Patterns

- `set option=value` → `vim.opt.option = value`
- `nnoremap key action` → `vim.keymap.set('n', 'key', 'action')`
- `autocmd Event pattern action` → `vim.api.nvim_create_autocmd('Event', {pattern = 'pattern', callback = function() ... end})`
- `command! Name action` → `vim.api.nvim_create_user_command('Name', function() ... end, opts)`
- `" comment` → `-- comment`

### Vital.vim Replacement

Since vital.vim doesn't exist for Neovim, create custom Lua modules:

- `vital#vimrc#import('Data.List')` → require('utils.list')  
- `vital#vimrc#import('Vim.Message')` → require('utils.message')
- Implement expected functionality in separate modules under `lua/utils/`

### Missing Autoload Functions

When encountering missing autoload functions like `vimrc#hide_or_quit()`:

- Create equivalent Lua functions in appropriate modules under `lua/`
- Maintain the same interface and behavior as the original Vim function
- Update all references to use the new Lua implementation

### Testing Requirements

After migration:

- Ensure Neovim starts without errors
- Verify all keymappings work as expected  
- Test filetype detection and plugins
- Confirm all custom commands are available

## Neovim Migration Lessons Learned

### Successful Migration Strategy

When completing a full Vim to Neovim migration (1792 lines + 153 additional files):

#### 1. Sequential Approach Works Best

- Start with core `init.lua` conversion
- Handle vital.vim dependencies early with custom utils
- Convert ftdetect and after/ftplugin systematically
- Test each component independently before moving to next

#### 2. Fast Event Context Issues

Common error: `E5560: nvim_exec2 must not be called in a fast event context`

Solution:
```lua
-- Instead of direct vim.cmd() in callbacks
local function set_git_root(git_root)
  vim.schedule(function()
    vim.cmd('echomsg "git root: ' .. git_root .. '"')
    vim.g.vimrc.git_root = git_root
  end)
end
```

#### 3. Plugin Manager Migration Strategy

- Keep existing dein.vim setup initially for stability
- Focus on Lua conversion first, plugin manager migration second
- Use `pcall()` for graceful degradation when plugins aren't available

#### 4. Effective Testing Commands

```bash
# Basic startup test
timeout 30s nvim --headless -c "lua print('test')" -c "qa"

# Filetype detection test  
nvim --headless file.ext -c "lua print('Filetype: ' .. vim.bo.filetype)" -c "qa"

# Plugin functionality test
nvim --headless file.py -c "echo 'Shiftwidth: ' . &shiftwidth" -c "qa"
```

#### 5. Common Lua Conversion Patterns

```lua
-- Autoload functions
-- Vim: call vimrc#function()
-- Lua: require('vimrc').function() or _G.vimrc.function()

-- Buffer/window local options
-- Vim: setlocal option=value
-- Lua: vim.opt_local.option = value or vim.bo.option = value

-- Global variables with nested structure
-- Vim: let g:config.nested.value = 'test'
-- Lua: vim.g.config = {nested = {value = 'test'}}
```

#### 6. File Organization Best Practices

```
lua/
├── utils/           # Utility modules (list, message, etc.)
├── plugins/         # Plugin configurations  
├── keymaps.lua      # All keymappings
├── autocmds.lua     # All autocommands
└── vimrc.lua        # Core functions (replaces autoload/vimrc.vim)
```

#### 7. Startup Performance Considerations

- Use `vim.defer_fn()` for non-critical initialization
- Avoid heavy operations in main init.lua
- Test startup time: `nvim --startuptime startup.log`

#### 8. Backward Compatibility Maintenance

When converting autoload functions, maintain global access:
```lua
-- At end of module
_G.vimrc = M  -- Makes functions available as vimrc#function_name()
return M
```

### Migration Statistics

Successfully converted:
- 1 main config file (.vimrc → init.lua)
- 44 ftdetect files 
- 109 after/ftplugin files
- 5 plugin files
- 20 syntax files  
- 1 indent file
- 3 autoload files → lua modules

Total: 183 files migrated from Vimscript to Lua

## Neovim Configuration Notes

### Directory Creation Issues

- NEVER create a directory named `v:null` in `.config/nvim/`
- This directory is unnecessary and should be avoided during Neovim configuration modifications
- If created accidentally, remove it with `rmdir`
