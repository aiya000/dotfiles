local Terminal = require('toggleterm.terminal').Terminal
local nvim = require('nvim')

vim.opt_local.cursorline = true

---TODO: 相対パスにする。サブディレクトリでこれやるとエラーになる
---@return string | nil ---nil when failed to parse the line
local function get_current_line_file_path()
  local line = vim.fn.getline('.') -- Like ' M path/to/file.txt', 'M  path/to/file.txt', '?? path/to/file.txt', and etc
  local filepath = line:match('^%s+%S+%s+(.*)$')
  if filepath == nil or filepath == '' then
    return nil
  end
  return filepath
end

---Runs `git stash push --message "{message}" -- "{current_line_file_path}"`
local function run_stash_push_message()
  if InitLua.git_root == nil then
    vim.notify('git root directory is never loaded. wait.', vim.log.levels.ERROR)
    return
  end

  local filename = get_current_line_file_path()
  if filename == nil then
    vim.notify('Failed to parse the current line for file path', vim.log.levels.ERROR)
    return
  end

  local filepath = InitLua.git_root .. '/' .. filename

  local message = vim.fn.input('Stash message: ')
  if message == '' then
    vim.notify('Stash message cannot be empty', vim.log.levels.ERROR)
    return
  end

  local result = vim.system({ 'git', 'stash', 'push', '--message', message, '--', filepath }):wait()
  if result.code ~= 0 then
    vim.notify('Stash failed: ' .. (result.stderr or 'Unknown error'), vim.log.levels.ERROR)
    return
  end

  vim.notify(result.stdout or 'Stash created successfully', vim.log.levels.INFO)
  vim.cmd('GinStatus') -- Refresh
end

local function run_add_patch()
  if InitLua.git_root == nil then
    vim.notify('git root directory is never loaded. wait.', vim.log.levels.ERROR)
    return
  end
  local filename = get_current_line_file_path()
  local filepath = InitLua.git_root .. '/' .. filename

  vim.cmd('vertical new')
  vim.fn.jobstart({ 'git', 'add', '--patch', filepath }, {
    term = true,
    on_exit = function()
      vim.cmd('close')
    end,
  })
  vim.fn.feedkeys('i', 'n') -- Enter insert mode
end

local function close_win_later(win_id)
  vim.schedule(function()
    if vim.api.nvim_win_is_valid(win_id) then
      vim.api.nvim_win_close(win_id, true)
    end
  end)
end

---@param action fun(): nil
local function run_closing_float(action)
  local win = nvim.is_in_float_window() and vim.api.nvim_get_current_win() or nil
  action()
  if win ~= nil then
    close_win_later(win)
  end
end

---@param subcmd? string[] --`:Gin commit --verbose {subcmd (concatenated)}`
local function open_commit_buffer(subcmd)
  local git_commit = vim.fn.extendnew({ 'Gin', 'commit', '--verbose' }, subcmd or {})
  if nvim.is_in_float_window() then
    local win = vim.api.nvim_get_current_win()
    vim.cmd('tabnew')
    close_win_later(win)
  end
  vim.cmd(table.concat(git_commit, ' '))
end

local function delete_this_file()
  vim.cmd('normal "zyy')
  local filepath = vim.fn.trim(vim.fn.getreg('z'))

  nvim.confirm('Delete this file?: ' .. filepath, function()
    local rm_dust = vim.fn.expand('~/.dotfiles/bash-toys/bin/rm-dust')
    local result = vim.system({ rm_dust, filepath }):wait()
    if result.code == 0 then
      vim.notify('Removed file (via rm-dust): ' .. filepath, vim.log.levels.INFO)
      vim.cmd('GinStatus') -- Refresh
    else
      vim.notify('Failed to remove file: ' .. (result.stderr or 'Unknown error'), vim.log.levels.ERROR)
    end
  end)
end

---@param float_win integer
---@return string | nil
local function resolve_filepath(float_win)
  local filepath = get_current_line_file_path()
  if filepath == nil then
    vim.notify('Failed to parse the current line for file path', vim.log.levels.ERROR)
    return nil
  end
  if InitLua.git_root ~= nil then
    filepath = InitLua.git_root .. '/' .. filepath
  end
  return filepath
end

-- - When not float window: Open the selected file in new vertical split window
-- - When float window: Open the selected file in prev window (edit)
local function open_file_in_window()
  if not nvim.is_in_float_window() then
    vim.cmd('vsp')
    nvim.run_with_virtual_keymaps('<Plug>(gin-action-edit)')
    return
  end

  run_closing_float(function()
    local float_win = vim.api.nvim_get_current_win()
    local filepath = resolve_filepath(float_win)
    if filepath == nil then
      return
    end

    local ok, prev_win = pcall(vim.api.nvim_win_get_var, float_win, 'gin_status_prev_win')
    if ok and vim.api.nvim_win_is_valid(prev_win) then
      vim.api.nvim_set_current_win(prev_win)
    end
    vim.cmd('edit ' .. vim.fn.fnameescape(filepath))
  end)
end

-- - When not float window: Open diff in vsplit
-- - When float window: Open diff in diff tab (reuse existing diff tab if any)
local function open_diff()
  if not nvim.is_in_float_window() then
    nvim.run_with_virtual_keymaps('<Plug>(gin-action-diff:smart:vsplit)')
    return
  end

  local float_win = vim.api.nvim_get_current_win()
  local ok, diff_tab = pcall(vim.api.nvim_win_get_var, float_win, 'gin_diff_tabpage')

  if ok and vim.api.nvim_tabpage_is_valid(diff_tab) then
    local filepath = resolve_filepath(float_win)
    if filepath == nil then
      return
    end
    local line = vim.fn.getline('.')
    local status = line:match('^%s*(%S+)')
    local has_staged = status ~= nil and status:sub(1, 1) ~= ' ' and status:sub(1, 1) ~= '?'
    vim.api.nvim_set_current_tabpage(diff_tab)
    vim.cmd('vsplit')
    if has_staged then
      vim.cmd('GinDiff ++cached -- ' .. vim.fn.fnameescape(filepath))
    else
      vim.cmd('GinDiff -- ' .. vim.fn.fnameescape(filepath))
    end
  else
    nvim.run_with_virtual_keymaps('<Plug>(gin-action-diff:smart:tabedit)')
    local new_tab = vim.api.nvim_get_current_tabpage()
    vim.api.nvim_win_set_var(float_win, 'gin_diff_tabpage', new_tab)
  end
end

local function open_file_in_new_tab()
  vim.cmd('normal "zyy')
  vim.cmd('tabnew')
  vim.cmd('edit ' .. vim.fn.trim(vim.fn.getreg('z')))
end

local function switch_branch_via_cmdpalette()
  nvim.feedkeys()
end

---When float window only: Open the selected file in prev window with vsplit (float stays open)
local function open_file_in_prev_win_vsplit()
  if not nvim.is_in_float_window() then
    return
  end

  local float_win = vim.api.nvim_get_current_win()
  local filepath = resolve_filepath(float_win)
  if filepath == nil then
    return
  end

  local ok, prev_win = pcall(vim.api.nvim_win_get_var, float_win, 'gin_status_prev_win')
  if not (ok and vim.api.nvim_win_is_valid(prev_win)) then
    return
  end
  vim.api.nvim_set_current_win(prev_win)
  vim.cmd('vsp ' .. vim.fn.fnameescape(filepath))
end

-- NOTE: `remap = true` to open cmdpalette
vim.keymap.set('n', 'Q', '<Cmd>bdelete!<CR>', { buffer = true, silent = true })
vim.keymap.set('n', 'A', run_add_patch, { buffer = true, silent = true })
vim.keymap.set('n', '<C-r>', '<Cmd>GinStatus<CR>', { buffer = true, silent = true }) -- TODO: `gin#util#reload()`が使えそう
vim.keymap.set('n', 'P', ':<C-u>AsyncRun git push', { remap = true, buffer = true })
vim.keymap.set('n', 'gP', ':<C-u>AsyncRun git pull', { remap = true, buffer = true })
vim.keymap.set('n', 'sa', '<Plug>(gin-action-stash)', { buffer = true, silent = true })
vim.keymap.set('n', 'ss', run_stash_push_message, { buffer = true })
vim.keymap.set('n', 'sp', '<Cmd>Gin stash pop<CR>', { buffer = true })
vim.keymap.set('n', 'cc', open_commit_buffer, { buffer = true, silent = true })
vim.keymap.set('n', 'cC', '<Cmd>ClaudeCodeFocus<CR>/git-commit', { buffer = true, silent = true })
vim.keymap.set('n', 'ca', function() open_commit_buffer({ '--amend' }) end, { buffer = true, silent = true })
vim.keymap.set('n', 'B', '<Cmd>GinBranch<CR>', { buffer = true, silent = true })
vim.keymap.set('n', 'C', ':<C-u>Gin switch --create<Space>', { remap = true, buffer = true })
vim.keymap.set('n', 'cf', ':<C-u>GitCommitFixup<Space>', { remap = true, buffer = true })
vim.keymap.set('n', 'S', ':<C-u>Gin switch<Space>', { remap = true, buffer = true, silent = true })
vim.keymap.set({ 'n', 'v' }, '<:', '<Plug>(gin-action-restore:ours)', { buffer = true })
vim.keymap.set({ 'n', 'v' }, '>:', '<Plug>(gin-action-restore:theirs)', { buffer = true })
vim.keymap.set('n', '==', '<Plug>(gin-action-reset)', { buffer = true })
vim.keymap.set('n', 'D', delete_this_file, { buffer = true })
vim.keymap.set('n', 'O', open_file_in_new_tab, { buffer = true, silent = true })
vim.keymap.set('n', '<C-g>', ':<C-u>AsyncRun git<Space>', { nowait = true, remap = true, buffer = true, silent = true })

-- For float windows - Keymaps that behave differently between float and non-float windows
vim.keymap.set('n', 'o', open_file_in_window, { buffer = true, silent = true })
vim.keymap.set('n', 'V', open_file_in_prev_win_vsplit, { buffer = true, silent = true })
vim.keymap.set('n', 'p', open_diff, { buffer = true, silent = true, nowait = true })
