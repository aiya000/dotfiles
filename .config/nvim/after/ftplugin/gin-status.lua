local Terminal = require('toggleterm.terminal').Terminal
local nvim = require('nvim')

vim.opt_local.cursorline = true

---TODO: 相対パスにする。サブディレクトリでこれやるとエラーになる
---@return string | nil ---nil when failed to parse the line
local function get_current_line_file_path()
  -- `git status --short` puts the XY status on the first 2 columns.
  -- Like ' M path/to/file.txt', 'M  path/to/file.txt', '?? path/to/file.txt', 'R  old.txt -> new.txt', and etc
  local line = vim.fn.getline('.')
  if line:match('^##') ~= nil then -- The branch header line
    return nil
  end

  local filepath = line:match('^..%s(.*)$')
  if filepath == nil or filepath == '' then
    return nil
  end

  filepath = filepath:match('.* %-> (.*)$') or filepath -- A renamed entry shows both of the paths
  return (filepath:gsub('^"(.*)"$', '%1')) -- git quotes a path that has special characters
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

---NOTE: Must be called when the gin-status window is the current window
---@return string | nil
local function resolve_filepath()
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

---The window that the gin-status float window was opened from
---@param float_win integer
---@return integer | nil
local function get_prev_win(float_win)
  local ok, prev_win = pcall(vim.api.nvim_win_get_var, float_win, 'gin_status_prev_win')
  if not ok or not vim.api.nvim_win_is_valid(prev_win) then
    return nil
  end
  return prev_win
end

---Gets back the cursor to the window (usually the gin-status window)
---@param win integer
local function back_to_win_later(win)
  vim.schedule(function()
    if vim.api.nvim_win_is_valid(win) then
      vim.api.nvim_set_current_win(win)
    end
  end)
end

---@param win integer
---@return boolean
local function is_float_win(win)
  return vim.api.nvim_win_get_config(win).relative ~= ''
end

---@param tabpage integer
---@return integer | nil
local function find_non_float_win(tabpage)
  for _, win in ipairs(vim.api.nvim_tabpage_list_wins(tabpage)) do
    if not is_float_win(win) then
      return win
    end
  end
  return nil
end

---Is the tabpage opened by `open_diff()`?
---i.e. Does the tabpage have gin-diff windows only? (Float windows are ignored)
---@param tabpage integer
---@return boolean
local function is_diff_tabpage(tabpage)
  local found_diff_win = false
  for _, win in ipairs(vim.api.nvim_tabpage_list_wins(tabpage)) do
    if not is_float_win(win) then
      local buf = vim.api.nvim_win_get_buf(win)
      if vim.bo[buf].filetype ~= 'gin-diff' then
        return false
      end
      found_diff_win = true
    end
  end
  return found_diff_win
end

---Prefers the current tabpage, then falls back to another tabpage
---@return integer | nil
local function find_diff_tabpage()
  local current = vim.api.nvim_get_current_tabpage()
  if is_diff_tabpage(current) then
    return current
  end
  for _, tabpage in ipairs(vim.api.nvim_list_tabpages()) do
    if is_diff_tabpage(tabpage) then
      return tabpage
    end
  end
  return nil
end

---Does the current line have staged changes? (The X column of `git status --short`)
---NOTE: Must be called when the gin-status window is the current window
---@return boolean
local function current_line_has_staged_change()
  local x = vim.fn.getline('.'):sub(1, 1)
  return x ~= ' ' and x ~= '?' and x ~= '!'
end

---@param filepath string
---@param opener 'vsplit' | 'tabedit'
---@param cached boolean
local function exec_gin_diff(filepath, opener, cached)
  local args = { 'GinDiff', '++opener=' .. opener }
  if cached then
    table.insert(args, '--cached')
  end
  table.insert(args, '--')
  table.insert(args, vim.fn.fnameescape(filepath))
  vim.cmd(table.concat(args, ' '))
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
    local filepath = resolve_filepath()
    if filepath == nil then
      return
    end

    local prev_win = get_prev_win(float_win)
    if prev_win ~= nil then
      vim.api.nvim_set_current_win(prev_win)
    end
    vim.cmd('edit ' .. vim.fn.fnameescape(filepath))
  end)
end

---A float window belongs to its tabpage, so `:tabedit` leaves it behind on the previous tabpage.
---Re-opens the gin-status float window on the current tabpage to keep it shown, then closes the old one.
---@param old_float_win integer
---@param prev_win integer --The window that the new float window opens files into (`V`, `o`)
local function move_float_to_current_tabpage(old_float_win, prev_win)
  local buf = vim.api.nvim_win_get_buf(old_float_win)
  local cursor = vim.api.nvim_win_get_cursor(old_float_win)

  local new_float_win = nvim.open_buffer_in_float_window(buf)
  vim.api.nvim_win_set_var(new_float_win, 'gin_status_prev_win', prev_win)
  vim.wo[new_float_win].cursorline = true -- 'cursorline' is window local, so the new window needs it again
  pcall(vim.api.nvim_win_set_cursor, new_float_win, cursor)

  close_win_later(old_float_win)
end

-- Opens the diff of the file on the current line. Never closes the gin-status window.
--
-- - When no diff tabpage is found: Open the diff in a new tabpage, and move to it with the gin-status window
-- - When a diff tabpage (a tabpage that has gin-diff windows only) is found:
--   Open the diff in it with vsplit, then get the cursor back to the gin-status window (same as `V`)
--     - The current tabpage is preferred, so pressing this in a diff tabpage opens the diff in the same tabpage
local function open_diff()
  local status_win = vim.api.nvim_get_current_win()
  local filepath = resolve_filepath()
  if filepath == nil then
    return
  end
  local cached = current_line_has_staged_change()

  local diff_tabpage = find_diff_tabpage()
  if diff_tabpage == nil then
    local is_float = is_float_win(status_win)
    exec_gin_diff(filepath, 'tabedit', cached) -- Stays in the opened tabpage
    if is_float then
      move_float_to_current_tabpage(status_win, vim.api.nvim_get_current_win())
    end
    return
  end

  vim.api.nvim_set_current_tabpage(diff_tabpage)
  local non_float_win = find_non_float_win(diff_tabpage) -- Cannot split a float window
  if non_float_win ~= nil then
    vim.api.nvim_set_current_win(non_float_win)
  end
  exec_gin_diff(filepath, 'vsplit', cached)
  back_to_win_later(status_win)
end

local function open_file_in_new_tab()
  vim.cmd('normal "zyy')
  vim.cmd('tabnew')
  vim.cmd('edit ' .. vim.fn.trim(vim.fn.getreg('z')))
end

local function switch_branch_via_cmdpalette()
  nvim.feedkeys()
end

---Opens the selected file with vsplit, then gets the cursor back to the gin-status window
---(i.e. Never closes the gin-status window)
--- - When float window: vsplit the window that gin-status was opened from
--- - When not float window: vsplit the gin-status window itself
local function open_file_in_vsplit()
  local status_win = vim.api.nvim_get_current_win()
  local filepath = resolve_filepath()
  if filepath == nil then
    return
  end

  if nvim.is_in_float_window() then
    local prev_win = get_prev_win(status_win) -- Cannot split a float window
    if prev_win == nil then
      vim.notify('Failed to detect the window that gin-status was opened from', vim.log.levels.ERROR)
      return
    end
    vim.api.nvim_set_current_win(prev_win)
  end

  vim.cmd('vsp ' .. vim.fn.fnameescape(filepath))
  back_to_win_later(status_win)
end

-- NOTE: `remap = true` to open cmdpalette
vim.keymap.set('n', 'Q', '<Cmd>bdelete!<CR>', { buffer = true, silent = true })
vim.keymap.set('n', 'A', run_add_patch, { buffer = true, silent = true })
vim.keymap.set('n', '<C-r>', '<Cmd>GinStatus<CR>', { buffer = true, silent = true }) -- TODO: `gin#util#reload()`が使えそう
vim.keymap.set('n', 'P', ':<C-u>AsyncRunNotifyHide git push', { remap = true, buffer = true })
vim.keymap.set('n', 'gP', ':<C-u>AsyncRunNotifyHide git pull', { remap = true, buffer = true })
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
vim.keymap.set('n', 'V', open_file_in_vsplit, { buffer = true, silent = true })
vim.keymap.set('n', 'p', open_diff, { buffer = true, silent = true, nowait = true })
