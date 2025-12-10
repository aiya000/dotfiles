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

---@param subcmd? string[] --`:Gin commit --verbose {subcmd (concatenated)}`
local function open_commit_buffer(subcmd)
  local git_commit = vim.fn.extendnew({ 'Gin', 'commit', '--verbose' }, subcmd or {})
  vim.cmd(table.concat(git_commit, ' '))
end

local claude_commit_terminal = nil
local function open_claude_commit_float_window()
  if claude_commit_terminal ~= nil then
    claude_commit_terminal:toggle() -- Show
    return
  end

  claude_commit_terminal = Terminal:new({
    cmd = 'claude /git-commit-auto',
    direction = 'float',
    close_on_exit = false,
    on_exit = function()
      vim.notify('Claude commit process exited.', vim.log.levels.INFO)
      claude_commit_terminal = nil  -- Reset the terminal variable
    end,
  })
  claude_commit_terminal:toggle() -- Show

  vim.schedule(function()
    nvim.keymaps_set('t', nvim.escaping_keys, function()
      claude_commit_terminal:toggle() -- Hide
    end, { buffer = true })
  end)
end

local function delete_this_file()
  vim.cmd('normal "zyy')
  local filepath = vim.fn.trim(vim.fn.getreg('z'))
  if not nvim.confirm('Delete this file?: ' .. filepath) then
    return
  end

  local ok, err = os.remove(filepath)
  if ok then
    vim.notify('Removed file: ' .. filepath, vim.log.levels.INFO)
    vim.cmd('GinStatus') -- Refresh
  else
    vim.notify('Failed to remove file: ' .. err, vim.log.levels.ERROR)
  end
end

vim.keymap.set('n', 'Q', '<Cmd>bdelete!<CR>', { buffer = true, silent = true })
vim.keymap.set('n', 'A', run_add_patch, { buffer = true, silent = true })
vim.keymap.set('n', 'o', ':<C-u>vsp<CR><Plug>(gin-action-edit)', { buffer = true, silent = true })
vim.keymap.set('n', '<C-r>', '<Cmd>GinStatus<CR>', { buffer = true, silent = true }) -- TODO: `gin#util#reload()`が使えそう
vim.keymap.set('n', 'p', '<Plug>(gin-action-diff:smart:vsplit)', { buffer = true, silent = true, nowait = true })
vim.keymap.set('n', 'P', ':<C-u>!git push', { remap = true, buffer = true })
vim.keymap.set('n', 'gP', ':<C-u>!git pull', { remap = true, buffer = true })
vim.keymap.set('n', 'sa', '<Plug>(gin-action-stash)', { buffer = true, silent = true })
vim.keymap.set('n', 'ss', run_stash_push_message, { buffer = true })
vim.keymap.set('n', 'sp', '<Cmd>Gin stash pop<CR>', { buffer = true })
vim.keymap.set('n', 'cc', open_commit_buffer, { buffer = true, silent = true })
vim.keymap.set('n', 'cC', open_claude_commit_float_window, { buffer = true, silent = true })
vim.keymap.set('n', 'B', '<Cmd>GinBranch<CR>', { buffer = true, silent = true })
vim.keymap.set('n', 'C', ':<C-u>Gin switch --create<Space>', { remap = true, buffer = true })
vim.keymap.set('n', 'cf', ':<C-u>GitCommitFixup<Space>', { remap = true, buffer = true })
vim.keymap.set('n', '<:', '<Plug>(gin-action-restore:ours)', { buffer = true })
vim.keymap.set('n', '>:', '<Plug>(gin-action-restore:theirs)', { buffer = true })
vim.keymap.set('n', '==', '<Plug>(gin-action-reset)', { buffer = true })
vim.keymap.set('n', 'D', delete_this_file, { buffer = true })

vim.keymap.set('n', 'O', function()
  vim.cmd('normal "zyy')
  vim.cmd('tabnew')
  vim.cmd('edit ' .. vim.fn.trim(vim.fn.getreg('z')))
end, { buffer = true, silent = true })

vim.keymap.set('n', 'S', function()
  nvim.feedkeys(':<C-u>Cmdpalette<CR>Gin switch<Space>')
end, { buffer = true, silent = true })

vim.keymap.set('n', 'ca', function()
  open_commit_buffer({ '--amend' })
end, { buffer = true, silent = true })
