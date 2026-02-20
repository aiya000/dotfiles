local nvim = require('nvim')
local list = require('utils.list')

---@param branch_name string
---@param cont fun(): nil
local function confirm_to_delete_branch(branch_name, cont)
  local protected_branches = { 'main', 'master', 'develop', 'release' }
  nvim.confirm(("Delete '%s' branch?"):format(branch_name), function()
    if not list.has(protected_branches, branch_name) then
      cont()
      return
    end

    nvim.confirm(
      ("'%s' is a protected branch. Are you sure?"):format(branch_name),
      cont
    )
  end)
end

local function confirm_to_force_delete_not_merged_branch()
  -- TODO: YES<Enter>と入力することのみで、強制削除を実行できるようにする
  error('TODO: Not Implemented Yet (confirm_to_force_delete_not_merged_branch)')
end

-- TODO: 動いてなさそう。直す
---@param stderr string | nil
---@return boolean
local function is_error_due_to_no_merge(stderr)
  if stderr == nil then
    return false
  end

  local first_line = vim.split(stderr, '\r')[1]
  if first_line == nil then
    return false
  end

  return first_line:match('^error: the branch') and first_line:match('is not fully merged\\.$')
end

local function delete_current_line_branch()
  local line = vim.api.nvim_get_current_line()
  local branch_name = line:match('^[%s*]*([^%s]+)')
  if not branch_name then
    vim.notify('Could not extract branch name from current line', vim.log.levels.ERROR)
    return
  end

  confirm_to_delete_branch(branch_name, function()
    local result = vim.system({ 'git', 'branch', '-d', branch_name }):wait()
    if result.code ~= 0 and is_error_due_to_no_merge(result.stderr) then
      confirm_to_force_delete_not_merged_branch()
      return
    end
    if result.code ~= 0 then
      vim.notify(("Failed to delete branch '%s': %s"):format(branch_name, result.stderr), vim.log.levels.ERROR)
      return
    end

    vim.notify(("Branch '%s' deleted successfully"):format(branch_name), vim.log.levels.INFO)
    vim.cmd('GinBranch') -- Refresh the buffer
  end)
end

vim.keymap.set('n', 'Q', function()
  vim.api.nvim_buf_delete(0, { force = true })
end, { buffer = true, silent = true, desc = 'Close this window' })

nvim.keymaps_set('n', { 'dd', 'D' }, delete_current_line_branch, {
  buffer = true,
  desc = 'Delete branch under cursor',
})

vim.keymap.set('n', '<C-r>', '<Cmd>GinBranch<CR>', {
  buffer = true,
  silent = true,
  desc = 'Refresh buffer',
})

vim.keymap.set('n', 'A', '<Cmd>GinBranch --all<CR>', {
  buffer = true,
  silent = true,
  desc = 'Show all branches',
})

vim.keymap.set('n', 's', '<Cmd>GinStatus<CR>', {
  buffer = true,
  silent = true,
  desc = 'Open GinStatus',
})
