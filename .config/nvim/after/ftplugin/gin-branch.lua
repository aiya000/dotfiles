local helper = require('helper')
local list = require('utils.list')

---@param branch_name string
---@return boolean --true if 'y'
local function confirm_to_delete_branch(branch_name)
  local protected_branches = { 'main', 'master', 'develop', 'release' }

  local answer = helper.confirm_to_get_charstr(("Delete '%s' branch? (y/n):"):format(branch_name))
  if answer:lower() ~= 'y' then
    return false
  end

  if list.has(protected_branches, branch_name) then
    local next_answer = helper.confirm_to_get_charstr(
      ("'%s' is a protected branch. Are you sure you want to delete it? (y/n):"):format(branch_name)
    )
    if next_answer:lower() ~= 'y' then
      return false
    end
  end

  return true
end

local function delete_current_line_branch()
  local line = vim.api.nvim_get_current_line()
  local branch_name = line:match('^[%s*]*([^%s]+)')
  if not branch_name then
    vim.notify('Could not extract branch name from current line', vim.log.levels.ERROR)
    return
  end

  local want_to_delete = confirm_to_delete_branch(branch_name)
  if not want_to_delete then
    return
  end

  local result = vim.system({ 'git', 'branch', '-d', branch_name }):wait()
  if result.code ~= 0 then
    vim.notify(("Failed to delete branch '%s': %s"):format(branch_name, result.stderr), vim.log.levels.ERROR)
    return
  end

  vim.notify(("Branch '%s' deleted successfully"):format(branch_name), vim.log.levels.INFO)
  vim.cmd('GinBranch') -- Refresh the buffer
end

vim.keymap.set('n', 'Q', function()
  vim.api.nvim_buf_delete(0, { force = true })
end, { buffer = true, silent = true, desc = 'Close this window' })

helper.keymaps_set('n', { 'dd', 'D' }, delete_current_line_branch, {
  buffer = true,
  desc = 'Delete branch under cursor',
})

vim.keymap.set('n', '<C-r>', '<Cmd>GinBranch<CR>', {
  buffer = true,
  silent = true,
  desc = 'Refresh buffer',
})
