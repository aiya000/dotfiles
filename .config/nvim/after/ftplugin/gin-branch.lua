local function confirm_to_delete_branch(branch_name)
  vim.api.nvim_echo({
    {
      ("Delete '%s' branch? (y/n):"):format(branch_name),
      'Question',
    }
  }, false, {})
  vim.cmd('redraw')
  return vim.fn.getcharstr()
end

local function delete_current_line_branch()
  local line = vim.api.nvim_get_current_line()
  local branch_name = line:match('^[%s*]*([^%s]+)')
  if not branch_name then
    vim.notify('Could not extract branch name from current line', vim.log.levels.ERROR)
    return
  end

  local answer = confirm_to_delete_branch(branch_name)
  if answer:lower() ~= 'y' then
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

vim.keymap.set('n', 'dd', delete_current_line_branch, {
  buffer = true,
  desc = 'Delete branch under cursor',
})

vim.keymap.set('n', 'D', delete_current_line_branch, {
  buffer = true,
  desc = 'Delete branch under cursor',
})

vim.keymap.set('n', '<C-r>', '<Cmd>GinBranch<CR>', {
  buffer = true,
  silent = true,
  desc = 'Refresh buffer',
})
