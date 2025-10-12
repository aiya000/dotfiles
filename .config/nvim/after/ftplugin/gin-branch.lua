vim.keymap.set('n', 'Q', function()
  vim.api.nvim_buf_delete(0, { force = true })
end, { buffer = true, silent = true })

vim.keymap.set('n', 'D', function()
  -- Get current line and extract branch name
  local line = vim.api.nvim_get_current_line()
  -- Remove leading spaces and asterisk
  local branch_name = line:match('^[%s*]*([^%s]+)')

  if not branch_name then
    vim.notify('Could not extract branch name from current line', vim.log.levels.ERROR)
    return
  end

  -- Ask for confirmation
  local answer = vim.fn.input(string.format('Delete branch "%s"? (y/n): ', branch_name))

  -- Clear the input prompt
  vim.cmd('redraw')

  if answer:lower() == 'y' then
    -- Execute git branch -d
    local result = vim.fn.system(string.format('git branch -d %s', vim.fn.shellescape(branch_name)))

    if vim.v.shell_error == 0 then
      vim.notify(string.format('Branch "%s" deleted successfully', branch_name), vim.log.levels.INFO)
      -- Refresh the buffer
      vim.cmd('edit')
    else
      vim.notify(string.format('Failed to delete branch "%s": %s', branch_name, result), vim.log.levels.ERROR)
    end
  end
end, { buffer = true, silent = true, desc = 'Delete branch under cursor' })
