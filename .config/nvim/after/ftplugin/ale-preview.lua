vim.keymap.set('n', 'Q', function()
  vim.api.nvim_buf_delete(0, { force = false })
end, { buffer = true, silent = true })
