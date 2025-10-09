vim.keymap.set('n', 'Q', function()
  vim.api.nvim_buf_delete(0, { force = true })
end, { buffer = true, silent = true })

vim.keymap.set('n', 'yy', function()
  vim.cmd('normal! yy')
end, { buffer = true })

vim.keymap.set('v', 'y', function()
  vim.cmd('normal! y')
end, { buffer = true })
