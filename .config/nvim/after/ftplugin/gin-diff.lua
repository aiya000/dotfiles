vim.opt_local.list = false
vim.opt_local.tabstop = 8

vim.keymap.set('n', 'Q', function()
  vim.api.nvim_buf_delete(0, { force = true })
end, { buffer = true, silent = true })

