vim.opt_local.wrap = true
vim.opt_local.list = false

vim.keymap.set('n', 'Q', function()
  vim.api.nvim_win_close(0, false)
end, { buffer = true, silent = true })
