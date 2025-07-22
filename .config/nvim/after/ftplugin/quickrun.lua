vim.opt_local.wrap = true
vim.opt_local.list = false

vim.keymap.set('n', 'Q', function()
  vim.cmd('<C-u>quit')
end, { buffer = true, silent = true })
