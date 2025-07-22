vim.opt_local.number = false
vim.opt_local.relativenumber = false
vim.opt_local.list = false
vim.opt_local.wrap = true
vim.opt_local.expandtab = false

vim.keymap.set('n', 'Q', function()
  vim.cmd('<C-u>bdelete')
end, { buffer = true, silent = true })
