-- filetype defined by vim-pager and vim-manpager

vim.opt_local.number = false
vim.opt_local.relativenumber = false
vim.opt_local.wrap = true

vim.keymap.set('n', 'Q', function()
  vim.cmd('<C-u>q')
end, { buffer = true })
-- Forget the vim-manpager specified nmaps
vim.keymap.set('n', '<buffer>', '<C-n> gt', { buffer = true })
vim.keymap.set('n', '<buffer>', '<C-p> gT', { buffer = true })
