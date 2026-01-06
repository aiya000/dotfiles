-- filetype defined by vim-pager and vim-manpager

vim.opt_local.number = false
vim.opt_local.relativenumber = false
vim.opt_local.wrap = true

vim.keymap.set('n', 'Q', vim.cmd.quit, { buffer = true })
