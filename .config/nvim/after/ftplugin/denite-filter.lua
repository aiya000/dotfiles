
vim.opt_local.list = false
vim.opt_local.number = false
vim.opt_local.relativenumber = false

vim.keymap.set('i', "<buffer>", "<C-p> <Esc><C-w>pk<C-w>pa", { buffer = true })
vim.keymap.set('i', "<buffer>", "<C-n> <Esc><C-w>pj<C-w>pa", { buffer = true })
vim.keymap.set('i', "<buffer><silent>", "<C-l> <Esc>:q<CR><C-w>p", { buffer = true, silent = true })
vim.keymap.set('i', "<buffer><silent>", "<Esc> <Esc>:q<CR><C-w>p", { buffer = true, silent = true })
vim.keymap.set('i', "<buffer><silent>", "<C-[> <Esc>:q<CR><C-w>p", { buffer = true, silent = true })
