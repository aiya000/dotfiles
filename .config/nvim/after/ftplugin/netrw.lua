
vim.opt_local.list = false
vim.opt_local.wrapscan = true

vim.cmd("nmap             <buffer> H              G-")
vim.cmd("nmap             <buffer> gg             gg<CR>")
vim.cmd("nmap             <buffer> o              x")
vim.keymap.set('n', "L", "<NOP>", { buffer = true })
vim.keymap.set('n', "Q", function() vim.cmd("<C-u>quit") end, { buffer = true, silent = true })
vim.keymap.set('n', "~", function() vim.cmd("<C-u>execute 'Explore' expand('~')") end, { buffer = true, silent = true })
vim.keymap.set('n', "e", function() vim.cmd("<C-u>quit") end, { buffer = true, silent = true })
vim.keymap.set('n', "V", function() vim.cmd("<C-u>vertical split") end, { buffer = true, silent = true })
vim.keymap.set('n', "v", "<NOP>", { buffer = true })
vim.keymap.set('n', "S", function() vim.cmd("<C-u>split") end, { buffer = true, silent = true })
vim.keymap.set('n', "s", "<NOP>", { buffer = true })
vim.keymap.set('n', "gh", "<NOP>", { buffer = true })

local augroup_MyFtpluginNetrw = vim.api.nvim_create_augroup("MyFtpluginNetrw", { clear = true })
 vim.api.nvim_create_autocmd("BufLeave", { group = augroup_MyFtpluginNetrw, pattern = "*", callback = function() vim.cmd("if &ft ==# 'netrw' | setl nowrapscan | endif") end })
