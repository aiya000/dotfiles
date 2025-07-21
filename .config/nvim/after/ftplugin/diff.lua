vim.keymap.set('n', "Q", function() vim.cmd("<C-u>bdelete") end, { buffer = true, silent = true })

vim.cmd("normal zi")

-- git add --patch and others
vim.keymap.set('n', ">>", "0r+", { buffer = true })
vim.keymap.set('n', "<buffer>", "<< 0r-", { buffer = true })
vim.keymap.set('v', ">>", "(visualmode() ==# '') ? '0r+' : \"\\<C-v>0r+\"", { buffer = true })
vim.keymap.set('v', "<buffer><expr>", "<< (visualmode() ==# '') ? '0r-' : \"\\<C-v>0r-\"", { buffer = true })
