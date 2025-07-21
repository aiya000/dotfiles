vim.keymap.set('n', "Q", function() vim.cmd("<C-u>close") end, { buffer = true, silent = true })
