vim.keymap.set('n', "Q", function() vim.cmd("bdelete!") end, { buffer = true, silent = true })
