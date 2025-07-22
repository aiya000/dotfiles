vim.opt_local.cursorline = true

vim.keymap.set('n', 'call', "ddu#ui#do_action('itemAction')<CR>", { buffer = true, silent = true })
vim.keymap.set('n', 'call', "ddu#ui#do_action('toggleSelectItem')<CR>", { buffer = true, silent = true })
vim.keymap.set('n', 'i', function()
  vim.cmd("call ddu#ui#do_action('openFilterWindow')")
end, { buffer = true, silent = true })
vim.keymap.set('n', 'call', "ddu#ui#do_action('quit')<CR>", { buffer = true, silent = true })
vim.keymap.set('n', 'call', "ddu#ui#do_action('quit')<CR>", { buffer = true, silent = true })
