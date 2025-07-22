vim.keymap.set('n', 'Q', function()
  vim.cmd('<C-u>bdelete')
end, { buffer = true, silent = true })
