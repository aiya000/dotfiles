local nvim = require('nvim')

vim.keymap.set('n', 'p', 'pi', { buffer = true }) -- Enter insert mode after pasting

vim.keymap.set('n', 'I', function()
  nvim.feedkeys('i<C-a>')
end, { buffer = true })

vim.keymap.set('n', 'A', function()
  nvim.feedkeys('i<C-e>')
end, { buffer = true })
