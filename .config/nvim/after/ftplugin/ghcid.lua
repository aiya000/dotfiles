vim.keymap.set('n', 'R', function()
  vim.call("vimrc#open_terminal_as('stack_test', 'horizontal', 'stack test :tasty-test')")
end, { buffer = true, silent = true })
