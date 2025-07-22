vim.opt_local.ts = 2
vim.opt_local.sw = 2
vim.opt_local.et = true

vim.cmd([[
if expand('%:t') ==# 'package.yaml' " If this file is package.yaml (Haskell's hpack)
  nnoremap <buffer><silent> <localleader><localleader>R :<C-u>call vimrc#open_terminal_as('stack_test', 'horizontal', 'stack test :tasty-test')<CR>
endif
]])
