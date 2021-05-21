let b:undo_ftplugin = 'setl ' . join([
  \ 'tabstop<',
  \ 'shiftwidth<',
  \ 'expandtab<',
\ ])

setl ts=2 sw=2 et

if expand('%:t') ==# 'package.yaml' " If this file is package.yaml (Haskell's hpack)
  nnoremap <buffer><silent> <localleader><localleader>R :<C-u>call vimrc#open_terminal_as('stack_test', 'horizontal', 'stack test :tasty-test')<CR>
endif
