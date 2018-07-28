let b:undo_ftplugin = 'setl ' . join([
  \ 'tabstop<',
  \ 'shiftwidth<',
  \ 'nolist<',
  \ 'wrap<',
\])

setl tabstop=2 shiftwidth=2 nolist wrap

nnoremap <buffer><silent> <C-r> :<C-u>call vimrc#open_terminal_as('stack_test', 'horizontal', 'stack test', v:false)<CR><C-w>p:quit<CR><C-w>p
