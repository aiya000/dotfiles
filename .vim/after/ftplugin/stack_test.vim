let b:undo_ftplugin = 'setl ' . join([
  \ 'tabstop<',
  \ 'shiftwidth<',
  \ 'nolist<',
  \ 'nonumber<',
  \ 'norelativenumber<',
  \ 'wrap<',
\ ])

setl tabstop=2 shiftwidth=2 nolist nonu nornu nowrap

nnoremap <buffer><silent> <C-r> :<C-u>call vimrc#open_terminal_as('stack_test', 'horizontal', 'stack test', {'path': g:vimrc.path_at_started, 'noclose': v:true})<CR><C-w>p:quit<CR><C-w>p
