let b:undo_ftplugin = 'setl ' . join([
\   'tabstop<',
\   'shiftwidth<',
\   'nolist<',
\])

setl tabstop=2 shiftwidth=2 nolist
nnoremap <buffer><silent> <localleader><localleader>R :<C-u>call vimrc#open_terminal_as('stack_test', 'horizontal', 'stack test :tasty-test')<CR>
