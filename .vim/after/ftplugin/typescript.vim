let b:undo_ftplugin = 'setl ' . join([
  \ 'expandtab<',
  \ 'shiftwidth<',
  \ 'tabstop<',
\ ])

setl tabstop=2 shiftwidth=2 expandtab

nnoremap <buffer> <localleader><localleader>R :<C-u>call vimrc#open_terminal_as('', 'horizontal', 'yarn build', {'path': g:vimrc.path_at_started, 'noclose': v:true})<CR>
