let b:undo_ftplugin = 'setl ' . join([
  \ 'tabstop<',
  \ 'shiftwidth<',
  \ 'expandtab<'
\ ])

setl tabstop=2
setl shiftwidth=2
setl expandtab

nnoremap <buffer><silent> <C-l> <Esc>:syntax sync fromstart<CR>
