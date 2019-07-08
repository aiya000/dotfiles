let b:undo_ftplugin = 'setl ' . join([
  \ 'tabstop<',
  \ 'shiftwidth<',
  \ 'expandtab<'
\ ])

setl tabstop=2 shiftwidth=2 expandtab omnifunc=lsp#complete

nnoremap <buffer><silent> <C-l> :<C-u>syntax sync fromstart \| PreciousReset<CR>
