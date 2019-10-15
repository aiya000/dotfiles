let b:undo_ftplugin = 'setl ' . join([
  \ 'commentstring<',
  \ 'expandtab<',
  \ 'omnifunc<',
  \ 'shiftwidth<',
  \ 'tabstop<',
\ ])

setl tabstop=2 shiftwidth=2 expandtab omnifunc=lsp#complete
let &commentstring = '  // %s'
