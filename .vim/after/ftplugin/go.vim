let b:undo_ftplugin = 'setl ' . join([
  \ 'commentstring<',
  \ 'foldmethod<'
\ ])

let &commentstring = ' /*%s*/'
setl foldmethod=syntax nofoldenable noet
