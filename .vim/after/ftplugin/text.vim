let b:undo_ftplugin = 'setl ' . join([
  \ 'textwidth<',
  \ 'tabstop<',
  \ 'shiftwidth<',
  \ 'expandtab<',
  \ 'indentkeys<',
  \ 'commentstring<'
\ ])

setl textwidth=0 tabstop=4 shiftwidth=4 expandtab indentkeys-=#
let &commentstring = ' %s'
