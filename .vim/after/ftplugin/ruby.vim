let b:undo_ftplugin = 'setl ' . join([
  \ 'tabstop<',
  \ 'shiftwidth<',
  \ 'expandtab<'
\ ])

setl tabstop=2 shiftwidth=2 expandtab
let &commentstring = ' # %s'
