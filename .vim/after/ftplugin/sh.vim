let b:undo_ftplugin = 'setl ' . join([
  \ 'commentstring<',
  \ 'ts<',
  \ 'sw<',
  \ 'et<',
\ ])

let &commentstring = ' # %s'
setl ts=2 sw=2 et
