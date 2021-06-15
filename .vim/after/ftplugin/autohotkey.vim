let b:undo_ftplugin = 'setl ' . join([
  \ 'sw<',
  \ 'ts<',
  \ 'et<',
  \ 'commentstring<'
\ ])

setl sw=2 ts=2 et
let &commentstring = '; %s'
