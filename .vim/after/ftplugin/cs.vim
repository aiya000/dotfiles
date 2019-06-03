let b:undo_ftplugin = 'setl ' . join([
  \ 'commentstring<',
  \ 'cinkeys<',
  \ 'sw<',
  \ 'ts<',
  \ 'et<',
\ ])

let &commentstring = ' /*%s*/'
setl cinkeys-=0# sw=4 ts=4 et
