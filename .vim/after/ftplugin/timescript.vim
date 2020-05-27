let b:undo_ftplugin = 'setl ' . join([
  \ 'tw<',
  \ 'ts<',
  \ 'sw<',
  \ 'et<',
  \ 'commentstring<',
\ ])

setl tw=0 ts=2 sw=2 et
let &commentstring = ' " %s'
