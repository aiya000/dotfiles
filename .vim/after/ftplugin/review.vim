let b:undo_ftplugin = 'setl ' . join([
  \ 'sw<',
  \ 'ts<',
  \ 'et<',
  \ 'nospell<',
\ ])

setl sw=4 ts=4 et nospell

vmap <expr> is textobj#from_regexp#mapexpr('@<[a-z]\+>{\zs.*\ze}')
vmap <expr> as textobj#from_regexp#mapexpr('@<[a-z]\+>{.*}')
