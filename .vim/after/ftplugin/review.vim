let b:undo_ftplugin = 'setl ' . join([
  \ 'sw<',
  \ 'ts<',
  \ 'et<',
  \ 'nospell<',
\ ])

setl sw=4 ts=4 et nospell

vmap <expr> ix textobj#from_regexp#mapexpr('@<[a-z]\+>{\zs.*\ze}')
vmap <expr> ax textobj#from_regexp#mapexpr('@<[a-z]\+>{.*}')
