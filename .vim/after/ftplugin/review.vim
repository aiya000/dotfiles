let b:undo_ftplugin = 'setl ' . join([
    \ 'sw<',
    \ 'ts<',
    \ 'et<',
\ ])

setl sw=2 ts=2 et

vmap <expr> ix textobj#from_regexp#mapexpr('@<[a-z]\+>{\zs.*\ze}')
vmap <expr> ax textobj#from_regexp#mapexpr('@<[a-z]\+>{.*}')
