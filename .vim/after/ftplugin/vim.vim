let b:undo_ftplugin = 'setl ' . join([
    \ 'tw<',
    \ 'ts<',
    \ 'sw<',
    \ 'et<',
    \ 'commentstring<',
\])

setl tw=0 ts=4 sw=4 et
let &commentstring = ' " %s'
