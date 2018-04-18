let b:undo_ftplugin = 'setl ' . join([
    \ 'commentstring<',
\])

let &commentstring = '{- %s -}'
setl ts=2 sw=2 et tw=0
