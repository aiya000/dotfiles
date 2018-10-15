" This filetype is extended by .vimrc

let b:undo_ftplugin = 'setl ' . join([
    \ 'commentstring<',
    \ 'nolist<',
    \ 'nonumber<',
    \ 'norelativenumber<',
\ ])

let &commentstring = ' %s'

" For undefined :terminal type
setl nolist nonumber norelativenumber
