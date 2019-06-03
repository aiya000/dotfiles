" This filetype is defined by .vimrc

let b:undo_ftplugin = 'setl ' . join([
  \ 'commentstring<',
  \ 'nolist<',
  \ 'nonumber<',
  \ 'norelativenumber<',
\ ])

let &commentstring = ' %s'

" For :terminal
setl nolist nonumber norelativenumber
