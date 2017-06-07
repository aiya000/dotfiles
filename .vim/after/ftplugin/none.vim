let b:undo_ftplugin = 'setl ' . join([
\	'commentstring<',
\	'nolist<',
\	'nonumber<',
\	'norelativenumber<',
\])

" This filetype is extended by .vimrc
let &commentstring = ' %s'

" For undefined :terminal type
setl nolist nonumber norelativenumber
