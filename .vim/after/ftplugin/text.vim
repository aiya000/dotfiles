" This filetype is extended by .vimrc

let b:undo_ftplugin = 'setl ' . join([
\	'textwidth<',
\	'tabstop<',
\	'shiftwidth<',
\	'expandtab<',
\	'indentkeys<',
\	'commentstring<'
\])

setl textwidth=0
setl tabstop=2
setl shiftwidth=2
setl expandtab
setl indentkeys-=#
let &commentstring = ' %s'
