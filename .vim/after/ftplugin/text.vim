" This filetype is extended by .vimrc

let b:undo_ftplugin = 'setl ' . join([
\	'textwidth<',
\	'tabstop<',
\	'shiftwidth<',
\	'expandtab<'
\])

setl textwidth=0
setl tabstop=2
setl shiftwidth=2
setl expandtab
let &commentstring = ' %s'
