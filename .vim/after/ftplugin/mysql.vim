let b:undo_ftplugin = 'setl ' . join([
\	'tabstop<',
\	'shiftwidth<',
\	'expandtab<',
\	'commentstring<'
\])

setl tabstop=4
setl shiftwidth=4
setl expandtab
let &commentstring = ' -- %s'
