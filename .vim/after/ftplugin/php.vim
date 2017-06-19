let b:undo_ftplugin = 'setl ' . join([
\	'commentstring<',
\	'expandtab<'
\])

let &commentstring = ' /*%s*/'
setl noexpandtab
