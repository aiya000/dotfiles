let b:undo_ftplugin = 'setl ' . join([
\	'tabstop<',
\	'shiftwidth<',
\	'noexpandtab<',
\	'commentstring<'
\])
let &commentstring = ' /*%s*/'
setl tabstop=4 shiftwidth=4 noexpandtab
