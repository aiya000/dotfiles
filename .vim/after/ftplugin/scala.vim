let b:undo_ftplugin = 'setl ' . join([
\	'tabstop<',
\	'shiftwidth<',
\	'expandtab<',
\	'commentstring<'
\])
let &commentstring = ' /*%s*/'
setl tabstop=4 shiftwidth=4 expandtab
