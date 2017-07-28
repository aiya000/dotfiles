let b:undo_ftplugin = 'setl ' . join([
\	'textwidth<',
\	'ts<',
\	'sw<',
\	'et<',
\	'commentstring<',
\])

set tw=0 ts=4 sw=4 et
let &commentstring = ' " %s'
