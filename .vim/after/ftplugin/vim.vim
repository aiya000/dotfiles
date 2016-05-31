let b:undo_ftplugin = 'setl ' . join([
\	'textwidth<',
\	'commentstring<'
\])

set tw=0
let &commentstring = ' " %s'
