let b:undo_ftplugin = 'setl ' . join([
\	'commentstring<',
\	'ts<',
\	'sw<',
\	'et<',
\])

let &commentstring = ' # %s'
setl ts=4 sw=4 et
