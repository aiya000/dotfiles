let b:undo_ftplugin = 'setl ' . join([
\	'commentstring<',
\	'cinkeys<'
\])

let &commentstring = ' /*%s*/'
setl cinkeys-=0#
