let b:undo_ftplugin = 'setl ' . join([
\	'textwidth<',
\	'tabstop<',
\	'shiftwidth<',
\	'expandtab<',
\	'commentstring<'
\])

setl textwidth=0
setl tabstop=2
setl shiftwidth=2
setl expandtab
let &commentstring = '<!--%s-->'

nnoremap <silent><buffer> <localleader>r :<C-u>PrevimOpen<CR>

syntax sync fromstart
