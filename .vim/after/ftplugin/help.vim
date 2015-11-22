let b:undo_ftplugin = 'setl ' . join([
\	'tabstop<',
\	'shiftwidth<',
\	'expandtab<'
\])

setl tabstop=8
setl shiftwidth=8
setl noexpandtab
setl conceallevel=3

nnoremap <silent><buffer> Q :<C-u>helpclose<CR>
