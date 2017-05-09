let b:undo_ftplugin = 'setl ' . join([
\	'tabstop<',
\	'shiftwidth<',
\	'expandtab<',
\	'conceallevel<',
\])

setl tabstop=8 shiftwidth=8 noexpandtab
setl conceallevel=0

nnoremap <silent><buffer> Q :<C-u>helpclose<CR>
