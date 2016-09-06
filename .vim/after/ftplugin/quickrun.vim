let b:undo_ftplugin = 'setl ' . join([
\	'wrap<',
\	'list<'
\])

setl wrap
setl nolist

nnoremap <silent><buffer> Q :<C-u>quit<CR>
