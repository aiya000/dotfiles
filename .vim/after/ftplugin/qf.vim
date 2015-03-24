let b:undo_ftplugin = 'setl ' . join([
\	'cursorline<'
\])

setl cursorline

nnoremap <silent><buffer> Q :<C-u>bdelete<CR>
