let b:undo_ftplugin = 'setl ' . join([
\	'wrap<'
\])

setl wrap

nnoremap <silent><buffer> Q :<C-u>quit<CR>
