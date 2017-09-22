let b:undo_ftplugin = 'setl ' . join([
\   'nonumber<',
\   'norelativenumber<',
\   'nolist<',
\   'wrap<',
\   'noexpandtab<',
\])

IndentGuidesDisable

setl nonumber norelativenumber nolist wrap noexpandtab

nnoremap <buffer><silent> Q :<C-u>bdelete<CR>
