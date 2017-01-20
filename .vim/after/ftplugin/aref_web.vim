let b:undo_ftplugin = 'setl ' . join([
\	'nonumber<',
\	'norelativenumber<',
\	'nolist<',
\	'wrap<'
\])

setl nonumber norelativenumber nolist wrap

nnoremap <buffer><silent> Q :<C-u>bd<CR>
