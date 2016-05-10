let b:undo_ftplugin = 'setl ' . join([
\	'nonumber<',
\	'norelativenumber<',
\	'nolist<'
\])

setl nonumber norelativenumber nolist

nnoremap <buffer><silent> Q :<C-u>bd<CR>
