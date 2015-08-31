" filetype defined by vim-pager and vim-manpager

let b:undo_ftplugin = 'setl ' . join([
\	'number<',
\	'relativenumber<'
\])

setl nonumber norelativenumber

nnoremap <silent><buffer> Q :<C-u>bdelete!<CR>
