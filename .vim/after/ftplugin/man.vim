" filetype defined by vim-pager and vim-manpager

let b:undo_ftplugin = 'setl ' . join([
\	'nonumber<',
\	'norelativenumber<'
\])

setl nonumber
setl norelativenumber

nnoremap <silent><buffer> Q :<C-u>bdelete!<CR>
