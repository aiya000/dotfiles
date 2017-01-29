" filetype defined by vim-pager and vim-manpager

let b:undo_ftplugin = 'setl ' . join([
\	'number<',
\	'relativenumber<',
\	'wrap<'
\])

setl nonumber norelativenumber wrap

nnoremap <buffer> Q :<C-u>q<CR>
" Forget the vim-manpager specified nmaps
nnoremap <buffer> <C-n> gt
nnoremap <buffer> <C-p> gT
