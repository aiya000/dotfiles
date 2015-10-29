" filetype defined by vim-pager and vim-manpager

let b:undo_ftplugin = 'setl ' . join([
\	'number<',
\	'relativenumber<'
\])

setl nonumber norelativenumber

nmap <buffer> Q <Plug>(manpager-close)
" Forget vim-manpager specified nmaps
nnoremap <buffer> <C-n> gt
nnoremap <buffer> <C-p> gT
