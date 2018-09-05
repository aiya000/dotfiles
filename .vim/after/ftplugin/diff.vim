nnoremap <buffer><silent> Q :<C-u>bdelete<CR>

normal zi

" git add --patch and others
nnoremap <buffer> >> 0r+
nnoremap <buffer> << 0r-
vnoremap <buffer><expr> >> (visualmode() ==# '') ? '0r+' : "\<C-v>0r+"
vnoremap <buffer><expr> << (visualmode() ==# '') ? '0r-' : "\<C-v>0r-"
