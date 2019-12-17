let b:undo_ftplugin = 'setl ' . join([
    \ 'nolist<'
\ ])

setl nolist

nnoremap <buffer><silent> Q :<C-u>quit<CR>
nnoremap <buffer><silent> <leader>e :<C-u>quit<CR>

nmap <buffer> H <Plug>(vaffle-open-parent)
nmap <buffer> % <Plug>(vaffle-new-file)
nmap <buffer> d <Plug>(vaffle-mkdir)
nmap <buffer> D <Plug>(vaffle-delete-selected)
nmap <buffer> o <Plug>(vaffle-open-selected)
nmap <buffer> <C-j> <Plug>(vaffle-open-selected)
nmap <buffer> <C-r> <Plug>(vaffle-refresh)
