nnoremap <buffer> n n
nnoremap <buffer> N N
nnoremap <buffer> / /
nnoremap <buffer> ? ?
nunmap <buffer> <C-k>

nmap <buffer> + <Plug>(fern-action-zoom)
nmap <buffer> - <Plug>(fern-action-zoom:reset)
nmap <buffer> o <Plug>(fern-action-open:vsplit)
nmap <buffer> O <Plug>(fern-action-open:tabedit)
nmap <buffer> <CR> <Plug>(fern-action-open:tabedit)
nmap <buffer> E <Plug>(fern-action-new-file=)
nmap <buffer> ?? <Plug>(fern-action-help)
nmap <buffer> <C-r> <Plug>(fern-action-reload)
" Delete
nmap <buffer> D <Plug>(fern-action-trash=)y<CR>
