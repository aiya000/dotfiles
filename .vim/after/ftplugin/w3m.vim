let b:undo_ftplugin = 'setl ' . join([
\	'wrapscan<'
\])

setl wrapscan

nmap <buffer> H     <Plug>(w3m-back)
nmap <buffer> L     <Plug>(w3m-forward)
nmap <buffer> t     <Plug>(w3m-shift-click)
nmap <buffer> i     <Plug>(w3m-address-bar)
nmap <buffer> <C-i> <Plug>(w3m-next-link)
nmap <buffer> <C-o> <Plug>(w3m-prev-link)

nnoremap <silent><buffer> <localleader>r :<C-u>W3mShowExtenalBrowser<CR>
