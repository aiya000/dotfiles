let b:undo_ftplugin = 'setl ' . join([
\	'nolist<',
\	'wrapscan<'
\])

setl nolist
setl wrapscan

nmap             <buffer> H              G-
nnoremap         <buffer> L              <NOP>
nnoremap <silent><buffer> Q              :<C-u>quit<CR>
nnoremap <silent><buffer> ~              :<C-u>execute 'Explore' expand('~')<CR>
nnoremap <silent><buffer> <localleader>e :<C-u>quit<CR>
nnoremap <silent><buffer> V              :<C-u>vertical split<CR>
nnoremap <silent><buffer> S              :<C-u>split<CR>
nnoremap         <buffer> s              <NOP>
nnoremap         <buffer> gg             gg<CR>
