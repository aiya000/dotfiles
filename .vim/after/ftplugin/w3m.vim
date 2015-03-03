let b:undo_ftplugin = 'setl ' . join([
\	'wrapscan<'
\])

setl wrapscan

nmap             <buffer> H              <BS>
nnoremap <silent><buffer> <C-u>          :<C-u>W3mAddressBar <CR>
nnoremap <silent><buffer> <localleader>E :<C-u>W3mShowExtenalBrowser <CR>
