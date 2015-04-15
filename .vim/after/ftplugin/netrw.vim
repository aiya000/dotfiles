let b:undo_ftplugin = 'setl ' . join([
\	'list<',
\	'wrapscan<'
\])

setl nolist
setl wrapscan

nmap             <buffer> H              G-
nmap             <buffer> gg             gg<CR>
nnoremap         <buffer> L              <NOP>
nnoremap <silent><buffer> Q              :<C-u>quit<CR>
nnoremap <silent><buffer> ~              :<C-u>execute 'Explore' expand('~')<CR>
nnoremap <silent><buffer> <localleader>e :<C-u>quit<CR>
nnoremap <silent><buffer> V              :<C-u>vertical split<CR>
nnoremap         <buffer> v              <NOP>
nnoremap <silent><buffer> S              :<C-u>split<CR>
nnoremap         <buffer> s              <NOP>

"@Incomplete('do not functioned')
highlight default link CursorLine Visual

augroup MyFtpluginNetrw
	autocmd!
	autocmd BufLeave    * if &ft ==# 'netrw' | setl nowrapscan | endif
	"@Incomplete('do not functioned')
	autocmd ColorScheme * highlight default link CursorLine Visual
augroup END
