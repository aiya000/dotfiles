let b:undo_ftplugin = 'setl ' . join([
\	'wrap<',
\	'cursorline<',
\	'number<',
\	'relativenumber<',
\	'list<'
\])
setl wrap
setl cursorline
setl nonumber
setl norelativenumber
setl nolist

nnoremap <silent><buffer> Q :<C-u>bdelete<CR>

nmap <buffer> <C-r> <Plug>(adrone_home_reload)
nmap <buffer> <C-x> <Plug>(adrone_home_future)
nmap <buffer> <C-a> <Plug>(adrone_home_past)
nmap <buffer> s     <Plug>(adrone_home_open_say)
