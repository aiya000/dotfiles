let b:undo_ftplugin = 'setl ' . join([
\	'wrap<',
\	'cursorline<',
\	'number<',
\	'relativenumber<'
\])
setl wrap
setl cursorline
setl nonumber
setl norelativenumber

nnoremap <silent><buffer> Q :<C-u>bdelete<CR>

nmap <buffer> <C-r> <Plug>(adrone_home_reload)
nmap <buffer> <     <Plug>(adrone_home_future)
nmap <buffer> >     <Plug>(adrone_home_past)
nmap <buffer> s     <Plug>(adrone_home_open_say)
