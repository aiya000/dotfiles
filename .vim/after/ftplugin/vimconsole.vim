let b:undo_ftplugin = 'setl ' . join([
\	'wrap<'
\])

setl nowrap

nmap <silent><buffer> Q     <Plug>(vimconsole_close)
nmap <silent><buffer> <C-l> <Plug>(vimconsole_clear)
