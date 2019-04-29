let b:undo_ftplugin = 'setl ' . join([
  \ 'wrap<'
\ ])

setl nowrap

nmap <silent><buffer> Q     <Plug>(vimconsole_close)
nmap <silent><buffer> <C-r> <Plug>(vimconsole_clear)

resize 5
