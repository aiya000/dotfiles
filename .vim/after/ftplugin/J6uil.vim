let b:undo_ftplugin = 'setl ' . join([
  \ 'wrap<'
\ ])

setl wrap

nnoremap <silent><buffer> Q     :<C-u>bdelete<CR>
nnoremap <silent><buffer> <C-r> :J6uilReconnect<CR>
