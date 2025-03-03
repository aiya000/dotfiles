let b:undo_ftplugin = 'setl ' .. join([
  \ 'nolist<',
  \ 'tabstop<',
\ ])
setl nolist tabstop=8

nnoremap <buffer><silent> Q <Cmd>bdelete!<CR>

autocmd! BufCreate,BufEnter *
  \  if &filetype ==# 'gin-diff'
    \| IndentGuidesDisable
  \| endif
