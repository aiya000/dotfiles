let b:undo_ftplugin = 'setl ' .. join([
  \ 'nolist<',
\ ])
setl nolist

nnoremap <buffer><silent> Q <Cmd>bdelete!<CR>

autocmd! BufCreate,BufEnter *
  \  if &filetype ==# 'gin-diff'
    \| IndentGuidesDisable
  \| endif
