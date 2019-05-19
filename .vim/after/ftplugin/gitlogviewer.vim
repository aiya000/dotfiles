" This filetype was presented by plugin/gitlogviewer.vim
let b:undo_ftplugin = 'setl ' . join([
  \ 'nolist<',
  \ 'cul<',
\ ])

setl nolist cul

nnoremap <buffer><silent> Q :<C-u>bdelete!<CR>
nnoremap <buffer><silent> <C-r> :<C-u>GLog<CR>
