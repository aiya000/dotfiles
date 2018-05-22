let b:undo_ftplugin = 'setl ' . join([
  \ 'nonumber<',
  \ 'norelativenumber<',
  \ 'nolist<',
\])

setl nonumber norelativenumber nolist

nnoremap <buffer> <localleader>r i<End><C-u>:reload<CR>
