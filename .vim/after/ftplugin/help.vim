if &readonly
  let b:undo_ftplugin = 'setlocal ' .. join([
    \ 'nolist<',
    \ 'nonumber<',
    \ 'norelativenumber<',
  \ ])
  setlocal nolist nonumber norelativenumber
  nnoremap <silent><buffer> Q :<C-u>helpclose<CR>
else
  let b:undo_ftplugin = 'setlocal ' .. join([
    \ 'tabstop<',
    \ 'shiftwidth<',
    \ 'expandtab<',
    \ 'conceallevel<',
  \ ])
  setlocal tabstop=8 shiftwidth=8 noexpandtab conceallevel=1
endif
