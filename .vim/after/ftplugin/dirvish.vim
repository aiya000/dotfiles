"NOTE: ? Prevent appearing the character '«' at the beginning of a line when opened
normal! 0

nmap <buffer> H <Plug>(dirvish_up)
nmap <buffer> E eb
nnoremap <buffer> / /
nnoremap <buffer> <C-n> gT
nnoremap <buffer> <C-n> gt
nnoremap <buffer> ? ?
nnoremap <buffer> Q <Cmd>quit<CR>
nnoremap <buffer><nowait> d :!mkdir<Space>
nnoremap <buffer><silent> D <Cmd>call <SID>delete_file_or_dir()<CR>

" Redraw
nnoremap <buffer> <C-r> :Dirvish<CR>

function s:delete_file_or_dir() abort
  const file_or_dir_name = getline('.')
  if confirm($"Deleting '{file_or_dir_name}' ?", "&Yes\n&No") isnot 1
    echomsg 'Canceled'
    return
  endif

  if file_or_dir_name =~# '/$'  " if it is a directory
    call system($'rmdir ' .. file_or_dir_name)
    echomsg $'Deleted: {file_or_dir_name}'
    return
  endif
  " if it is a file
  call system('rm ' .. file_or_dir_name)
  echomsg $'Deleted: {file_or_dir_name}'
endfunction

augroup FtpluginDirvish
  autocmd!
  autocmd BufNew,BufEnter,WinEnter *
    \  if &ft ==# 'dirvish'
      \| execute ':lcd' expand('%:p:h')
    \| endif
augroup END
