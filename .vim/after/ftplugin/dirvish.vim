nmap <buffer> H <Plug>(dirvish_up)
nnoremap <buffer> / /
nnoremap <buffer> ? ?
nnoremap <buffer> Q :<C-u>quit<CR>
nnoremap <buffer> % :e<Space>
nnoremap <buffer> d :!mkdir<Space>
nnoremap <buffer><silent> D :<C-u>call <SID>delete_a_file_or_a_dir()<CR>

" Redraw
nnoremap <buffer> <C-r> :Dirvish<CR>

function s:delete_a_file_or_a_dir() abort
  const file_or_dir_name = getline('.')
  if confirm("Deleting '" .. file_or_dir_name .. "' ?", "&Yes\n&No") isnot 1
    echomsg "Deleting '" .. file_or_dir_name .. "' is canceled."
    return
  endif

  if file_or_dir_name =~# '/$'  " if it is a directory
    call system('rmdir ' .. file_or_dir_name)
    echomsg "Deleted a directory '" .. file_or_dir_name .. "' !"
    return
  endif
  " if it is a file
  call system('rm ' .. file_or_dir_name)
  echomsg "Deleted a file '" .. file_or_dir_name .. "' !"
endfunction
