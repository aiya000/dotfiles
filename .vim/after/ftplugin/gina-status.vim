let b:undo_ftplugin = 'setl ' . join([
  \ 'cursorline<',
\ ])

setl cursorline

function s:force_show_git_stash_size_into_top(_) abort
  let status = getline(1)
  if (status ==# '')
    call timer_start(50, function('s:force_show_git_stash_size_into_top'))
    return
  elseif (status =~# '\[stash [0-9]\+\]$')
    return
  endif

  let size = system("git stash list | wc -l")[:-2]
  if size ==# '0'
    return
  endif

  let status = status .. ' ' .. printf("[stash %s]", size)
  setl modifiable
  call setline(1, status)
  setl nomodifiable
endfunction

call timer_start(50, function('s:force_show_git_stash_size_into_top'))
