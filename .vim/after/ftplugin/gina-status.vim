let b:undo_ftplugin = 'setl ' . join([
  \ 'cursorline<',
\ ])

setl cursorline

function s:force_show_git_stash_size_into_top(gina_status_bufnr, _) abort
  let current_bufnr = winbufnr(0)

  try
    execute ':buffer' a:gina_status_bufnr
    let status = getline(1)

    if s:is_gina_status_never_loaded_yet(status)
      " Retry later
      call timer_start(50, function('s:force_show_git_stash_size_into_top', [a:gina_status_bufnr]))
      return
    elseif s:has_stash_size(status)
      return
    endif

    let size = system("git stash list | wc -l")[:-2]
    if size ==# '0'
      return
    endif

    call s:force_show(status, size)
  finally
    execute ':buffer' current_bufnr
  endtry
endfunction

function s:is_gina_status_never_loaded_yet(topline) abort
  return a:topline ==# ''
endfunction

function s:has_stash_size(topline) abort
  return a:topline =~# '\[stash [0-9]\+\]$'
endfunction

function s:force_show(topline, size) abort
  let status = a:topline .. ' ' .. printf('[stash %s]', a:size)
  setl modifiable
  call setline(1, status)
  setl nomodifiable
endfunction

call timer_start(50, function('s:force_show_git_stash_size_into_top', [winbufnr(0)]))
