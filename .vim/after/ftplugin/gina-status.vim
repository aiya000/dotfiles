let b:undo_ftplugin = 'setl ' . join([
  \ 'cursorline<',
\ ])

setl cursorline

let s:refresh_rate_to_show_stash = 50

function s:force_show_git_stash_size_onto_top(gina_status_bufnr, _) abort
  const current_pos = getpos('.')

  try
    execute ':buffer' a:gina_status_bufnr

    const status = getline(1)
    if s:has_stash_size(status)
      return
    endif

    if s:is_gina_status_never_loaded_yet(status)
      " Retry later
      call timer_start(
        \ s:refresh_rate_to_show_stash,
        \ function('s:force_show_git_stash_size_onto_top', [a:gina_status_bufnr])
        \ )
      return
    endif

    const size = system('git stash list | wc -l')[:-2]
    if size ==# '0'
      return
    endif

    call s:force_show(status, size)
  finally
    call setpos('.', current_pos)
  endtry
endfunction

function s:is_gina_status_never_loaded_yet(topline) abort
  return a:topline ==# ''
endfunction

function s:has_stash_size(topline) abort
  return a:topline =~# '\[stash [0-9]\+\]$'
endfunction

function s:force_show(topline, size) abort
  const status = a:topline .. ' ' .. printf('[stash %s]', a:size)
  setl modifiable
  call setline(1, status)
  setl nomodifiable
endfunction

call timer_start(
  \ s:refresh_rate_to_show_stash,
  \ function('s:force_show_git_stash_size_onto_top',
  \ [winbufnr(0)])
  \ )
