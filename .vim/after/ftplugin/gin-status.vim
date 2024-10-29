let b:undo_ftplugin = 'setl ' . join([
  \ 'cursorline<',
\ ])

setl cursorline

nnoremap <buffer><silent> Q <Cmd>bdelete!<CR>
nmap <buffer><silent><nowait> p <Plug>(gin-action-diff:smart:vsplit)
nmap <buffer><silent> sa <Plug>(gin-action-stash)
nnoremap <buffer><silent> sp <Cmd>Gin stash pop<CR>
nnoremap <buffer><silent> cc <Cmd>call <SID>open_commit_buffer('')<CR>
nnoremap <buffer><silent> ca <Cmd>call <SID>open_commit_buffer('--amend')<CR>
nnoremap <buffer> cf :<C-u>GCommitFixup<Space>
nmap <buffer> <: <Plug>(gin-action-restore:ours)
nmap <buffer> >: <Plug>(gin-action-restore:theirs)

let s:refresh_rate_to_show_stash = 50

function s:open_commit_buffer(subcmd) abort
  try
    execute 'Gin' 'commit' '--verbose' a:subcmd
  catch
    echomsg $'Opening terminal instead of `:Gin commit` because: {v:exception}'
    call s:open_term_commit_buffer(a:subcmd)
    return
  endtry
endfunction

function s:open_term_commit_buffer(subcmd) abort
  call vimrc#open_terminal_as('term-shell-git-commit', 'stay', &shell, #{ path: g:vimrc.git_root })

  const current_bufnr = bufnr('%')
  const sleeping_time_to_wait_spawning_terminal = 3000
  call timer_start(sleeping_time_to_wait_spawning_terminal, { _ ->
    \ term_sendkeys(current_bufnr, $"git commit {a:subcmd} \<CR>i:sparkles:\<Space>")
  \ }, #{ repeat: 1 })
endfunction

function s:force_show_git_stash_size_onto_top(gin_status_bufnr, _) abort
  const current_pos = getpos('.')

  try
    execute ':buffer' a:gin_status_bufnr

    const status = getline(1)
    if s:has_stash_size(status)
      return
    endif

    if s:is_gina_status_never_loaded_yet(status)
      " Retry later
      call timer_start(
        \ s:refresh_rate_to_show_stash,
        \ function('s:force_show_git_stash_size_onto_top', [a:gin_status_bufnr])
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

" TODO: Enable this when you want to use this, and update this
" call timer_start(
"   \ s:refresh_rate_to_show_stash,
"   \ function('s:force_show_git_stash_size_onto_top',
"   \ [winbufnr(0)])
"   \ )
