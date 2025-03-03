let s:Msg = vital#vimrc#import('Vim.Message')

let b:undo_ftplugin = 'setl ' . join([
  \ 'cursorline<',
\ ])

setl cursorline

nnoremap <buffer><silent> Q <Cmd>bdelete!<CR>
nmap <buffer><silent> A yy<Cmd>call term_start(['git', 'add','--patch', @"[:-1]], #{
  \ term_finish: 'close',
\ })<CR>
nnoremap <buffer><silent> <C-r> <Cmd>GinStatus<CR>
nmap <buffer><silent><nowait> p <Plug>(gin-action-diff:smart:vsplit)
nmap <buffer><silent> sa <Plug>(gin-action-stash)
nmap <buffer><silent> S yy<Cmd>call <SID>stash_message(@")<CR>
nnoremap <buffer><silent> sp <Cmd>Gin stash pop<CR>
nnoremap <buffer><silent> cc <Cmd>call <SID>open_commit_buffer([])<CR>
nnoremap <buffer><silent> ca <Cmd>call <SID>open_commit_buffer(['--amend'])<CR>
nnoremap <buffer> cf :<C-u>GCommitFixup<Space>
nmap <buffer> <: <Plug>(gin-action-restore:ours)
nmap <buffer> >: <Plug>(gin-action-restore:theirs)
nmap <buffer> == <Plug>(gin-action-reset)

function! s:stash_message(file_to_save) abort
  const message = input('Stash message: ')
  let local = {}

  function! local.notify_success(stdout, stderr) abort dict
    echomsg a:stdout->join("\n")
    if len(a:stderr) isnot 0
      call s:Msg.error(a:stderr->join("\n"))
    endif
    GinStatus " Refresh
  endfunction

  function! local.notify_failure(stdout, stderr, exit_code) abort dict
    call s:Msg.error($'exit_code: {a:exit_code}')
    call s:Msg.error($'stdout: {string(a:stdout)}')
    call s:Msg.error($'stderr: {string(a:stderr)}')
    GinStatus " Refresh
  endfunction

  call vimrc#job#start_simply(
    \ $'git stash push --message "{message}" -- "{a:file_to_save}"',
    \ local.notify_success,
    \ local.notify_failure,
  \ )
endfunction

function! s:open_commit_buffer(subcmd_list) abort
  try
    execute 'Gin' 'commit' '--verbose' a:subcmd_list->join(' ')
  catch
    echomsg $'Opening terminal instead of `:Gin commit` because: {v:exception}'
    call term_start(['git', 'commit'] + a:subcmd, #{
      \ term_finish: 'close',
    \ })
    return
  endtry
endfunction

function! s:open_terminal_git_buffer(filetype, subcmd_list) abort
  call term_start(&shell, a:filetype, 'stay', )
  const subcmd = a:subcmd_list->map({ _, x -> $"'{x}'" })->join(' ')

  const current_bufnr = bufnr('%')
  const sleeping_time_to_wait_spawning_terminal = 1000
  call timer_start(sleeping_time_to_wait_spawning_terminal, { _ ->
    \ term_sendkeys(current_bufnr, $"git {subcmd}\<CR>")
  \ })
endfunction

function! s:force_show_stash_size() abort
  const size = system('git stash list | wc -l')[:-2]
  if size ==# '0'
    return
  endif

  const topline = getline(1)
  const new_topline = $'{topline} [stash:{size}]'

  setl modifiable
  call setline(1, new_topline)
  setl nomodifiable
endfunction

call s:force_show_stash_size()
