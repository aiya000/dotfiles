let s:Msg = vital#vimrc#import('Vim.Message')

let b:undo_ftplugin = 'setl ' . join([
  \ 'cursorline<',
\ ])

setl cursorline

nnoremap <buffer><silent> Q <Cmd>bdelete!<CR>
nnoremap <buffer><silent> <C-r> <Cmd>GinStatus<CR>
nmap <buffer><silent><nowait> p <Plug>(gin-action-diff:smart:vsplit)
nmap <buffer><silent> sa <Plug>(gin-action-stash)
nmap <buffer><silent> ss yy<Cmd>call <SID>git_stash_message(@")<CR>
nnoremap <buffer><silent> sp <Cmd>Gin stash pop<CR>
nnoremap <buffer><silent> cc <Cmd>call <SID>open_commit_buffer('')<CR>
nnoremap <buffer><silent> ca <Cmd>call <SID>open_commit_buffer('--amend')<CR>
nnoremap <buffer> cf :<C-u>GCommitFixup<Space>
nmap <buffer> <: <Plug>(gin-action-restore:ours)
nmap <buffer> >: <Plug>(gin-action-restore:theirs)
nmap <buffer> == <Plug>(gin-action-reset)

function! s:git_stash_message(file_to_save) abort
  const message = input('Stash message: ')
  call vimrc#job#start_simply(
    \ $'git stash push --message "{message}" -- "{a:file_to_save}"',
    \ { stdout, stderr -> [
      \ s:Msg.echo('Normal', stdout->join("\n")),
      \ len(stderr) is 0 ? 'Nothing to do' : s:Msg.error(stderr->join("\n"))
    \ ]},
    \ { stdout, stderr, exit_code ->
      \ s:Msg.error($'{'{'}exit_code: {exit_code}, stdout: {string(stdout)}, stderr: {string(stderr)}, {'}'}')
    \ }
  \ )
endfunction

function! s:open_commit_buffer(subcmd) abort
  try
    execute 'Gin' 'commit' '--verbose' a:subcmd
  catch
    echomsg $'Opening terminal instead of `:Gin commit` because: {v:exception}'
    call s:open_term_commit_buffer(a:subcmd)
    return
  endtry
endfunction

function! s:open_term_commit_buffer(subcmd) abort
  call vimrc#open_terminal_as('term-shell-git-commit', 'stay', &shell, #{ path: g:vimrc.git_root })

  const current_bufnr = bufnr('%')
  const sleeping_time_to_wait_spawning_terminal = 3000
  call timer_start(sleeping_time_to_wait_spawning_terminal, { _ ->
    \ term_sendkeys(current_bufnr, $"git commit {a:subcmd} \<CR>i:sparkles:\<Space>")
  \ }, #{ repeat: 1 })
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
