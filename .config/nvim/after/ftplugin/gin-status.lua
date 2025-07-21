vim.cmd("let s:Msg = vital#vimrc#import('Vim.Message')")


vim.opt_local.cursorline = true

vim.keymap.set('n', "Q", function() vim.cmd("bdelete!") end, { buffer = true, silent = true })
vim.cmd("nmap <buffer><silent> A yy<Cmd>call term_start(['git', 'add','--patch', @\"[:-1]], #{")
  vim.cmd("\\ term_finish: 'close',")
vim.cmd("\\ })<CR>")
vim.keymap.set('n', "<buffer><silent>", "<C-r> <Cmd>GinStatus<CR>", { buffer = true, silent = true })
vim.cmd("nmap <buffer><silent><nowait> p <Plug>(gin-action-diff:smart:vsplit)")
vim.cmd("nmap <buffer><silent> sa <Plug>(gin-action-stash)")
vim.cmd("nmap <buffer><silent> S yy<Cmd>call <SID>stash_message(@\")<CR>")
vim.keymap.set('n', "sp", function() vim.cmd("Gin stash pop") end, { buffer = true, silent = true })
vim.keymap.set('n', "cC", function() vim.cmd("call <SID>commit_by_oco()") end, { buffer = true, silent = true })
vim.keymap.set('n', "cc", function() vim.cmd("call <SID>open_commit_buffer([])") end, { buffer = true, silent = true })
vim.keymap.set('n', "ca", function() vim.cmd("call <SID>open_commit_buffer(['--amend'])") end, { buffer = true, silent = true })
vim.keymap.set('n', "cf", ":<C-u>GCommitFixup<Space>", { buffer = true })
vim.cmd("nmap <buffer> <: <Plug>(gin-action-restore:ours)")
vim.cmd("nmap <buffer> >: <Plug>(gin-action-restore:theirs)")
vim.cmd("nmap <buffer> == <Plug>(gin-action-reset)")

vim.cmd([[
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
]])

  vim.cmd([[
  function! local.notify_failure(stdout, stderr, exit_code) abort dict
    call s:Msg.error($'exit_code: {a:exit_code}')
    call s:Msg.error($'stdout: {string(a:stdout)}')
    call s:Msg.error($'stderr: {string(a:stderr)}')
    GinStatus " Refresh
  endfunction
  ]])

  vim.call("vimrc#job#start_simply(")
    vim.cmd("\\ $'git stash push --message \"{message}\" -- \"{a:file_to_save}\"',")
    vim.cmd("\\ local.notify_success,")
    vim.cmd("\\ local.notify_failure,")
  vim.cmd("\\ )")
vim.cmd("endfunction")

vim.cmd([[
function! s:commit_by_oco() abort
  let local = {}

  " Notify result when oco is finished if Vim had leaved from the buffer of oco
  function! local.notify_when_generated(oco_bufnr, data) abort dict
    const current_bufnr = bufnr('%')
    if current_bufnr !=# a:oco_bufnr && a:data =~# 'Successfully committed'
      call vimrc#popup_atcursor('commit finished')
    endif
  endfunction
]])

  vim.cmd("const oco_bufnr = term_start('oco --yes', #{")
    vim.cmd("\\ vertical: v:true,")
    vim.cmd("\\ out_cb: { _job, data -> local.notify_when_generated(oco_bufnr, data) },")
    vim.cmd("\\ err_cb: { _job, data -> vimrc#popup_atcursor(data) },")
  vim.cmd("\\ })")
  vim.keymap.set('n', "Q", function() vim.cmd("bwipe") end, { buffer = true })
vim.cmd("endfunction")

vim.cmd([[
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
]])

vim.cmd([[
function! s:open_terminal_git_buffer(filetype, subcmd_list) abort
  call term_start(&shell, #{ curwin: v:true })
  const subcmd = a:subcmd_list->map({ _, x -> $"'{x}'" })->join(' ')

  const current_bufnr = bufnr('%')
  const sleeping_time_to_wait_spawning_terminal = 1000
  call timer_start(sleeping_time_to_wait_spawning_terminal, { _ ->
    \ term_sendkeys(current_bufnr, $"git {subcmd}\<CR>")
  \ })
endfunction
]])

vim.cmd([[
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
]])

vim.call("s:force_show_stash_size()")
