let s:V = vital#vimrc#new()
let s:Msg = s:V.import('Vim.Message')
let s:Job = s:V.import('System.Job')
let s:List = s:V.import('Data.List')

" TODO: Move functions to the ftplugin

" General functions {{{

function! s:caddexpr_on_stdout(data) abort " {{{
  for line in a:data
    " NOTE:
    " On NeoVim v0.3.1, the job's on_stdout may take 『『『無』』』 as a line break and else.
    " This passes it.
    if line ==# ''
      caddexpr "\n"
      continue
    endif
    caddexpr line
  endfor

  " Always show the last lines
  for n in range(1, winnr('$'))
    if getwinvar(n, '&filetype') ==# 'qf'
      execute n 'windo' 'normal! G'
      wincmd p
      break
    endif
  endfor
endfunction " }}}

" ------- }}}

" Append quickrun config for windows
function! vimrc#plugins#append_config_quickrun_windows() abort " {{{
  " C#
  let g:quickrun_config.cs['command'] = 'csc.exe'
  let g:quickrun_config.cs['hook/output_encode/encoding'] = 'cp932:utf-8'
  " Java
  let g:quickrun_config.java['hook/output_encode/encoding'] = 'cp932:utf-8'
  " HTML
  let g:quickrun_config.html['command'] = 'rundll32'
  let g:quickrun_config.html['exec']    = '%c url.dll,FileProtocolHandler uri file://%s:p'
endfunction " }}}

" Delete otiose lines
function! vimrc#plugins#weblio_filter(output) abort " {{{
  let l:lines = split(a:output, "\n")
  return join(l:lines[17 : ], "\n")
endfunction " }}}

" Open tweetvim by private account
function! vimrc#plugins#twitter_private() abort " {{{
  if !exists('g:vimrc.private["twitter"]["priv_ac"]')
    call s:Msg.error('Not set env variable => g:vimrc.private["twitter"]["priv_ac"]')
    return
  endif

  execute ':TweetVimSwitchAccount' g:vimrc.private['twitter']['priv_ac']
  let g:vimrc.private['twitter']['curr_ac'] = g:vimrc.private['twitter']['priv_ac']

  TweetVimHomeTimeline
endfunction " }}}

" Open tweetvim_say by private account
function! vimrc#plugins#tweet_private() abort " {{{
  if !exists('g:vimrc.private["twitter"]["priv_ac"]')
    call s:Msg.error('Not set env variable => g:vimrc.private["twitter"]["priv_ac"]')
    return
  endif

  execute ':TweetVimSwitchAccount' g:vimrc.private['twitter']['priv_ac']
  TweetVimSay

  "@Incomplete('wait sync here')
  "execute ':TweetVimSwitchAccount' g:vimrc.private['twitter']['curr_ac']
endfunction " }}}

" Open tweetvim by public account
function! vimrc#plugins#twitter_public() abort " {{{
  if !exists("g:vimrc.private['twitter']['publ_ac']")
    call s:Msg.error("Not set env variable => g:vimrc.private['twitter']['publ_ac']")
    return
  endif

  execute ':TweetVimSwitchAccount ' g:vimrc.private['twitter']['publ_ac']
  let g:vimrc.private['twitter']['curr_ac'] = g:vimrc.private['twitter']['publ_ac']

  TweetVimHomeTimeline
endfunction " }}}

" Open tweetvim_say by public account
function! vimrc#plugins#tweet_public(...) abort " {{{
  if !exists('g:vimrc.private["twitter"]["publ_ac"]')
    call s:Msg.error('Not set env variable => g:vimrc.private["twitter"]["publ_ac"]')
    return
  endif

  execute ':TweetVimSwitchAccount ' g:vimrc.private['twitter']['publ_ac']
  if len(a:000) is 0
    TweetVimSay
  else
    execute 'TweetVimCommandSay' join(a:000)
  endif

  "@Incomplete('wait here')
  "execute ':TweetVimSwitchAccount ' g:vimrc.private['twitter']['curr_ac']
endfunction " }}}

" let s:read_to_quickfix_it {{{

let s:read_to_quickfix_it = { cmd ->
  \ s:Job.start(cmd, {
    \ 'on_stdout': function('s:caddexpr_on_stdout'),
    \ 'on_stderr': function('s:caddexpr_on_stdout'),
  \ })
\ }

" }}}
" TODO: Unify to one
function! vimrc#plugins#run_stack_quickfix(stack_subcmd) abort " {{{
  CClear
  let stack_cmd = ['stack'] + split(a:stack_subcmd, ' ')
  call s:read_to_quickfix_it(stack_cmd)
  copen
endfunction " }}}
function! vimrc#plugins#run_gradle_quickfix(gradle_subcmd) abort " {{{
  let current = getcwd()
  try
    CClear
    let gradle_cmd = ['gradle', '--console=plain'] + split(a:gradle_subcmd, ' ')
    execute ':lcd' g:vimrc.path_at_started
    call s:read_to_quickfix_it(gradle_cmd)
    copen
  finally
    execute ':lcd' current
  endtry

  " Avoid
  augroup ScalaCompileWatch
    autocmd!
    autocmd VimLeave * call vimrc#plugins#stop_scala_compile_watch_quickfix()
  augroup END
endfunction " }}}

" NOTE: This requires to add sbt-launch.jar to $PATH
function! vimrc#plugins#run_scala_compile_watch_quickfix(sbt_subcmd) abort " {{{
  CClear
  call vimrc#plugins#stop_scala_compile_watch_quickfix() " Avoid running more processes
  " Run sbt directly for killing the sbt process (vimrc#plugins#stop_scala_compile_watch_quickfix)
  let sbt_launcher = system('which sbt-launch.jar')[0:-2]
  let sbt_cmd = ['java', '-jar', sbt_launcher, '-Dsbt.log.noformat=true', '~test:compile'] + split(a:sbt_subcmd, ' ')
  let s:sbt_compile_watch_job = s:read_to_quickfix_it(sbt_cmd)
  copen
endfunction " }}}

function! vimrc#plugins#stop_scala_compile_watch_quickfix() abort " {{{
  if get(s:, 'sbt_compile_watch_job', v:null) isnot v:null
    call s:sbt_compile_watch_job.stop()
    let s:sbt_compile_watch_job = v:null
    cclose
  endif
endfunction " }}}

function! vimrc#plugins#grep_those(...) abort " {{{
  CClear
  call s:List.map(a:000, { word ->
    \ execute('grepadd ' . word . ' %', 'silent!')
  \ })
  copen
endfunction " }}}

function! vimrc#plugins#open_this_file_in_gui() abort " {{{
  let file = expand('%:p')
  call s:Job.start([g:vimrc.gui_editor, file])
endfunction " }}}
