" Utility functions for job_start()

" Example:
" call job_start(
"   \ ['xxx'],
"   \ vimrc#job#get_basic_options_completes_with(
"     \ function('s:foo')
"   \ )
" \ )

let s:Msg = vital#vimrc#import('Vim.Msg')

function vimrc#job#on_stdout(stdout, _channel, msg) abort
  call add(a:stdout, a:msg)
endfunction

function vimrc#job#on_stderr(stderr, _channel, msg) abort
  call add(a:stderr, a:msg)
endfunction

function vimrc#job#on_exit(stdout, stderr, on_succeed, _job, exit_code) abort
  if a:exit_code isnot 0
    call s:Msg.error('a job exited with ' .. a:exit_code)
    call s:Msg.error('stdout: ' .. string(a:stdout))
    call s:Msg.error('stderr: ' .. string(a:stderr))
    return
  endif

  call a:on_succeed(a:stdout, a:stderr)
endfunction

" param: `on_succeed` `(stdout: Array<string>, stderr: Array<string>) => void` called with succeed
function vimrc#job#get_basic_options_completes_with(on_succeed) abort
  let stdout = []
  let stderr = []

  return #{
    \ out_cb: function('vimrc#job#on_stdout', [stdout]),
    \ err_cb: function('vimrc#job#on_stderr', [stderr]),
    \ exit_cb: function('vimrc#job#on_exit', [stdout, stderr, a:on_succeed]),
  \ }
endfunction
