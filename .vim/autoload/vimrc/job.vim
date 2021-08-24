" Utility functions for job_start()

" Example:
" call job_start(
"   \ ['xxx'],
"   \ vimrc#job#get_basic_options_completes_with(
"     \ function('s:foo')
"   \ )
" \ )

let s:Msg = vital#vimrc#import('Vim.Message')

function vimrc#job#on_stdout(stdout, _channel, msg) abort
  call add(a:stdout, a:msg)
endfunction

function vimrc#job#on_stderr(stderr, _channel, msg) abort
  call add(a:stderr, a:msg)
endfunction

function vimrc#job#on_exit(stdout, stderr, on_succeed, on_failed, _job, exit_code) abort
  if a:exit_code isnot 0
    call a:on_failed(a:stdout, a:stderr, a:exit_code)
    return
  endif

  call a:on_succeed(a:stdout, a:stderr)
endfunction

" params:
"   on_succeed: (stdout: Array<string>, stderr: Array<string>) => void
"     called with succeed
"   on_failed: (stdout: Array<string>, stderr: Array<string>, exit_code: int) => void
"     called with failed
function vimrc#job#get_basic_options_completes_with(...) abort
  const OnSucceed = get(a:000, 0, v:null)
  if OnSucceed is v:null
    throw 'vimrc#job#start_simply: 1 or 2 args required'
  endif
  const OnFailed = get(a:000, 1, { _1, _2, _3 -> 0 })

  const stdout = []
  const stderr = []

  return #{
    \ out_cb: function('vimrc#job#on_stdout', [stdout]),
    \ err_cb: function('vimrc#job#on_stderr', [stderr]),
    \ exit_cb: function('vimrc#job#on_exit', [stdout, stderr, OnSucceed, OnFailed]),
  \ }
endfunction

" params:
"   command: Array<string>
"   OnSucceed: (stdout: Array<string>, stderr: Array<string>) => void
"     called with succeed
"   OnFailed: (stdout: Array<string>, stderr: Array<string>, exit_code: int) => void
"     called with failed
function vimrc#job#start_simply(...) abort
  const command = get(a:000, 0, v:null)
  const OnSucceed = get(a:000, 1, v:null)
  if command is v:null || OnSucceed is v:null
    throw 'vimrc#job#start_simply: 2 or 3 args required'
  endif

  const OnFailed = get(a:000, 2, { _, __, ___ -> '' })
  call job_start(command, vimrc#job#get_basic_options_completes_with(OnSucceed, OnFailed))
endfunction
