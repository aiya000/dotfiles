let b:undo_ftplugin = 'setl ' . join([
  \ 'commentstring<',
\ ])

let &commentstring = ' /*%s*/'

nnoremap <buffer><silent> <localleader><localleader>r :<C-u>call <SID>run_quickfix()<CR>
nnoremap <buffer><silent> <localleader><localleader>w :<C-u>call <SID>start_quickfix()<CR>
nnoremap <buffer><silent> <localleader><localleader>W :<C-u>call <SID>stop_quickfix()<CR>

augroup FtpluginC
  autocmd!
  autocmd BufWritePost *.c,*.h call s:exec_quickfix_if_available()
augroup END

function! s:is_ready_to_ccache() abort
  let directory_exists =
    \ isdirectory(expand('~/hdd/.ccache')) ||
    \ isdirectory(expand('~/.ccache'))

  return directory_exists
    \ && executable('ccache')
endfunction

let s:make_args = s:is_ready_to_ccache()
  \ ? ('"' . 'CC="ccache gcc"' . '"')
  \ : ''

function! s:run_quickfix() abort
  execute 'QuickfixRunMake' s:make_args
endfunction

function! s:start_quickfix() abort
  let s:does_quickfix_watch = v:true
  execute 'QuickfixRunMake' s:make_args
endfunction

function! s:stop_quickfix() abort
  let s:does_quickfix_watch = v:false
  cclose
endfunction

function! s:exec_quickfix_if_available() abort
  if get(s:, 'does_quickfix_watch', v:false)
    execute 'QuickfixRunMake' s:make_args
  endif
endfunction
