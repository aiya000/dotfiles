let b:undo_ftplugin = 'setl ' . join([
  \ 'commentstring<',
  \ 'tw<',
  \ 'ts<',
  \ 'sw<',
\ ])
let &commentstring = ' /*%s*/'
setl ts=2 sw=2 tw=100

nnoremap <buffer><silent> <localleader><localleader>r :<C-u>QuickfixRunGradle build<CR>
nnoremap <buffer><silent> <localleader><localleader>R :<C-u>call vimrc#open_terminal_as('term-gradle', 'horizontal', "gradle run'", {'path': g:vimrc.path_at_started, 'noclose': v:true})<CR>
nnoremap <buffer><silent> <localleader><localleader>w :<C-u>call <SID>start_quickfix()<CR>
nnoremap <buffer><silent> <localleader><localleader>W :<C-u>call <SID>stop_quickfix()<CR>
nnoremap <buffer><silent> <localleader><localleader>c :<C-u>call vimrc#open_terminal_as('term-gradle', 'horizontal', "gradle clean'", {'path': g:vimrc.path_at_started, 'noclose': v:true})<CR>
nnoremap <silent><buffer> <Esc> <Esc>:syntax sync fromstart<CR>
nmap <buffer> <C-l> <Esc>

syntax sync fromstart

augroup FtpluginJava
  autocmd!
  autocmd BufWritePost *.java call s:exec_quickfix_if_available()
augroup END

function! s:start_quickfix() abort
  let s:does_quickfix_watch = v:true
  QuickfixRunGradle build
endfunction

function! s:stop_quickfix() abort
  let s:does_quickfix_watch = v:false
  cclose
endfunction

function! s:exec_quickfix_if_available() abort
  if get(s:, 'does_quickfix_watch', v:false)
    QuickfixRunGradle build
  endif
endfunction
