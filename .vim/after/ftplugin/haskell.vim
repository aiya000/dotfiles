let b:undo_ftplugin = 'setl ' . join([
  \ 'ts<',
  \ 'sw<',
  \ 'et<',
  \ 'conceallevel<',
  \ 'commentstring<',
  \ 'errorformat<',
\ ])

setl ts=2 sw=2 et conceallevel=0
let &commentstring = ' -- %s'
let &errorformat   = '%f:%l:%c:%m' " a format for stack build and stack test

nnoremap <buffer><silent> <localleader><localleader><localleader>r :<C-u>echo 'stack test is started'<CR>:QuickRun stack_test<CR>
nnoremap <buffer><silent> <localleader><localleader>R :<C-u>call vimrc#open_terminal_as('stack_test', 'horizontal', 'stack test', v:false)<CR>
nnoremap <buffer><silent> <localleader><localleader>b :<C-u>call vimrc#open_terminal_as('none', 'horizontal', 'stack build', v:false)<CR>
nnoremap <buffer><silent> <localleader><localleader>r :<C-u>echo 'stack build is started'<CR>:QuickfixRunStack test<CR>
nnoremap <buffer><silent> <localleader><localleader>t :<C-u>call <SID>stack_integrate_test_or_unit_or_both()<CR>
nnoremap <buffer><silent> <localleader><localleader>w :<C-u>call <SID>start_the_quickfix()<CR>
nnoremap <buffer><silent> <localleader>O :<C-u>vsp<CR>:Ghcie<CR>
nnoremap <buffer><silent> <localleader>o :<C-u>vsp<CR>:Ghci <C-r>=expand('%:p')<CR><CR>
nnoremap <buffer> <localleader>S :<C-u>Aref stackage <C-r>=expand('<cword>')<CR><CR>

nnoremap <buffer><silent> <C-k><C-w> :<C-u>call <SID>toggle_the_quickfix()<CR>

function! s:start_the_quickfix() abort
    let s:does_quickfix_watch = v:true
    QuickfixRunStack test
endfunction

function! s:toggle_the_quickfix() abort
    let s:does_quickfix_watch = !get(s:, 'does_quickfix_watch', v:false)
    if s:does_quickfix_watch
        echo 'turned on the quickfix'
    else
        echo 'turned off the quickfix'
    endif
endfunction

function! s:exec_the_quickfix_if_enabled() abort
    if get(s:, 'does_quickfix_watch', v:false)
        QuickfixRunStack test
    endif
endfunction

"TODO: Detect a context of Eta, and set filetype=eta, please
"nnoremap <buffer><silent> <localleader><localleader><localleader>r :<C-u>QuickRun eta<CR>
"nnoremap <buffer><silent> <localleader><localleader><localleader>b :<C-u>echo 'etlas build is started'<CR>:QuickRun etlas_build<CR>
"nnoremap <buffer><silent> <localleader><localleader><localleader>B :<C-u>call vimrc#open_terminal_as('none', 'horizontal', 'etlas build')<CR>

augroup FtpluginHappy
    autocmd!
    autocmd BufWritePost *.hs call s:exec_the_quickfix_if_enabled()
augroup END

function! s:stack_integrate_test_or_unit_or_both() abort
  echon join([
    \ "a: do tasty-test (default)",
    \ "b: do doctest",
    \ "c: do liquid-haskell",
    \ "d: all",
  \ ], "\n")
  let answer = getchar()
  let target = answer is char2nr('a') ? ':tasty-test'
  \          : answer is char2nr('b') ? ':doctest'
  \          : answer is char2nr('c') ? 'liquid-haskell'
  \          : answer is char2nr('d') ? ''
  \                                   : ':tasty-test'
  call vimrc#open_terminal_as('stack_test', 'horizontal', 'stack test ' . target)
endfunction
