let b:undo_ftplugin = 'setl ' . join([
    \ 'ts<',
    \ 'sw<',
    \ 'et<',
    \ 'conceallevel<',
    \ 'commentstring<',
    \ 'errorformat<',
\])

setl ts=2 sw=2 et conceallevel=0
let &commentstring = ' -- %s'
let &errorformat   = '%f:%l%c:%m' " a format for stack build and stack test

nnoremap <buffer><silent> <localleader><localleader><localleader>r :<C-u>echo 'stack test is started'<CR>:QuickRun stack_test<CR>
nnoremap <buffer><silent> <localleader><localleader>R :<C-u>call vimrc#open_terminal_as('stack_test', 'horizontal', 'stack test :tasty-test')<CR>
nnoremap <buffer><silent> <localleader><localleader>W :<C-u>StackWatchExec test :tasty-test<CR>
nnoremap <buffer><silent> <localleader><localleader>b :<C-u>call vimrc#open_terminal_as('none', 'horizontal', 'stack build')<CR>
nnoremap <buffer><silent> <localleader><localleader>r :<C-u>echo 'stack build is started'<CR>:QuickRun stack_build<CR>
nnoremap <buffer><silent> <localleader><localleader>t :<C-u>call <SID>stack_integrate_test_or_unit_or_both()<CR>
nnoremap <buffer><silent> <localleader><localleader>w :<C-u>StackQuickfixRun test :tasty-test<CR>
nnoremap <buffer><silent> <localleader>O :<C-u>vsp<CR>:Ghcie<CR>
nnoremap <buffer><silent> <localleader>S :<C-u>Aref stackage <C-r>=expand('<cword>')<CR><CR>
nnoremap <buffer><silent> <localleader>o :<C-u>vsp<CR>:Ghci <C-r>=expand('%:p')<CR><CR>

"TODO: Detect a context of Eta, and set filetype=eta, please
"nnoremap <buffer><silent> <localleader><localleader><localleader>r :<C-u>QuickRun eta<CR>
"nnoremap <buffer><silent> <localleader><localleader><localleader>b :<C-u>echo 'etlas build is started'<CR>:QuickRun etlas_build<CR>
"nnoremap <buffer><silent> <localleader><localleader><localleader>B :<C-u>call vimrc#open_terminal_as('none', 'horizontal', 'etlas build')<CR>

augroup FtpluginHaskell
    autocmd!
    " v for :StackWatchExec
    autocmd BufWritePost *.hs CClear
augroup END

function! s:stack_integrate_test_or_unit_or_both() abort
    echon join(["a: do tasty-test (default)",
        \       "b: do doctest",
        \       "c: do liquid-haskell",
        \       "d: all",
        \       ], "\n")
    let answer = getchar()
    let target = answer is char2nr('a') ? ':tasty-test'
    \          : answer is char2nr('b') ? ':doctest'
    \          : answer is char2nr('c') ? 'liquid-haskell'
    \          : answer is char2nr('d') ? ''
    \                                   : ':tasty-test'
    call vimrc#open_terminal_as('stack_test', 'horizontal', 'stack test ' . target)
endfunction
