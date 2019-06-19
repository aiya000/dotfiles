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

nnoremap <buffer><silent> <localleader><localleader>R :<C-u>call vimrc#open_terminal_as('stack_test', 'vertical', 'stack test', {'noclose': v:true, 'path': g:vimrc.path_at_started})<CR>
" NOTE: v  This is useful for that is building happy codes and show its warnings/errors on vim-ghcid-quickfix
nnoremap <buffer><silent> <localleader><localleader>b :<C-u>call vimrc#open_terminal_as('stack_build', 'hidden', 'stack build', {'path': g:vimrc.path_at_started})<CR>
nnoremap <buffer><silent> <localleader><localleader>B :<C-u>call vimrc#open_terminal_as('stack_build', 'vertical', 'stack build', {'noclose': v:true, 'path': g:vimrc.path_at_started})<CR>
nnoremap <buffer><silent> <localleader><localleader>w :<C-u>call <SID>ghcid_quickfix_start_on_path_started()<CR>
nnoremap <buffer><silent> <localleader><localleader>W :<C-u>GhcidQuickfixStop<CR>
nnoremap <buffer><silent> <localleader><localleader>T :<C-u>call <SID>stack_integrate_test_or_unit_or_both()<CR>
nnoremap <buffer><silent> <localleader><localleader>t :<C-u>GhcidQuickfixStart '--command=stack ghci :tasty'<CR>
nnoremap <buffer><silent> <localleader><localleader>s :<C-u>sp \| StackGhci<CR>
nnoremap <buffer> <localleader>S :<C-u>Aref stackage <C-r>=expand('<cword>')<CR><CR>

"TODO: Detect a context of Eta, and set filetype=eta, please
"nnoremap <buffer><silent> <localleader><localleader><localleader>r :<C-u>QuickRun eta<CR>
"nnoremap <buffer><silent> <localleader><localleader><localleader>b :<C-u>echo 'etlas build is started'<CR>:QuickRun etlas_build<CR>
"nnoremap <buffer><silent> <localleader><localleader><localleader>B :<C-u>call vimrc#open_terminal_as('none', 'horizontal', 'etlas build')<CR>

augroup FtpluginHaskell
  autocmd!
  autocmd BufWritePre *.hs HaskellSortImport
augroup END

function! s:ghcid_quickfix_start_on_path_started() abort
  let current = getcwd()
  copen
  LcdStarted
  GhcidQuickfixStart
  execute 'lcd' current
endfunction

function! s:stack_integrate_test_or_unit_or_both() abort
  echon join([
    \ 'a: do tasty-test (default)',
    \ 'b: do doctest',
    \ 'c: do liquid-haskell',
    \ 'd: all',
  \ ], "\n")

  let answer = getchar()
  let target = answer is char2nr('a') ? ':tasty-test'
    \        : answer is char2nr('b') ? ':doctest'
    \        : answer is char2nr('c') ? 'liquid-haskell'
    \        : answer is char2nr('d') ? ''
    \                                 : ':tasty-test'
  call vimrc#open_terminal_as('stack_test', 'horizontal', 'stack test ' . target)
endfunction
