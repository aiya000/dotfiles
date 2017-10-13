let s:V   = vital#vimrc#new()
let s:Job = s:V.import('System.Job')
let s:M   = s:V.import('Vim.Message')

let b:undo_ftplugin = 'setl ' . join([
\   'ts<',
\   'sw<',
\   'et<',
\   'conceallevel<'
\])

setl ts=2 sw=2 et conceallevel=0
let &commentstring = ' -- %s'

nnoremap <buffer><silent> <localleader><localleader>r :<C-u>echo 'stack test is started'<CR>:QuickRun stack_test<CR>
nnoremap <buffer><silent> <localleader><localleader>R :<C-u>call vimrc#open_terminal_as('stack_test', 'horizontal', 'stack test :integrate-test')<CR>
nnoremap <buffer><silent> <localleader><localleader>S :<C-u>Snowtify<CR>
nnoremap <buffer><silent> <localleader><localleader>t :<C-u>call <SID>stack_integrate_test_or_unit_or_both()<CR>

function! s:stack_integrate_test_or_unit_or_both() abort
    echon "a: do integrate-test (default)\n" .
    \     "b: do unite-test\n" .
    \     "c: both"
    let answer = getchar()
    let target = answer is char2nr('a') ? ':integrate-test'
    \          : answer is char2nr('b') ? ':unit-test'
    \          : answer is char2nr('c') ? ''
    \                                   : ':integrate-test'
    call vimrc#open_terminal_as('stack_test', 'horizontal', 'stack test ' . target)
endfunction

augroup FtpluginHaskell
    autocmd!
    autocmd BufWritePost *.hs HaskDogs
augroup END
