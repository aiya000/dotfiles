let s:V = vital#vimrc#new()
let s:Job = s:V.import('System.Job')

let b:undo_ftplugin = 'setl ' . join([
    \ 'ts<',
    \ 'sw<',
    \ 'et<',
    \ 'errorformat<',
\])

setl ts=4 sw=4 et
let &errorformat = '%t: %f: (%l\, %c): %m'

nnoremap <buffer><silent> <localleader><localleader>r :<C-u>QuickfixRunGradle build<CR>
nnoremap <buffer><silent> <localleader><localleader>R :<C-u>call vimrc#open_terminal_as('term-gradle', 'horizontal', "bash -c 'cd $(git rev-parse --show-toplevel) && gradle run'", v:false)<CR>
nnoremap <buffer><silent> <localleader><localleader>w :<C-u>call <SID>start_quickfix()<CR>
nnoremap <buffer><silent> <C-k><C-w> :<C-u>call <SID>toggle_quickfix()<CR>
nnoremap <silent><buffer> <Esc> <Esc>:syntax sync fromstart<CR>
nmap             <buffer> <C-l> <Esc>

syntax sync fromstart

augroup FtpluginKotlin
    autocmd!
    autocmd BufWritePost *.kt call s:exec_quickfix_if_available()
augroup END

function! s:start_quickfix() abort
    let s:does_quickfix_watch = v:true
    QuickfixRunGradle build
endfunction

function! s:toggle_quickfix() abort
    let s:does_quickfix_watch = !get(s:, 'does_quickfix_watch', v:false)
    if s:does_quickfix_watch
        echo 'turned on the quickfix'
    else
        echo 'turned off the quickfix'
    endif
endfunction

function! s:exec_quickfix_if_available() abort
    if get(s:, 'does_quickfix_watch', v:false)
        QuickfixRunGradle build
    endif
endfunction
