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

nnoremap <buffer><silent> <localleader><localleader>r :<C-u>call <SID>exec_quickfix()<CR>
nnoremap <buffer><silent> <localleader><localleader>R :<C-u>call vimrc#open_terminal_as('term-gradle', 'horizontal', "bash -c 'cd $(git rev-parse --show-toplevel) && gradle run'", v:false)<CR>
nnoremap <buffer><silent> <localleader><localleader>w :<C-u>call <SID>start_quickfix()<CR>
nnoremap <buffer><silent> <C-k><C-w> :<C-u>call <SID>toggle_quickfix()<CR>

augroup FtpluginKotlin
    autocmd!
    autocmd BufWritePost *.kt call s:exec_quickfix_if_available()
augroup END

function! s:caddexpr_on_stout(data) abort
    for line in a:data
        caddexpr line
    endfor
endfunction

function! s:exec_quickfix() abort
    CClear " Clear old
    let current_dir = fnameescape(execute('pwd')[1:])
    echo 'gradle build is started'
    CdGitRoot

    call s:Job.start(['gradle', 'build', '--console=plain'], {
        \ 'on_stdout': function('s:caddexpr_on_stout'),
        \ 'on_stderr': function('s:caddexpr_on_stout'),
        \ 'on_exit' : { _ ->
            \ execute('cwindow')
        \ },
    \ })
    execute 'cd' current_dir
endfunction

let s:does_quickfix_watch = v:false

function! s:start_quickfix() abort
    let s:does_quickfix_watch = v:true
    call s:exec_quickfix()
endfunction

function! s:toggle_quickfix() abort
    let s:does_quickfix_watch = !s:does_quickfix_watch
    if s:does_quickfix_watch
        echo 'turned on the quickfix'
    else
        echo 'turned off the quickfix'
    endif
endfunction

function! s:exec_quickfix_if_available() abort
    if s:does_quickfix_watch
        call s:exec_quickfix()
    endif
endfunction
