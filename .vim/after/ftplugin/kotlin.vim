let b:undo_ftplugin = 'setl ' . join([
    \ 'ts<',
    \ 'sw<',
    \ 'et<',
    \ 'errorformat<',
\])

setl ts=4 sw=4 et
let &errorformat = '%t: %f: (%l\, %c): %m'

nnoremap <buffer><silent> <localleader><localleader>r :<C-u>call <SID>quickrun_gradle_build()<CR>
nnoremap <buffer><silent> <localleader><localleader>R :<C-u>call vimrc#open_terminal_as('term-gradle', 'horizontal', "bash -c 'cd $(git rev-parse --show-toplevel) && gradle build'")<CR>

function! s:quickrun_gradle_build() abort
    let current_dir = fnameescape(execute('pwd')[1:])
    echo 'gradle build is started'
    CdGitRoot
    QuickRun gradle_build
    execute 'cd' current_dir
endfunction
