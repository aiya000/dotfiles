let b:undo_ftplugin = 'setl ' . join([
    \ 'ts<',
    \ 'sw<',
    \ 'et<',
    \ 'errorformat<',
\])

setl ts=4 sw=4 et
let &errorformat = '%t: %f: (%l\, %c): %m'

nnoremap <buffer><silent> <localleader><localleader>r :<C-u>call vimrc#plugins#quickrun_gradle_build()<CR>
nnoremap <buffer><silent> <localleader><localleader>R :<C-u>call vimrc#open_terminal_as('term-gradle', 'horizontal', "bash -c 'cd $(git rev-parse --show-toplevel) && gradle build'")<CR>

augroup FtpluginKotlin
    autocmd!
    " Incremental development :D
    autocmd BufWritePost *.kt CClear
augroup END
