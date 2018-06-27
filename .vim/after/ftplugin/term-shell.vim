" For sh, bash, zsh, or else

let b:undo_ftplugin = 'setl ' . join([
    \ 'nolist<',
    \ 'nonumber<',
    \ 'norelativenumber<',
\])

setl nolist nonumber norelativenumber
