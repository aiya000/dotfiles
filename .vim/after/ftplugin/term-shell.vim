" For sh, bash, zsh, or else

let b:undo_ftplugin = 'setl ' . join([
    \ 'nolist<',
    \ 'nonumber<',
    \ 'norelativenumber<',
    \ 'cursorline<',
\ ])

setl nolist nonumber norelativenumber cursorline
