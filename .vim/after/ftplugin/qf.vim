let b:undo_ftplugin = 'setl ' . join([
\   'statusline<',
\   'nonumber<',
\   'norelativenumber<',
\   'cursorline',
\   'nolist<',
\])

setl statusline+=\ %L
setl nonumber norelativenumber
setl cursorline nolist

nnoremap <buffer> <C-j> <CR>
