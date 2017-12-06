let s:V    = vital#vimrc#new()
let s:Math = s:V.import('Math')
let s:O    = s:V.import('Data.Optional')

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

nnoremap <buffer> Q :<C-u>bdelete<CR>
nnoremap <buffer> <C-j> <CR>
nnoremap <buffer><silent> <expr> <C-a> <SID>go_to_errorformat(v:count1)
nnoremap <buffer><silent> <expr> <C-x> <SID>go_to_errorformat(-v:count1)

" Thanks thinca for teaching this
function! s:go_to_errorformat(motion)
    let max = line('$')
    let list = getloclist(0)
    if empty(list) || len(list) != max
        let list = getqflist()
    endif
    let cur = line('.') - 1
    let pos = s:Math.modulo(cur + a:motion, max)
    let m = 0 < a:motion ? 1 : -1
    while cur != pos && list[pos].bufnr == 0
        let pos = s:Math.modulo(pos + m, max)
    endwhile
    return (pos + 1) . 'G'
endfunction
