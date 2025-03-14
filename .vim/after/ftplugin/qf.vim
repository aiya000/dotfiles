let s:Math = vital#vimrc#import('Math')

let b:undo_ftplugin = 'setl ' .. join([
  \ 'statusline<',
  \ 'nonumber<',
  \ 'norelativenumber<',
  \ 'cursorline',
  \ 'nolist<',
  \ 'wrap<',
\ ])

setl statusline+=\ %L
setl nonumber norelativenumber
setl cursorline nolist wrap

nnoremap <buffer> Q <Cmd>bdelete<CR>
nnoremap <buffer> <C-j> <CR>
nnoremap <buffer> <C-r> <Cmd>call setqflist([])<CR>
nnoremap <buffer><silent> <expr> <C-a><C-a> <SID>go_to_errorformat(v:count1)
nnoremap <buffer><silent> <expr> <C-x><C-x> <SID>go_to_errorformat(-v:count1)

" Thanks thinca
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
