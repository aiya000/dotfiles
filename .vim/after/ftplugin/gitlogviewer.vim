" This filetype was presented by plugin/gitlogviewer.vim

let s:V = vital#vimrc#new()
let s:List = s:V.import('Data.List')

let b:undo_ftplugin = 'setl ' . join([
  \ 'nolist<',
  \ 'cul<',
\ ])

setl nolist cul

nnoremap <buffer><silent> Q :<C-u>bdelete!<CR>
nnoremap <buffer><silent> S :<C-u>call <SID>show_show()<CR>
nnoremap <buffer><silent> <C-r> :<C-u>GitLogViewer <C-r>=b:gitlogviewer_args<CR><CR>

function s:show_show() abort
  if s:List.has(split(b:gitlogviewer_args), '--oneline')
    normal! _"zyiw
  else
    normal! g_"zyiw
  endif

  execute 'GitShowViewer' @z
endfunction
