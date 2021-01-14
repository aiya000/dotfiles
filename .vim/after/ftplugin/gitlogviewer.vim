" This filetype was presented by plugin/gitlogviewer.vim

let s:V = vital#vimrc#new()
let s:List = s:V.import('Data.List')

let b:undo_ftplugin = 'setl ' . join([
  \ 'nolist<',
  \ 'cul<',
\ ])

setl nolist cul

function s:try_show_git_show() abort
  try
    call s:show_git_show()
  catch
    echomsg v:exception
    close
    call s:show_git_show()
  endtry
endfunction

function s:show_git_show() abort
  normal! [z
  vsplit
  if s:List.has(split(b:gitlogviewer_args), '--oneline')
    normal! _"zyiw
  else
    " TODO: Currently, this is not working if I'm on wrapped line.
    normal! g_"zyiw
  endif

  execute 'GitShowViewer' @z
endfunction

nnoremap <buffer><silent> Q :<C-u>bdelete!<CR>
nnoremap <buffer><silent> S :<C-u>call <SID>try_show_git_show()<CR>
nnoremap <buffer><silent> <C-r> :<C-u>GitLogViewer <C-r>=b:gitlogviewer_args<CR><CR>
