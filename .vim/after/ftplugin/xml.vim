let b:undo_ftplugin = 'setl ' . join([
  \ 'tabstop<',
  \ 'shiftwidth<',
  \ 'expandtab<',
\ ])

setl tabstop=4 shiftwidth=4 expandtab

nnoremap <buffer><silent> vx :<C-u>call <SID>expand_surround_tag()<CR>

"NOTE: this is an easy implementation
function! s:expand_surround_tag() abort
  normal! g_hx_w
  let tag_name = expand('<cword>')
  normal! o</
  let @" = tag_name
  normal! pA>
endfunction
