let b:undo_ftplugin = 'setl ' . join([
  \ 'nolist<',
  \ 'nonumber<',
  \ 'norelativenumber<',
\ ])

setl nolist nonumber norelativenumber

inoremap <buffer> <C-p> <Esc><C-w>pk<C-w>pa
inoremap <buffer> <C-n> <Esc><C-w>pj<C-w>pa
inoremap <buffer><silent> <C-l> <Esc>:q<CR><C-w>p
inoremap <buffer><silent> <Esc> <Esc>:q<CR><C-w>p
inoremap <buffer><silent> <C-[> <Esc>:q<CR><C-w>p
