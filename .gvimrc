"---------------"
" Global values "
"---------------"
" {{{

let g:gvimrc = get(g:, 'gvimrc', {})
let g:gvimrc['loaded'] = get(g:gvimrc, 'loaded', 0)

" }}}


"---------"
" Options "
"---------"
" {{{

" TODO: If Windows
let g:vimrc['guifont'] = {
  \ 'size': 11,
\ }
"let &guifont = 'Ricty-Nerd-Font-Regular ' . g:vimrc.guifont.size
let &guifont = 'RictyDiminished NF ' . g:vimrc.guifont.size

set guioptions-=T
set guioptions-=m
set guioptions-=e
set guioptions-=r
set guioptions-=L
set guioptions+=c
set winaltkeys=no
set mouse=

" }}}


"----------"
" Commands "
"----------"
" {{{

function! s:open_this_file_in_new_window()
  let filepath = expand('%')
  execute ':bd' filepath
  execute ':!start' g:vimrc.gui_editor printf('"%s"', fnameescape(filepath))
endfunction

command! OpenThisFileInNewWindow call s:open_this_file_in_new_window()

" }}}


"---------"
" Plugins "
"---------"
" --- TweetVim --- {{{

let g:tweetvim_display_username = 1
let g:tweetvim_display_icon     = 1

" }}}
" --- J6uil --- {{{

let g:J6uil_display_icon = 1

" }}}
" --- vim-suvmode --- {{{

call submode#enter_with('font_size', 'n', 's', '<C-s>+', ':<C-u>call vimrc#cmd#increment_gui_fontsize()<CR>')
call submode#enter_with('font_size', 'n', 's', '<C-s>-', ':<C-u>call vimrc#cmd#decrement_gui_fontsize()<CR>')
call submode#map('font_size', 'n', 's', '+', ':<C-u>call vimrc#cmd#increment_gui_fontsize()<CR>')
call submode#map('font_size', 'n', 's', '-', ':<C-u>call vimrc#cmd#decrement_gui_fontsize()<CR>')

" }}}

syntax enable
let g:gvimrc['loaded'] = 1

" vim:foldmethod=marker
