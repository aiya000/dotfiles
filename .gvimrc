"---------"
" Startup "
"---------"
" Set encodings {{{

scriptencoding utf-8

" }}}


"----------------------"
" Define global values "
"----------------------"
" {{{

let g:gvimrc = get(g:, 'gvimrc', {})
let g:gvimrc['loaded'] = get(g:gvimrc, 'loaded', 0)

" }}}


"------------------"
" Set gvim options "
"------------------"
" {{{

" TODO: If Windows
set guifont=RictyDiminished\ NF\ 11

set guioptions-=T
set guioptions-=m
set guioptions-=e
set guioptions-=r
set guioptions-=L
set guioptions+=c
set winaltkeys=no
set mouse=

" }}}


"--------------"
" GUI Commands "
"--------------"
" {{{

function! s:open_this_file_in_new_window()
  let filepath = expand('%')
  execute ':bd' filepath
  execute ':!start' g:vimrc.gui_editor printf('"%s"', fnameescape(filepath))
endfunction

command! OpenThisFileInNewWindow call s:open_this_file_in_new_window()

" }}}

"------------------"
" Plugin_Configure "
"------------------"
" --- TweetVim --- {{{

let g:tweetvim_display_username = 1
let g:tweetvim_display_icon     = 1

" }}}
" --- J6uil --- {{{

let g:J6uil_display_icon = 1

" }}}

syntax enable
let g:gvimrc['loaded'] = 1
