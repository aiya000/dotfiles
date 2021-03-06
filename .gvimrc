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

let g:vimrc['guifont'] = #{
  \ size: 9,
\ }
let &guifont = 'RictyDiminished NF ' .. g:vimrc.guifont.size

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


"----------"
" Augroups "
"----------"
" {{{

augroup gvimrc
  autocmd!
augroup END

" }}}


"---------------"
" Local scripts "
"---------------"
" {{{

if filereadable($'${$HOME}/.gvimrc_env')
  source ~/.gvimrc_env
endif

" }}}


"---------"
" Plugins "
"---------"
" TweetVim {{{

let g:tweetvim_display_username = 1
let g:tweetvim_display_icon     = 1

" }}}
" J6uil {{{

let g:J6uil_display_icon = 1

" }}}
" vim-suvmode {{{

call submode#enter_with('font_size', 'n', 's', '<C-s>+', ':<C-u>call vimrc#increment_gui_fontsize()<CR>')
call submode#enter_with('font_size', 'n', 's', '<C-s>-', ':<C-u>call vimrc#decrement_gui_fontsize()<CR>')
call submode#map('font_size', 'n', 's', '+', ':<C-u>call vimrc#increment_gui_fontsize()<CR>')
call submode#map('font_size', 'n', 's', '-', ':<C-u>call vimrc#decrement_gui_fontsize()<CR>')

" }}}
" vim-cursorword {{{

augroup gvimrc
  autocmd VimEnter,ColorScheme * highlight CursorWord0 gui=bold guibg=#66cdaa guifg=#006400
  autocmd VimEnter,ColorScheme * highlight CursorWord1 gui=bold guibg=#66cdaa guifg=#191970
augroup END

" }}}

syntax enable
let g:gvimrc['loaded'] = 1

" vim:foldmethod=marker
