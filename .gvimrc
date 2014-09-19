scriptencoding utf8


"-------------------
"--  Recipe Menu  --
"-------------------
" -- Parameter
" -- Application Setting
" -- Functional Command
" -- Alias
" -- View Setting
" -- Plugin Configure
" ---
" Ideas {{{



" }}}
" Issues {{{



"}}}
" Todo {{{



" }}}



"-------------------------"
"        Parameter        "
"-------------------------"
"{{{

let s:isWindows = has('win32')

let g:gvimrc_loaded = get(g:, 'gvimrc_loaded', 0)

"}}}


"-------------------------"
"   Application Setting   "
"-------------------------"
"{{{

set guioptions-=T
set guioptions-=m

if s:isWindows
	set guifont=MS_Gothic:h10
	set guifontwide=MS_Gothic:h10
endif

"}}}


"-------------------------"
"   Functional Command    "
"-------------------------"
"{{{

command! DressUpColorEvening
\	colorscheme evening
\|	if s:isWindows
\|		set transparency=220
\|	endif

command! DressUpColorMolokai
\	colorscheme molokai
\|	if s:isWindows
\|		set transparency=255
\|	endif

"}}}


"-------------------------"
"          Alias          "
"-------------------------"
"{{{

command! GVimConfig e $MYGVIMRC

if s:isWindows
	command! HighTransparence set transparency=140
	command! Transparence     set transparency=180
	command! LowTransparence  set transparency=220
	command! NoTransparence   set transparency=255
endif

"}}}


"-------------------------"
"      View Setting       "
"-------------------------"
"{{{

augroup gui_highlight
	"@Bugs('Not Shown')
	autocmd VimEnter,BufWinEnter * syntax match     grcEmSpace /　/
	autocmd ColorScheme          * highlight grcEmSpace gui=standout guifg=White

	"@Bugs('?')
	autocmd VimEnter,BufWinEnter * syntax match grcHint /\s*"@\w\+/
	autocmd ColorScheme          *
	\	if &filetype == 'vim'
	\|		highlight    rcHint gui=bold guifg=#ef5939
	\|	endif

	autocmd ColorScheme * highlight CursorLine gui=underline guifg=cyan guibg=NONE
augroup END


colorscheme evening

"}}}


"------------------------"
"*** Plugin Configure ***"
"------------------------"
"--- TweetVim ---"{{{

let g:tweetvim_display_username = 1
let g:tweetvim_display_icon = 1

"}}}
"--- J6uil ---"{{{

let g:J6uil_display_icon = 1

"}}}


let g:gvimrc_loaded = 1

