
let s:isWindows = has('win32')

if s:isWindows
	command! HighTransparence set transparency=140
	command! Transparence     set transparency=180
	command! LowTransparence  set transparency=220
	command! NoTransparence   set transparency=255
	set transparency=220  "@Bugs('not set at start up gvim')
endif

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


let g:tweetvim_display_username = 1
let g:tweetvim_display_icon = 1
let g:J6uil_display_icon = 1

command! GVimConfig e $MYGVIMRC

