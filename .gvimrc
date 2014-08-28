
let s:isWindows = has('win32') || has('win64')

if s:isWindows
	command! HighTransparence set transparency=140
	command! Transparence     set transparency=180
	command! LowTransparence  set transparency=220
	command! NoTransparence   set transparency=255
	set transparency=220
endif

augroup gui_highlight
	autocmd VimEnter,WinEnter * match     rcEmSpace /　/
	autocmd ColorScheme       * highlight rcEmSpace gui=standout guifg=White

	autocmd VimEnter,WinEnter * syntax match rcHint /.*"@\w\+/
	autocmd FileType vim        highlight    rcHint gui=bold guifg=#ef5939
augroup END
highlight CursorLine gui=underline guifg=cyan guibg=NONE

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

