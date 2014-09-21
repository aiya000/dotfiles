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

let g:grc_guifont = get(g:, 'grc_guifont', {})
let g:grc_guifont.font = get(g:grc_guifont, 'font', s:isWindows ? 'MS_Gothic' : 'Monospace')
let g:grc_guifont.size = get(g:grc_guifont, 'size', s:isWindows ? ':h10' : ' 10')

"}}}


"-------------------------"
"   Application Setting   "
"-------------------------"
"{{{

set guioptions-=T
set guioptions-=m

"if s:isWindows
"	set guifont=MS_Gothic:h10
"	set guifontwide=MS_Gothic:h10
"endif
let &guifont     = g:grc_guifont['font'] . g:grc_guifont['size']
let &guifontwide = g:grc_guifont['font'] . g:grc_guifont['size']

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


command! FontSizeUp
	\	let g:grc_guifont['size'] += 1
	\|	let &guifont     = g:grc_guifont['font'] .':h'. g:grc_guifont['size']
	\|	let &guifontwide = g:grc_guifont['font'] .':h'. g:grc_guifont['size']

command! FontSizeDown
	\	let g:grc_guifont['size'] -= 1
	\|	let &guifont     = g:grc_guifont['font'] .':h'. g:grc_guifont['size']
	\|	let &guifontwide = g:grc_guifont['font'] .':h'. g:grc_guifont['size']

command! FontSizeDefault
	\	let g:grc_guifont['size'] = 10
	\|	let &guifont     = g:grc_guifont['font'] .':h'. g:grc_guifont['size']
	\|	let &guifontwide = g:grc_guifont['font'] .':h'. g:grc_guifont['size']


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
"--- submode ---"{{{

call submode#enter_with('font_size', 'n', '', '<C-s>f', '<NOP>')
call submode#map('font_size', 'n', '', ';', ':FontSizeUp<CR>')
call submode#map('font_size', 'n', '', '-', ':FontSizeDown<CR>')
call submode#map('font_size', 'n', '', '=', ':FontSizeDefault<CR>')

"}}}


let g:gvimrc_loaded = 1

