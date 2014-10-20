scriptencoding utf8


"-------------------
"--  Recipe Menu  --
"-------------------
" -- Parameter
" -- Application_Setting
" -- Functional_Command
" -- Alias
" -- View_Setting
" -- Plugin_Configure
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

let g:gvimrc_loaded = get(g:, 'gvimrc_loaded', 0)

let s:isUnix    = has('unix')
let s:isWindows = has('win32')

let g:grc_guifont      = get(g:, 'grc_guifont', {})
let g:grc_guifont.font = get(g:grc_guifont, 'font', s:isWindows ? 'MS_Gothic' : 'Monospace')
let g:grc_guifont.size = get(g:grc_guifont, 'size', s:isWindows ? ':h10' : ' 10')

"}}}


"-------------------------"
"   Application_Setting   "
"-------------------------"
"{{{

set guioptions-=T
set guioptions-=m
set winaltkeys=no

let &guifont     = g:grc_guifont['font'] . g:grc_guifont['size']
let &guifontwide = g:grc_guifont['font'] . g:grc_guifont['size']

"}}}


"-------------------------"
"   Functional_Command    "
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
	command! MidTransparence  set transparency=180
	command! LowTransparence  set transparency=220
	command! NoTransparence   set transparency=255
endif

"}}}


"-------------------------"
"       View_Setting      "
"-------------------------"
"{{{
"
augroup def_highlight
	"autocmd Colorscheme * highlight Normal       gui=NONE      guifg=Cyan
	"autocmd ColorScheme * highlight Visual       gui=underline guifg=White guibg=Cyan
	"autocmd ColorScheme * highlight IncSearch                  guifg=Black guibg=Cyan
	"autocmd ColorScheme * highlight Pmenu        gui=standout  guifg=Blue
	"autocmd ColorScheme * highlight PmenuSel                   guifg=Black guibg=White
	"autocmd ColorScheme * highlight TabLine      gui=standout  guifg=Blue
	"autocmd ColorScheme * highlight TabLineSel   gui=NONE      guifg=Cyan
	"autocmd ColorScheme * highlight TabLineFill  gui=standout  guifg=Blue
	"autocmd ColorScheme * highlight VertSplit    gui=NONE      guifg=Cyan  guibg=Blue
	"autocmd ColorScheme * highlight StatusLine                 guifg=Cyan  guibg=Black
	"autocmd ColorScheme * highlight StatusLineNC               guifg=Blue
	"autocmd ColorScheme * highlight LineNr                     guifg=Blue
	"autocmd ColorScheme * highlight CursorLine   gui=underline guifg=Cyan


	"@Bugs('Not Shown')
	autocmd VimEnter,BufWinEnter * syntax match GrcEmSpace /　/
	autocmd ColorScheme          * highlight GrcEmSpace guibg=White

	"@Bugs('okasiisi is if &filetype timing')
	autocmd VimEnter,BufWinEnter * syntax match GrcHint /\s*"@\w\+/
	autocmd ColorScheme          *
	\	if &filetype == 'vim'
	\|		highlight    GrcHint gui=bold guifg=#ef5939
	\|	endif

	autocmd ColorScheme * highlight CursorLine gui=underline guifg=cyan guibg=NONE
augroup END


colorscheme evening

"}}}


"------------------------"
"*** Plugin_Configure ***"
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

