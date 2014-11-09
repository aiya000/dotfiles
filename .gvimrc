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

let g:gvimrc = get(g:, 'gvimrc', {})
let g:gvimrc['loaded'] = get(g:gvimrc, 'loaded', 0)

let s:isUnix    = has('unix')
let s:isWindows = has('win32')

let g:gvimrc['guifont']      = get(g:gvimrc, 'guifont', {})
let g:gvimrc['guifont'].font = get(g:gvimrc, 'font', s:isWindows ? 'MS_Gothic' : 'Nimbus Mono L')
let g:gvimrc['guifont'].size = get(g:gvimrc, 'size', s:isWindows ? ':h10' : ' 10')

"}}}


"-------------------------"
"   Application_Setting   "
"-------------------------"
"{{{

set guioptions-=TmerL
set guioptions+=c
set winaltkeys=no

let &guifont     = g:gvimrc.guifont['font'] . g:gvimrc.guifont['size']
let &guifontwide = g:gvimrc.guifont['font'] . g:gvimrc.guifont['size']

"}}}


"-------------------------"
"   Functional_Command    "
"-------------------------"
"{{{

" DressUp kawaii-vim
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
	command! MidTransparence  set transparency=180
	command! LowTransparence  set transparency=220
	command! NoTransparence   set transparency=255
endif

"}}}


"-------------------------"
"       View_Setting      "
"-------------------------"
"{{{

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


let g:gvimrc['loaded'] = 1

