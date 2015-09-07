"-------------------
"--  Recipe Menu  --
"-------------------
" -- Parameter
" -- Initialize
" -- Application_Setting
" -- Functional_Command
" -- Command_Utils
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
"       Initialize        "
"-------------------------"
" file encoding {{{

" Encoding for this script
scriptencoding utf8

" }}}


"-------------------------"
"        Parameter        "
"-------------------------"
"{{{

let g:gvimrc = get(g:, 'gvimrc', {})
let g:gvimrc['loaded'] = get(g:gvimrc, 'loaded', 0)

let s:is_unix    = has('unix')
let s:is_windows = has('win32')

let g:gvimrc['guifont']      = get(g:gvimrc, 'guifont', {})
let g:gvimrc['guifont'].font = get(g:gvimrc, 'font', s:is_windows ? 'MS_Gothic' : 'Andale Mono')
let g:gvimrc['guifont'].size = get(g:gvimrc, 'size', s:is_windows ? ':h10' : ' 10')

"}}}


"-------------------------"
"   Application_Setting   "
"-------------------------"
"{{{

set guioptions-=T
set guioptions-=m
set guioptions-=e
set guioptions-=r
set guioptions-=L
set guioptions+=c
set winaltkeys=no
set mouse=

if !g:gvimrc['loaded']
	let &guifont     = g:gvimrc.guifont['font'] . g:gvimrc.guifont['size']
	let &guifontwide = g:gvimrc.guifont['font'] . g:gvimrc.guifont['size']
endif

"}}}


"-------------------------"
"      Command_Utils      "
"-------------------------"
"{{{

command! -bar GVimConfig    e $MYGVIMRC
command! -bar GVimConfigTab tabnew $MYGVIMRC

"}}}


"-------------------------"
"       View_Setting      "
"-------------------------"
"{{{

augroup HighlightPrefs
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
	autocmd ColorScheme * highlight CursorLine gui=underline guifg=cyan guibg=NONE

	autocmd ColorScheme       * highlight GuiRcEmSpace guibg=White
	autocmd VimEnter,WinEnter * call matchadd('GuiRcEmSpace', '　')
augroup END


if !g:gvimrc['loaded']
	set background=dark

	if s:is_unix
		colorscheme molokai
	elseif s:is_windows
		colorscheme lucius
	endif
endif

"}}}


"------------------------"
"*** Plugin_Configure ***"
"------------------------"
"--- TweetVim ---"{{{

let g:tweetvim_display_username = 1
let g:tweetvim_display_icon     = 1

"}}}
"--- J6uil ---"{{{

let g:J6uil_display_icon = 1

"}}}
"--- vim-submode ---{{{

if s:is_windows
	augroup KeyMapping
		autocmd User MyGVimRc call submode#enter_with('trans_changer', 'n', '', '<C-s>*')
		autocmd User MyGVimRc call submode#map('trans_changer', 'n', '', 'j', ':let &transparency = <C-r>=&transparency<CR> + 10<CR>')
		autocmd User MyGVimRc call submode#map('trans_changer', 'n', '', 'k', ':let &transparency = <C-r>=&transparency<CR> - 10<CR>')
		autocmd User MyGVimRc call submode#map('trans_changer', 'n', '', 'H', ':set transparency=1<CR>')
		autocmd User MyGVimRc call submode#map('trans_changer', 'n', '', 'M', ':set transparency=127<CR>')
		autocmd User MyGVimRc call submode#map('trans_changer', 'n', '', 'L', ':set transparency=255<CR>')
	augroup END
endif

"}}}


syntax enable
doautocmd User MyGVimRc
let g:gvimrc['loaded'] = 1
