scriptencoding utf8

"-------------------
"--  Recipe Menu  --
"-------------------
" -- Parameter
" -- Initialize
" -- Application_Setting
" -- Functional_Command
" -- Command_Alias
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
let g:gvimrc['guifont'].font = get(g:gvimrc, 'font', s:isWindows ? 'MS_Gothic' : 'Andale Mono')
let g:gvimrc['guifont'].size = get(g:gvimrc, 'size', s:isWindows ? ':h10' : ' 10')

"}}}


"-------------------------"
"       Initialize        "
"-------------------------"
" autcmd Groups {{{

augroup GuiPrefs
	autocmd!
augroup END

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
"   Functional_Command    "
"-------------------------"
"{{{

" DressUp kawaii-vim
command! DressUpColorDesert
\	colorscheme desert
\|	if s:isWindows
\|		set transparency=245
\|	endif

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

command! DressUpColorSolarized
\	colorscheme solarized
\|	if s:isWindows
\|		set transparency=220
\|	endif

"}}}


"-------------------------"
"      Command_Alias      "
"-------------------------"
"{{{

command! GVimConfig    e $MYGVIMRC
command! GVimConfigTab tabnew $MYGVIMRC

if s:isWindows
	command! HighTransparence  set transparency=140
	command! MidTransparence   set transparency=180
	command! LowTransparence   set transparency=220
	command! LightTransparence set transparency=245
	command! NoTransparence    set transparency=255
endif

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

	autocmd ColorScheme       * highlight GrcEmSpace guibg=White
	autocmd VimEnter,WinEnter * call matchadd('GrcEmSpace', '　')
augroup END


if !g:gvimrc['loaded']
	set background=dark

	if s:isUnix
		colorscheme molokai
	elseif s:isWindows
		colorscheme solarized
	endif
endif

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
"--- vim-submode ---{{{

if s:isWindows
	augroup FileEvent
		autocmd FileType * call submode#enter_with('trans_changer', 'n', '', '<C-s>*')
		autocmd FileType * call submode#map('trans_changer', 'n', '', 'j', ':let &transparency = <C-r>=&transparency<CR> + 10<CR>')
		autocmd FileType * call submode#map('trans_changer', 'n', '', 'k', ':let &transparency = <C-r>=&transparency<CR> - 10<CR>')
	augroup END
endif

"}}}


"-------------------------"
"        File_Types       "
"-------------------------"
"{{{

" Call matchadd when that file is target filetype
function! s:matchadd_with_filetype(ft, tag, regex, priority, id) "{{{
	if &filetype == a:ft
		try
			let l:id = matchadd(a:tag, a:regex, a:priority, a:id)
		catch /\vE(799|801)/
			" Suppress repeate add
			let l:id = a:id
		endtry
	else
		try
			call matchdelete(a:id)
		catch /\vE(802|803)/
			" Suppress repeate delete
		endtry

		let l:id = a:id
	endif

	return l:id
endfunction "}}}

augroup FileEvent
	" Set for "Vi Improved"
	autocmd VimEnter,ColorScheme * highlight GrcMyHint gui=bold guifg=#ef5939
	"@Incomplete('These were not deleted')
	autocmd BufWinEnter          * let s:grcHint = s:matchadd_with_filetype('vim', 'GrcMyHint', '\s*"\zs@\w\+(.*)\ze', 10, get(s:, 'grcHint', 10101))

	" Set for Haskell
	autocmd VimEnter,ColorScheme * highlight GrcHeadHfSpace gui=underline guifg=Cyan
	autocmd BufWinEnter          * let s:grcHeadHfSpace = s:matchadd_with_filetype('haskell', 'GrcHeadHfSpace', '^\s\+', 10, get(s:, 'grcHeadHfSpace', 10102))

	" Set for C-Sharp
	autocmd VimEnter,ColorScheme * highlight default link GrcTypeInference Identifier
	autocmd VimEnter,WinEnter    *.cs syntax keyword GrcTypeInference var
augroup END

"}}}


syntax enable
let g:gvimrc['loaded'] = 1

