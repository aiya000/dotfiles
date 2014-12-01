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
"       Initialize        "
"-------------------------"
" autcmd Groups {{{

augroup gui_prefs
	autocmd!
augroup END

"}}}


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

let &guifont     = g:gvimrc.guifont['font'] . g:gvimrc.guifont['size']
let &guifontwide = g:gvimrc.guifont['font'] . g:gvimrc.guifont['size']

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

"}}}


"-------------------------"
"          Alias          "
"-------------------------"
"{{{

command! GVimConfig    e $MYGVIMRC
command! GVimConfigTab tabnew | e $MYGVIMRC

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

augroup highlight_pref
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


if s:isUnix
	colorscheme molokai
elseif s:isWindows
	colorscheme evening
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


"-------------------------"
"        File_Types       "
"-------------------------"
"{{{

" Call matchadd when that file is target filetype
function! s:matchadd_with_filetype(ft, tag, regex) "{{{
	if &filetype == a:ft
		call matchadd(a:tag, a:regex)
	endif
endfunction "}}}

" Set for "Vi Improved"
augroup extension_type
	autocmd ColorScheme * highlight GrcMyHint gui=bold guifg=#ef5939
	autocmd VimEnter,WinEnter    * call s:matchadd_with_filetype('vim', 'GrcMyHint', '\s*"\zs@\w\+(.*)\ze')
augroup END

" Set for C-Sharp
augroup extension_type
	autocmd ColorScheme * highlight GrcTypeInference gui=none guifg=Cyan
	autocmd VimEnter,WinEnter    * call s:matchadd_with_filetype('cs', 'GrcTypeInference', '\<var\>')
augroup END

" Set for Haskell
augroup extension_type
	autocmd ColorScheme * highlight GrcHeadHfSpace gui=underline guifg=Cyan
	autocmd VimEnter,WinEnter    * call s:matchadd_with_filetype('haskell', 'GrcHeadHfSpace', '^\s\+')
augroup END

"}}}


syntax enable
let g:gvimrc['loaded'] = 1

