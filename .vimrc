scriptencoding utf8

"----------------------------"
"  Target of this config     "
"    - UNIX like OS          "
"    - Cygwin                "
"    - Kaoriya Vim           "
"----------------------------"
"{{{

"---------------------------------------"
"*** Verified Functioned Environment ***"
"---------------------------------------"

" --- Windows Kaoriya GVim --- {{{

" 1. New Version download by Kaoriya site
" 2. Reference this config file
" 3. Starting GVim
" -> Completely Succeeded.


" }}}

"}}}


"-------------------
"--  Recipe Menu  --
"-------------------
" -- Parameter
" -- Local Function
" -- Initialize
" -- Plugin Manage
" -- Plugin Configure
" -- View Setting
" -- Action Setting
" -- Inner Setting
" -- Event Method
" -- Function Command
" -- Alias
" -- Global KeyMap
" -- FileType
" -- Ignore Setting
" -- Environment Pref
" ---
" Ideas {{{

"-- gJ,gK => zh,zl like scroll up and down

"-- (*^-') < vimshell-kawaii kakuremi no jutsu !!

"-- point marker line num in a file
"  -- I can jump marked line and list up mark lines

"-- implement command that print format (%0, %1, %2), ({0}, {1}, {2}) replace arguments on real time

"-- Implement Local vimgrep :Grep
"  -- correspond for unnamed buffer

"-- Translate English Plugin By Weblio

"-- Implement a command, Englishnize selected lines by QuickRun 'en'

"-- Uniteでencoding変えたい…。

"-- Easy CaslII Emulator for Vim

" }}}
" Issues {{{

"-- C-o hard use when vimshell

"-- automatic mkdir './C:' when execute NeoBundleInstall in windows kaoriya

"-- color highlight 'var' is not highlight when ...executed vsp|b hoge..?
"  -- What is best event ?

"-- My Hints highlight faild in kaoriya gvim
"  -- and Cygwin Vim

"-- [autocmd FileType * nnoremap <C-j> :normal o] Applied tweetvim buffer
"  -- want to not functioned this map in that buffer

"-- not functioned ./doc/myhelp.txt

"-- 'gist:aiya000/ec5f6b2375a639831953' cannot divide configure
"  -- if diveding then install it as no named plugin

"-- Do filetype not found when enter filetype ?
"  -- mmm, this take by filetype text ?

"}}}
" Todo {{{

"-- set paste option added after checking operate.

"-- <C-~> is split window size equalize

" }}}



"----------------------------------------
" {- Hints -} "
" @Unsupported  => Do not supported functions when now.
" @Incubated    => I don't know why this functioned.
"     ／人◕ ‿‿ ◕人＼ <  Wakega wakaranaiyo!
" @Incompleted  => This is not completed making.
" @Deprecated   => Deprecated This vimrc Version.
" @Bugs         => This hoge has the bugs.
" @Unused       => Not used this yet now, needs inquires deleting this.
" @Unchecked    => This was not unchecked that is operate right
" @See          => Referred URL, Saw Document, and etc...
" @Code         => A sample code using it
"-------------------
" Designating the target platform.
" @Hoge{Win|Ubuntu}  : This Hint for Win and Ubuntu.
" @Hoge!{Mac}        : This Hint for other than Mac.
"----------------------------------------


"---------------------"
"      Parameter      "
"---------------------"
"{{{

let s:isWindows = has('win32')
let s:isCygwin  = has('win32unix')
let s:isKaoriya = has('kaoriya')
let s:isDosWin  = s:isWindows && !s:isCygwin &&!s:isKaoriya
let s:isUnix    = has('unix')
let s:isMac     = has('mac')

let s:hasCygwin = isdirectory('/cygwin/bin')
let s:hasMingw  = 0

let s:vimHome   = expand('~/.vim')

let s:backupdir = expand('~/.backup/vim_backup')
let s:directory = s:backupdir.'/swp/'
let s:undodir   = s:backupdir.'/undo/'
let s:viewdir   = s:backupdir.'/view/'

let s:username  = expand('$USER')
let s:groupname = $GROUP == '' ? expand('$USER') : expand('$GROUP')

if !exists('g:vimrc_loaded')
	let g:vimrc_loaded = 0
endif

"}}}


"---------------------"
"    Local Function   "
"---------------------"
"{{{

function! s:system(cmd)
	if exists(':VimProcBang')
		return vimproc#system(a:cmd)
	else
		return system(a:cmd)
	endif
endfunction


function! s:rm(file)
	if s:isUnix
		execute '!rm '.a:file
	else
		execute '!del '.substitute(a:file, "/", "\\", 'g')
	endif
endfunction

"}}}


"-------------------------"
"       Initialize        "
"-------------------------"
filetype plugin indent on
" For Support Kaoriya Vim {{{

if s:isKaoriya

	" Set Environment
	let s:vimHome = $VIM.'/_vim'
	let &runtimepath = &runtimepath.','.s:vimHome
	let $HOME = $VIM
	if s:hasCygwin
		let $PATH = '/cygwin/bin;/cygwin/usr/bin;/cygwin/usr/sbin;'.$PATH
		let $PATH = $HOME.'/bin;'.$PATH
	endif


	" Build Base Directories
	if !isdirectory(s:vimHome)
		call mkdir(s:vimHome)
	endif


	" For Using No Default vimproc
	"@Bugs('do not loaded vimproc')
	let suppress = $VIM.'/switches/enabled/disable-vimproc.vim'
	if s:isWindows && !s:hasMingw && filereadable(suppress)
		call s:rm(suppress)
	elseif s:isWindows && s:hasMingw && !filereadable(suppress)
		call writefile([], suppress)
	endif
	unlet suppress


	" Unset Kaoriya Preference
	set noignorecase
	set nosmartcase
endif

"}}}
" Check NeoBundle exists {{{
let s:bundleDir    = s:vimHome.'/bundle/'
let s:neobundleDir = s:bundleDir.'/neobundle.vim/'

if !isdirectory(s:bundleDir)
	call mkdir(s:bundleDir)
endif

function! s:remove_empty_bundledir()  "{{{
	let dirs = split(s:system('ls '.s:bundleDir), '\n')
	for dir in dirs
		let pluginDir = s:bundleDir.'/'.dir
		let isEmpty = s:system('ls '.pluginDir) ==# ''
		if isEmpty
			call s:system('rmdir '.pluginDir)
		endif
	endfor
endfunction  "}}}
function! s:fetch_neobundle()  " {{{
	if executable('git')
		echo 'NeoBundle was not installed...'
		echo 'Installing NeoBundle.'

		execute '!git clone http://github.com/Shougo/neobundle.vim '.s:neobundleDir
		return
	else
		echohl Error
		echo 'Sorry, You do not have git command.'
		echo 'Cannot introduce NeoBundle.''
		echohl None
		throw "neobundle.vim clone failed."
	endif
endfunction  " }}}

if has('vim_starting')
	try
		let &runtimepath = &runtimepath.','.s:vimHome.'/bundle/neobundle.vim'
		" Throws Error when nothing neobundle in runtime path
		call neobundle#begin(expand(s:bundleDir))
	catch
		if isdirectory(s:neobundleDir) && !exists(':NeoBundle')
			" Plugin Directories may be empty when git cloned new.
			call s:remove_empty_bundledir()
			echo 'bundle directories initialized.'
		endif
		try
			call s:fetch_neobundle()
			call neobundle#begin(expand(s:bundleDir))
			echo 'NeoBundle installed.'
			echo 'Please closing vim and reopening vim once,'
			echo 'and executing :NeoBundleInstall .'
		catch
			echohl Error
			echo "neobundle.vim clone failed."
			echo ">> Vim Config Error <<"
			echohl None
		endtry
	endtry
endif

unlet s:neobundleDir
unlet s:bundleDir
"}}}
" autocmd Groups {{{

augroup PluginPrefs | autocmd!
augroup END

augroup FileEvents | autocmd!
augroup END

augroup FilePositSave | autocmd!
augroup END

augroup ProgramTypes | autocmd!
augroup END

augroup SyntaxHighlights | autocmd!
augroup END

augroup AddtionalKeys | autocmd!
augroup END

"}}}
" Check Backup, Swap and Undo directory exists {{{

if !isdirectory(s:backupdir)
	call mkdir(s:backupdir, 'p', 0755)
	call s:system(printf('chown -R %s:%s %s', s:username, s:groupname, s:backupdir))
endif

if !isdirectory(s:directory)
	call mkdir(s:directory, 'p', 0755)
	call s:system(printf('chown -R %s:%s %s', s:username, s:groupname, s:directory))
endif

if !isdirectory(s:undodir)
	call mkdir(s:undodir, 'p', 0755)
	call s:system(printf('chown -R %s:%s %s', s:username, s:groupname, s:backupdir))
endif

"}}}


"-------------------------"
"     Plugin Manage       "
"-------------------------"
"*** Plugin List ***"{{{

NeoBundleFetch 'Shougo/neobundle.vim'
NeoBundle 'basyura/twibill.vim'
NeoBundle 'tyru/open-browser.vim'
NeoBundle 'basyura/bitly.vim'
NeoBundle 'Shougo/unite.vim'
NeoBundle 'Shougo/vimproc.vim'
NeoBundle 'basyura/TweetVim'
NeoBundle 'mattn/webapi-vim'
NeoBundle 'Shougo/vimshell.vim'
NeoBundle 'rhysd/wandbox-vim'
NeoBundle 'thinca/vim-quickrun'
NeoBundle 'osyo-manga/quickrun-outputter-replace_region'
NeoBundle 'yuratomo/gmail.vim'
NeoBundle 'basyura/J6uil.vim'
NeoBundle 'osyo-manga/vim-gyazo'
NeoBundle 'yuratomo/w3m.vim'
NeoBundle 'mattn/learn-vimscript'
NeoBundle 'rbtnn/vimconsole.vim'
NeoBundle 'jimsei/winresizer'
NeoBundle 'add20/vim-conque'
NeoBundle 'supermomonga/thingspast.vim'
NeoBundle 'supermomonga/vimshell-kawaii.vim'
NeoBundle 'mattn/excitetranslate-vim'
NeoBundle 'kana/vim-altercmd'
NeoBundle 'mattn/unite-advent_calendar'
NeoBundleLazy 'thinca/vim-splash'
NeoBundle 'supermomonga/jazzradio.vim'
NeoBundle 'mattn/favstar-vim'
NeoBundle 'ujihisa/unite-colorscheme'
NeoBundle 'Shougo/vinarise.vim'
NeoBundle 'mattn/gist-vim'
NeoBundle 'vim-scripts/Align'
NeoBundle 'vim-scripts/postmail.vim'
NeoBundle 'rbtnn/rabbit-ui.vim'
NeoBundle 'thinca/vim-ref'
NeoBundle 'ujihisa/ref-hoogle'
NeoBundle 'rbtnn/rabbit-ui-collection.vim'
NeoBundleLazy 'vim-jp/vital.vim'
NeoBundle 'Shougo/unite-outline'
NeoBundle 'ebc-2in2crc/vim-ref-jvmis'
NeoBundleLazy 'rbtnn/puyo.vim'
NeoBundleLazy 'mattn/benchvimrc-vim'
NeoBundle 'tacroe/unite-alias'
NeoBundle 'mattn/ideone-vim'
NeoBundleLazy 'mattn/yamada-vim'
NeoBundleLazy 'jvoorhis/coq.vim'
NeoBundle 'eagletmt/coqtop-vim'
NeoBundle 'duff/vim-scratch'
NeoBundle 'rhysd/vim-grammarous'
NeoBundleLazy 'thinca/vim-themis'
NeoBundle 'tomasr/molokai'
NeoBundle 'soramugi/auto-ctags.vim'
NeoBundle 'aiya000/ahoge-put.vim'
NeoBundle 'kannokanno/previm'
NeoBundle 'gist:aiya000/ec5f6b2375a639831953', {
\	'name'        : 'gitlogviewer.vim',
\	'script_type' : 'plugin'
\}

call neobundle#end()
"NeoBundleCheck
"}}}
"*** Plugin Depends and Auto Config ***" {{{

let vimproc_config = {
\	'build' : {
\		'unix'    : 'make -f make_unix.mak',
\		'mac'     : 'make -f make_mac.mak'
\	}
\}
if s:isCygwin
	let vimproc_config.build["cygwin"]  = 'make -f make_cygwin.mak'
elseif s:hasMingw
	let vimproc_config.build["windows"] = 'make -f make_mingw32.mak'
endif
call neobundle#config('vimproc.vim', vimproc_config)
unlet vimproc_config

call neobundle#config('TweetVim', {
\	'depends' : [
\		'basyura/twibill.vim',
\		'tyru/open-browser.vim',
\		'h1mesuke/unite-outline',
\		'basyura/bitly.vim',
\		'Shougo/unite.vim',
\		'Shougo/vimproc.vim',
\		'mattn/favstar-vim'
\	],
\	'autoload' : {'on_source' : ['vimproc.vim']}
\})
call neobundle#config('vimshell.vim', {
\	'depends' : ['Shougo/vimproc.vim']
\})
call neobundle#config('quickrun-outputter-replace_region', {
\	'depends' : ['thinca/vim-quickrun']
\})
call neobundle#config('gmail.vim', {
\	'depends' : ['Shougo/vimproc.vim']
\})
call neobundle#config('J6uil.vim', {
\	'depends' : [
\		'mattn/webapi-vim',
\		'Shougo/vimproc.vim',
\		'tyru/open-browser.vim',
\		'Shougo/unite.vim'
\	]
\})
call neobundle#config('vim-gyazo', {
\	'depends' : [
\		'tyru/open-browser.vim',
\		'basyura/TweetVim'
\	]
\})
call neobundle#config('vimshell-kawaii.vim', {
\	'depends'  : ['Shougo/vimshell.vim'],
\	'autoload' : {'on_source' : ['vimshell.vim']}
\})
call neobundle#config('unite-advent_calendar', {
\	'depends' : ['h1mesuke/unite-outline']
\})
call neobundle#config('vim-splash', {
\	'autoload' : {'commands' : ['Splash']}
\})
call neobundle#config('jazzradio.vim', {
\	'depends'  : ['Shougo/unite.vim'],
\})
call neobundle#config('ref-hoogle', {
\	'depends'  : ['thinca/vim-ref'],
\})
call neobundle#config('vital.vim', {
\	'autoload' : {'commands' : ['VitalOn']}
\})
call neobundle#config('puyo.vim', {
\	'autoload' : {'commands' : ['Puyo']}
\})
call neobundle#config('benchvimrc-vim', {
\	'autoload' : {'commands' : ['BenchVimrc']}
\})
call neobundle#config('unite-alias', {
\	'depends' : ['Shougo/unite.vim']
\})
call neobundle#config('yamada-vim', {
\	'autoload' : {'commands' : ['Yamada']}
\})
call neobundle#config('coq.vim', {
\	'autoload' : {'commands' : ['FtCoqInstancyOn']}
\})
call neobundle#config('coqtop-vim', {
\	'depends' : ['Shougo/vimproc.vim']
\})
call neobundle#config('vim-grammarous', {
\	'disabled' : !executable('java')
\})

" }}}


"------------------------"
"*** Plugin Configure ***"
"------------------------"
"--- vim-quickrun ---" {{{

let g:quickrun_config = {
\	'_' : {
\		'split'  : '',
\		'runner' : 'vimproc',
\		'runner/vimproc/updatetime' : 10,
\		'hook/time/enable' : 1,
\	},
\	'cpp' : {
\		'command' : 'g++',
\		'cmdopt'  : '-I/usr/include/c++/4.9 -I/usr/include/c++/4.9/x86_64-linux-gnu -std=c++11',
\	},
\	'java' : {
\		'cmdopt' : '-source 1.8'
\	}
\}

if s:isUnix
	let g:quickrun_config['cs'] = {
	\	'command'  : 'gmcs',
	\	'exec'     : ['%c %o %s:p > /dev/null', 'mono %s:p:r.exe', 'rm %s:p:r.exe'],
	\	'tempfile' : '{tempname()}.cs'
	\}
elseif s:isCygwin
	let g:quickrun_config['cs'] = {
	\	'command' : 'cocot csc.exe',
	\	'exec'     : ['%c %o %s:p > /dev/null', './%s:p:r.exe', 'rm %s:p:r.exe'],
	\	'tempfile' : '{tempname()}.cs'
	\}
elseif s:isWindows
	let g:quickrun_config['cs'] = {
	\	'command' : 'csc.exe',
	\	'exec'     : ['%c %o %s:p', '%s:p:r.exe', 'rm %s:p:r.exe'],
	\	'tempfile' : '{tempname()}.cs'
	\}
endif

if s:isCygwin
	let g:quickrun_config['java'] = {
	\	'command' : 'javac',
	\	'exec'    : ['%c %o `echo %s | sed s:¥:/:g | cygpath -w -f -`', '%c %s:t:r %a'],
	\	'hook/output_encode/encoding': 'Shift_JIS',
	\}
	let javav = s:system('java -version')
	if javav =~# '1\.8'
		let g:quickrun_config.java['cmdopt'] = '-source 1.8 -encoding UTF-8'
	elseif javav =~# '1\.7'
		let g:quickrun_config.java['cmdopt'] = '-source 1.7 -encoding UTF-8'
	else
		let g:quickrun_config.java['cmdopt'] = '-encoding UTF-8'
	endif
	unlet javav

	let g:quickrun_config['haskell'] = {
	\	'command' : 'ghc',
	\	'exec'    : 'ghc %s',
	\}
endif

augroup PluginPrefs
	autocmd FileType quickrun setlocal wrap
augroup END

" }}}
"--- vimproc.vim ---"{{{

if s:isWindows && !s:hasMingw
	"NeoBundleDisable 'Shougo/vimproc.vim'
	"@Incompleted('I should use a like NeoBundleDisable')
	set runtimepath-=~/_vim/bundle/vimproc.vim/
endif

" }}}
"--- TweetVim ---"{{{

let g:tweetvim_async_post = 1
augroup PluginPrefs
	autocmd FileType tweetvim setlocal wrap
augroup END

"}}}
"--- vimshell.vim ---"{{{

" Add to VimShell Commands Directory of My Home
let &runtimepath = &runtimepath.','.s:vimHome.'/autoload/vimshell/commands'

"let g:vimshell_user_prompt = 'fnamemodify(getcwd(), ":~")'
let g:vimshell_no_save_history_commands = {
\	'history': 1,
\	'ls'     : 1,
\	'clear'  : 1
\}

" Suteki Shell
let g:vimshell_enable_transient_user_prompt = 1
let g:vimshell_force_overwrite_statusline = 1

augroup PluginPrefs
	autocmd FileType vimshell
	\	call vimshell#altercmd#define('thanks', "echo \"(*^o^)< You're welcome!\"")
	\|	call vimshell#set_alias('sp',  ':sp  | VimShellCreate')
	\|	call vimshell#set_alias('vsp', ':vsp | VimShellCreate')

	autocmd FileType vimshell  set fdm=marker
augroup END

"}}}
"--- vimshell-kawaii.vim ---"{{{

let g:vimshell_kawaii_smiley = 1

"}}}
"--- w3m.vim ---"{{{

"let g:w3m#external_browser = 'firefox'
let g:w3m#homepage = 'http://www.google.co.jp/'

"}}}
"--- vimconsole.vim ---"{{{

let g:vimconsole#auto_redraw = 1

"}}}
"--- vim-conque ---"{{{

let g:ConqueTerm_CloseOnEnd = 1
let g:ConqueTerm_SessionSupport = 1
let g:ConqueTerm_ReadUnfocused = 1
let g:ConqueTerm_Color = 1
let g:ConqueTerm_InsertOnEnter = 0
let g:ConqueTerm_StartMessages = 1

"}}}
"--- jazzradio.vim ---"{{{

"@See sugoi momonga blog
if neobundle#tap('jazzradio.vim')
	call neobundle#config({
	\	'autoload' : {
	\		'unite_sources' : ['jazzradio'],
	\		'commands'      : [
	\			'JazzradioUpdateChannels',
	\			'JazzradioStop', {
	\				'name'     : 'JazzradioPlay',
	\				'complete' : 'customlist,jazzradio#channel_id_comlete'
	\			}
	\		],
	\		'function_prefix' : 'Jazzradio'
	\	}
	\})
endif

"}}}
"--- unite-alias ---"{{{

" :Unite javasrc
let g:unite_source_alias_aliases = {
\	'javasrc' : {
\		'source' : 'file_rec',
\		'args'   : '~/Documents/workspace/Java/src',
\	},
\}

"}}}
"--- For Private ---"{{{

" Read Privacy Config
if filereadable(expand('~/.vimrc_private'))
	source ~/.vimrc_private
endif

"}}}

"-------------------------"
"      View Setting       "
"-------------------------"
"{{{

" Status Bar always displayed
set laststatus=2

" Status Bar format $ @See http://sourceforge.jp/magazine/07/11/06/0151231
set statusline=%F%m\%=[FileType=%y][Format=%{&ff}]

" Wrap Line
set wrap

" View line number
set number

" Indent Width
set tabstop=4

" Highlight Hit Keyword
set hlsearch

" ☆Fix View 2byte Code (Not support gnore-terminal)
set ambiwidth=double

" One More Set for 2byte Code !!
syntax sync fromstart

" Syntax Highlight On
syntax on

" Visualize Tab and Space
set list
if &encoding == 'cp932'  "@Unchecked('Is this suitable condition ?')
	set listchars=tab:>_,trail:_,extends:>,precedes:<,nbsp:%
else
	set listchars=tab:»_,trail:_,extends:»,precedes:«,nbsp:%
endif

" Powered Up Syntax Highlight
" {{{

augroup SyntaxHighlights
	"autocmd Colorscheme * highlight Normal       cterm=NONE      ctermfg=Cyan
	autocmd ColorScheme * highlight Visual       cterm=underline ctermfg=White ctermbg=Cyan
	autocmd ColorScheme * highlight IncSearch                    ctermfg=Black ctermbg=Cyan
	autocmd ColorScheme * highlight Pmenu        cterm=standout  ctermfg=Blue
	autocmd ColorScheme * highlight PmenuSel                     ctermfg=Black ctermbg=White
	autocmd ColorScheme * highlight TabLine      cterm=standout  ctermfg=Blue
	autocmd ColorScheme * highlight TabLineSel   cterm=NONE      ctermfg=Cyan
	autocmd ColorScheme * highlight TabLineFill  cterm=standout  ctermfg=Blue
	autocmd ColorScheme * highlight VertSplit    cterm=NONE      ctermfg=Cyan ctermbg=Blue
	autocmd ColorScheme * highlight StatusLine                   ctermfg=Cyan ctermbg=Black
	autocmd ColorScheme * highlight StatusLineNC                 ctermfg=Blue
	autocmd ColorScheme * highlight LineNr                       ctermfg=Blue
	autocmd ColorScheme * highlight CursorLine   cterm=underline ctermfg=Cyan

	autocmd VimEnter,BufWinEnter * match     rcEmSpace /　/
	autocmd ColorScheme * highlight rcEmSpace cterm=standout ctermfg=LightBlue

	"@Incompleted('not functioned'){Ubuntu:vim_7.4.427}
	autocmd VimEnter,BufWinEnter * match     rcMyHint /"@\w\+/
	autocmd ColorScheme * highlight rcMyHint cterm=standout ctermfg=Red
augroup END

augroup SyntaxHighlights
	autocmd InsertEnter * highlight StatusLine ctermfg=Black ctermbg=Cyan
	autocmd InsertLeave * highlight StatusLine ctermfg=Cyan  ctermbg=Black
augroup END

" }}}

" Set Color Scheme
colorscheme desert

" Indent Wrapped Text
if exists('+breakindent')
	set breakindent
	set linebreak
endif

" Invisible character visualize to hex
"set display=uhex

"}}}


"-------------------------"
"     Action Setting      "
"-------------------------"
"{{{

" Set Compatibility with vi is off
set nocompatible

" Set Backspace can delete empty line
if v:version < 704  " Is this suitable condition ?
	set whichwrap=b,s,h,l,<,>,[,]
	set backspace=indent,eol,start
	"noremap 
	"noremap! 
endif

" Auto new line enter off
set textwidth=0

" Auto Indent on
set autoindent

" Regex Search Engine Type[1]
"   0: if search new engine is failure then search old engine [default]
"   1: search old engine only
"   2: search new engine only
"set regexpengine=0

" Incremental Searching
set incsearch

" Do not return file top when completed searching
"set nowrapscan

" Tag Jump Quickly
"set tagbsearch

" C Type Auto Indent
set cindent

" Indent Width when auto indent
set shiftwidth=4

" Fold Text with '{{{' and '}}}'
set foldmethod=marker

" Collection Swap File
let &directory = s:directory

" Hold View Position when file closed
let &viewdir = s:viewdir

" Hold Undo Archive when file closed
if has('persistent_undo')
	set undofile
	let &undodir = s:undodir
endif

" Bell Sound is instead of Screen flash.
set visualbell

"}}}


"-------------------------"
"     Inner Setting       "
"-------------------------"
"{{{

" Default File Encoding
set fileencoding=utf-8

" Leaving a history and it limit is a 50 pieces
set history=50

" Adding Runtime Path
set runtimepath+=~/.vim/vimball
set runtimepath+=~/.vim/makes/arot13.vim
set runtimepath+=~/.vim/makes/ahoge-put.vim

" Set Vimball Install place
let g:vimball_home = s:vimHome.'/vimball'

" Display Command Complement
set wildmenu
"set wildmode=list

" Path Delimiter is Slash
set shellslash

" Add Match Pairs
set matchpairs+=<:>

" Load Target for ctags
set tags=./tags,~/tags

" Explore wake up default dir
set browsedir=buffer

" Auto Judge file encode
if !g:vimrc_loaded
	let &fileencodings = 'utf-8,ucs-bom,iso-2022-jp-3,iso-2022-jp,eucjp-ms,euc-jisx0213,euc-jp,sjis,cp932,' . &fileencodings
endif


"}}}


"-------------------------"
"      Event Method       "
"-------------------------"
"{{{

augroup FileEvents
	" Auto Reload when save this file
	autocmd BufWritePost $MYVIMRC  source $MYVIMRC
	autocmd BufWritePost $MYGVIMRC source $MYGVIMRC
augroup END

" Save Cursor Position when file closed
try
	augroup FilePositSave
		autocmd BufWinLeave ?* silent mkview
		autocmd BufWinEnter ?* silent loadview
	augroup END
catch /E32/
	echo 'File Name is Nothing'
catch
	echo 'View Error'
endtry

" Powered Auto File Backup when written
set nobackup
function! s:update_backup_by_date() "{{{
	let l:dailydir = s:backupdir . '/' . strftime("%Y-%m-%d")
	if !isdirectory(l:dailydir)
		call mkdir(l:dailydir, 'p', 0755)
		call s:system(printf('chown -R %s:%s %s', s:username, s:groupname, l:dailydir))
	endif

	let filepath = split(expand('%'), '/')
	let filename = filepath[len(filepath)-1] . strftime('_at_%H:%M')
	execute 'w! '.l:dailydir.'/'.filename
endfunction "}}}
augroup FileEvents
	autocmd BufWritePre ?* silent call s:update_backup_by_date()
augroup END

"}}}

"-------------------------"
"   Functional Command    "
"-------------------------"
" Utility Function {{{

" Revese string of current line
function! s:reverse_line()  " {{{
	let l:reverse = ""
	let l:str = getline('.')
	let l:len = strlen(l:str)
	for i in range(1, l:len)
		let l:reverse .= l:str[l:len - i]
	endfor
	let l:reverse .= "\n"

	let l:r = @"
	execute 'normal dd'
	let @" = l:reverse
	execute 'normal P'
	let @" = l:r
endfunction  " }}}
command! ReverseLine call s:reverse_line()


" Revese Ranged Lines
function! s:tac_line() range " {{{
	if executable('tac')
		execute "'<,'>!tac"
	else
		if a:firstline == a:lastline
			return
		endif
		let lines = []
		let posit = getpos('.')
		for line in range(a:firstline, a:lastline)
			call add(lines, substitute(getline(line)."\n", "\t", "", 'g'))
		endfor

		for i in range(a:firstline, a:lastline)
			execute 'normal "_dd'
			execute 'normal i' . lines[a:lastline - i]
		endfor
		call setpos('.', posit)
	endif
endfunction " }}}
command! -range=%
\	Tac :<line1>, <line2> call s:tac_line()


" Catenate and echo files
function! s:cat_file(...) "{{{
	let l:catenate = ""
	if executable('cat')
		for filePath in a:000
			let l:catenate .= s:system('cat '.filePath)
		endfor
	else
		for filePath in a:000
			let l:catenate .= join(readfile(filePath), "\n")
		endfor
	endif

	echo l:catenate
endfunction "}}}
command! -nargs=* Cat call s:cat_file(<f-args>)


" Move Cursor Line's Center
command! CursorCenter
\	execute 'normal 0' |
\	for i in range(strlen(getline('.'))/2) | execute 'normal l' | endfor


" Conditions for Different Actions
if s:isWindows  " is different operated sp ubuntu and kaoriya?
	command! ScratchUp  execute ':Scratch' | execute ':resize 5'
else
	function! ScratchUpByCondition()
		if !&modified
			execute ':sp|Scratch' | execute ':resize 5'
		else
			execute ':Scratch' | execute ':resize 5'
		endif
	endfunction
	command! ScratchUp  call ScratchUpByCondition()
endif


" Low accuracy randome integer
function! s:random_int(max)"{{{
	let l:matchEnd = matchend(reltimestr(reltime()), '\d\+\.') + 1
	return reltimestr(reltime())[l:matchEnd :] % (a:max + 1)
endfunction"}}}
command! -nargs=1 RandomPut execute 'normal a' . s:random_int(<q-args>) )


" For Movement in a Indent Block
" {{{

function! s:up_cursor_lid() "{{{
	if &ft == 'netrw' | return | endif

	while 1
		let l:p = getpos('.')[2]
		execute 'normal k'

		let l:isIndentChanged = l:p != getpos('.')[2]
		if l:isIndentChanged || getpos('.')[1] == 1
			if l:isIndentChanged | execute 'normal j' | endif
			break
		endif
	endwhile
endfunction "}}}
command! UpCursorLid call s:up_cursor_lid()

function! s:down_cursor_ground() "{{{
	if &ft == 'netrw' | return | endif

	let l:eol = len(readfile(@%))
	while 1
		let l:p = getpos('.')[2]
		execute 'normal j'

		let l:isIndentChanged = l:p != getpos('.')[2]
		if l:isIndentChanged || getpos('.')[1] == l:eol
			if l:isIndentChanged | execute 'normal k' | endif
			break
		endif
	endwhile
endfunction "}}}
command! DownCursorGround call s:down_cursor_ground()

" }}}


"@Incompleted('"+" deleted in sql sytax')
"@Code('Select it, and execute this.')
"  $ "SELECT *" +
"  $ " FROM table;";
"  Yank => SELECT * FROM tablle;
function! s:sql_yank_normalize() range "{{{
	let sql = ""
	for i in range(a:firstline, a:lastline)
		let line = getline(i)
		let lineOfSql = substitute(substitute(
		\		substitute(line, "\"", "", 'g'),
		\	"+", "", 'g'), "\t", "", 'g')

		let sql .= lineOfSql
	endfor
	let @" = substitute(sql, "\s\s\+", " ", 'g')
endfunction "}}}
command! -range SqlCopy :<line1>,<line2>call s:sql_yank_normalize()

" Time Watcher  $ @See http://leafcage.hateblo.jp/entry/2013/08/02/001600
command! -bar TimerStart let  s:startTime = reltime()
command! -bar TimerEcho  echo reltimestr( reltime(s:startTime) )
command! -bar TimerPut   execute 'normal o' . reltimestr(reltime(s:startTime))

"}}}
" Development Support {{{

" If use *NIX then use QuickRun else use this.
if executable('javac') && executable('java')
	function! s:java_run_func() "{{{
		let l:javaname = split(@%, '\.')[0]
		let l:javav = s:system('java -version')
		if l:javav =~# '1\.8'
			let l:command  = ['javac -source 1.8 -encoding utf8', 'java']
		elseif l:javav =~# '1\.7'
			let l:command  = ['javac -source 1.7 -encoding utf8', 'java']
		else
			let l:command  = ['javac -encoding utf8', 'java']
		endif
		if s:isCygwin
			if executable('cocot')
				let l:command[0] = 'cocot '.l:command[0]
				let l:command[1] = 'cocot '.l:command[1]
			else
				echo 'You must be get [cocot] command.'
				return
			endif
		endif

		execute '!'.
		\	printf('%s %s.java',   l:command[0], l:javaname).';'.
		\	printf('%s %s',        l:command[1], l:javaname).';'.
		call s:rm( printf('%s*.class', l:javaname) )
	endfunction "}}}
	command! JavaRun call s:java_run_func()
endif

if executable('python')
	function! s:put_python_import_for_jp() "{{{
		execute 'normal gg'
		execute 'normal O' . "#!/usr/bin/env python"
		execute 'normal o' . "# -*- coding: utf-8 -*-"
		execute 'normal o' . "import sys"
		execute 'normal o' . "import codecs"
		execute 'normal o' . "sys.stdout = codecs.getwriter('utf_8')(sys.stdout)"
	endfunc "}}}
	command! ImportPythonJp call s:put_python_import_for_jp()
endif

" }}}
" Key Maping {{{

augroup AddtionalKeys
	autocmd FileType * nnoremap <silent> gc :CursorCenter<CR>
	autocmd FileType * nnoremap <silent> gk :UpCursorLid<CR>
	autocmd FileType * nnoremap <silent> gj :DownCursorGround<CR>
	"autocmd FileType * xmap gk :UpCursorLid<CR>
	"autocmd FileType * xmap gj :DownCursorGround<CR>
	"autocmd FileType * cmap gk :UpCursorLid<CR>
	"autocmd FileType * cmap gj :DownCursorGround<CR>
augroup END

" }}}


"-------------------------"
"          Alias          "
"-------------------------"
" Base {{{

call altercmd#load()

command! VimConfig         execute 'e  '.$MYVIMRC
command! VimConfigTab      execute 'tabnew|e '.$MYVIMRC
command! Reload            execute 'so '.$MYVIMRC
\|	if has('gui_running')
\|		execute 'so '.$MYGVIMRC
\|	endif
command! Wso               w|so %
if executable('sudo')
	command! ForceSave     w !sudo tee > /dev/null %
endif
command! ClearSwap         call s:system('rm '.s:directory.'/{.*,*}')
command! ClearView         call s:system('rm '.s:viewdir.'/*')
command! ClearUndo         call s:system('rm '.s:undodir.'/*')
command! ClearCache        execute 'normal :ClearSwap<CR> :ClearView<CR> :ClearUndo<CR>'

command! ColorPreview      Unite colorscheme -auto-preview
command! NeoBundleSearch   Unite neobundle/search

command! -nargs=+ GistPrivate Gist -p <args>
" Twitter {{{

"-- Basic --"
command! Twitter            TweetVimHomeTimeline
function! TwitterTabFunc() "{{{
	execute ':tabnew | TweetVimHomeTimeline'
endfunction "}}}
command! TwitterTab         call TwitterTabFunc()
command! Tweet              TweetVimSay
command! UserStream         TweetVimUserStream
command! UserTimeline       TweetVimUserTimeline


"-- Private Account --"
function! TwitterPrivateFunc() "{{{
	execute ':TweetVimSwitchAccount '.g:myPriv['twitter'].privAc
	execute ':TweetVimHomeTimeline'
endfunction "}}}
command! TwitterPrivate     call TwitterPrivateFunc()
function! TwitterPrivateTabFunc() "{{{
	execute ':tabnew'
	call TwitterPrivateFunc()
endfunction "}}}
command! TwitterPrivateTab  call TwitterPrivateTabFunc()
function! TweetPrivateFunc() "{{{
	execute ':TweetVimSwitchAccount '.g:myPriv['twitter'].privAc
	execute ':TweetVimSay'
endfunction "}}}
command! TweetPrivate       call TweetPrivateFunc()


"-- Public Account --"
function! TwitterPublicFunc() "{{{
	execute ':TweetVimSwitchAccount '.g:myPriv['twitter'].publAc
	execute ':TweetVimHomeTimeline'
endfunction "}}}
command! TwitterPublic      call TwitterPublicFunc()
function! TwitterPublicTabFunc() "{{{
	execute ':tabnew'
	call TwitterPublicFunc()
endfunction "}}}
command! TwitterPublicTab   call TwitterPublicTabFunc()
function! TweetPublicFunc() "{{{
	execute ':TweetVimSwitchAccount '.g:myPriv['twitter'].publAc
	execute ':TweetVimSay'
endfunction "}}}
command! TweetPublic        call TweetPublicFunc()

" }}}
command!      Bitly        TweetVimBitly
AlterCommand  tvs          TweetVimSwitchAccount
command! -nargs=1
\             Lingr        J6uil
command!      Translate    ExciteTranslate
command!      JazzUpdate   JazzradioUpdateChannels
command!      JazzList     Unite jazzradio
AlterCommand  JazzPlay     JazzradioPlay
command!      JazzStop     JazzradioStop

"}}}
" Development Support {{{

command! -nargs=1 Log VimConsoleLog <args>
command! LogClear VimConsoleClear

if executable('ghc') && executable('ghci')
	command! -nargs=*  Ghc      !runghc % <q-args>
	command!           Ghci     ConqueTerm ghci
	command!           Sghci    sp|ConqueTerm ghci
	command!           Vghci    vsp|ConqueTerm ghci
	command!           GhciTab  tabnew|ConqueTerm ghci
endif
if executable('hoogle')
	command! -nargs=1  Hoogle Ref hoogle <args>
endif

if executable('bash')
	command!  Bash     ConqueTerm bash
	command!  Sbash    sp|ConqueTerm bash
	command!  Vbash    vsp|ConqueTerm bash
	command!  BashTab  tabnew|ConqueTerm bash
endif

"}}}
" PluginOn {{{

command! VitalOn          NeoBundleSource vital.vim
command! FtCoqInstancyOn  NeoBundleSource coq.vim

"}}}


"-------------------------"
"     Global KeyMap       "
"-------------------------"
" Disable Default Keys {{{

augroup AddtionalKeys
	autocmd FileType * nnoremap <Up>    <NOP>
	autocmd FileType * nnoremap <Down>  <NOP>
	autocmd FileType * nnoremap <Left>  <NOP>
	autocmd FileType * nnoremap <Right> <NOP>
	autocmd FileType * inoremap <Up>    <NOP>
	autocmd FileType * inoremap <Down>  <NOP>
	autocmd FileType * inoremap <Left>  <NOP>
	autocmd FileType * inoremap <Right> <NOP>
	autocmd FileType * cnoremap <Left>  <NOP>
	autocmd FileType * cnoremap <Right> <NOP>
	autocmd FileType * nnoremap q:      <NOP>
augroup END

" }}}
" Override Defined KeyMaps {{{

augroup AddtionalKeys
	autocmd FileType * nnoremap Q gQ
	autocmd FileType * nnoremap <silent><buffer> <C-l><C-l> :nohlsearch<CR>
augroup END

" }}}
" Bash like KeyMaps {{{

augroup AddtionalKeys
	autocmd FileType * nmap     <C-j> <CR>
	autocmd FileType * imap     <C-j> <CR>
	autocmd FileType * cnoremap <C-b> <Left>
	autocmd FileType * cnoremap <C-f> <Right>
	autocmd FileType * cnoremap <C-a> <Home>
	autocmd FileType * cnoremap <C-h> <Backspace>
	autocmd FileType * cnoremap <C-d> <Del>
	autocmd FileType * cnoremap <C-e> <End>
	autocmd FileType * cnoremap <C-k> <C-\>e getcmdpos() < 2 ?'':getcmdline()[:getcmdpos()-2]<CR>
augroup END

" }}}
" Addtional KeyMaps {{{

" All buffers
augroup AddtionalKeys
	" Escape Insert Mode by <C-l>
	autocmd FileType * inoremap <C-l> <Esc>
	autocmd FileType * vnoremap <C-l> <Esc>
	autocmd FileType * cnoremap <C-l> <Esc>

	" Easy Load
	autocmd FileType * nnoremap <silent> <C-k><C-l> :so %<CR>

	" Special ESC Map when cannot use default <C-c> Map (Exam: VimShell)
	autocmd FileType * inoremap <C-k><C-l> <Esc>

	" Empty Line into Under
	autocmd FileType * nnoremap <silent> <C-m> :normal o<CR>

	" Easy Tabnew
	autocmd FileType * nnoremap <silent> <C-w>t :tabnew<CR>
augroup END


" Plugin buffers
augroup PluginPrefs
	autocmd FileType netrw  nunmap   L
	autocmd FileType netrw  nnoremap <buffer> <C-h>      -
	autocmd FileType netrw  nnoremap <silent><buffer> Q  :quit<CR>

	autocmd FileType tweetvim nmap <buffer> <leader>R   <Plug>(tweetvim_action_remove_status)
	autocmd FileType tweetvim nmap <buffer> <C-r>       <Plug>(tweetvim_action_reload)
	autocmd FileType tweetvim nnoremap <silent><buffer> s      :TweetVimSay <CR>
	autocmd FileType tweetvim nnoremap <buffer>         <C-a>  :TweetVimSwitchAccount<Space>
	autocmd FileType tweetvim nnoremap <buffer>         U      :TweetVimUserTimeline<Space>
	autocmd FileType tweetvim_say nnoremap <buffer> q      <NOP>
	autocmd FileType tweetvim_say inoremap <buffer> <C-i>  <Space><Space>

	autocmd FileType vimshell nunmap <buffer> Q
	autocmd FileType vimshell nunmap <buffer> q
	autocmd FileType vimshell imap   <buffer> <C-l>       <Plug>(vimshell_clear)
	autocmd FileType vimshell imap   <buffer> <C-k><C-p>  <Plug>(vimshell_history_unite)
	autocmd FileType vimshell iunmap <buffer> <C-p>
	autocmd FileType vimshell iunmap <buffer> <C-n>

	autocmd FileType w3m nnoremap <buffer> H       <BS>
	autocmd FileType w3m nnoremap <silent><buffer> <C-u>      :W3mAddressBar <CR>
	autocmd FileType w3m nnoremap <silent><buffer> <leader>E  :W3mShowExtenalBrowser <CR>
augroup END


" All buffers with plugins
augroup AddtionalKeys

	autocmd FileType * nmap <leader>w  <Plug>(openbrowser-open)

	autocmd FileType * nnoremap <silent> <leader>b          :ScratchUp<CR>

	autocmd FileType * nnoremap <silent> <leader>v          :VimShell -split-command=vsp -toggle<CR>
	autocmd FileType * nnoremap <silent> <leader><leader>v  :VimShellPop<CR>
	autocmd FileType * nmap              <leader>V          <Plug>(vimshell_create)
	autocmd FileType * nnoremap <silent> <leader><leader>V  :VimShellTab<CR>
augroup END

" }}}


"-------------------------"
"       File Type         "
"-------------------------"
"{{{

augroup ProgramTypes
	autocmd FileType vim    let &commentstring = ' "%s'
	autocmd FileType vim    NeoBundleSource 'vim-themis'

	autocmd VimEnter,WinEnter * syntax match rcHint /\t*"@\w\+/
	autocmd FileType vim highlight rcHint cterm=standout ctermfg=DarkYellow
augroup END

augroup ProgramTypes
	autocmd FileType c          let &commentstring = " /*%s*/"
	autocmd FileType cpp        let &commentstring = " /*%s*/"
	autocmd FileType java       let &commentstring = " /*%s*/"
	autocmd FileType cs         let &commentstring = " /*%s*/"

	"autocmd VimEnter,WinEnter    *  syntax match TypeInference /var\s\+/
	"autocmd FileType             cs highlight TypeInference cterm=bold ctermfg=11
	
	autocmd VimEnter,WinEnter *  syntax match Identifier /var\s\+/
	autocmd FileType cs highlight Identifier
augroup END

augroup ProgramTypes
	autocmd FileType haskell    let &commentstring = " --%s"
	autocmd FileType yesod      set ts=4|set sw=4|set et
	" autocmd TextChangedI *
	" 	\if &ft == 'yesod' &&
	" 	\	getline('.')[col('.')-2] . getline('.')[col('.')-3] .
	" 	\	getline('.')[col('.')-4] . getline('.')[col('.')-5] == '   '
	" 	\|	execute 'normal ia'
	" 	\|	imap <C-h> <Backspace>
	" 	\|	inoremap <Backspace> <Backspace><Backspace><Backspace><Backspace>
	" 	\|endif

	autocmd VimEnter,WinEnter * syntax match rcHfSpace /^\s\s*/
	autocmd FileType haskell    highlight    rcHfSpace cterm=underline ctermfg=Cyan
augroup END

augroup ProgramTypes
	autocmd BufNewFile,BufRead *.v let &ft='coq'
	autocmd FileType coq execute ':FtCoqInstancyOn'
	autocmd FileType coq let &commentstring = " (*%s*)"
augroup END

augroup ProgramTypes
	autocmd BufNewFile,BufRead *.md set filetype=markdown
	autocmd FileType markdown       set tabstop=2
	\|                              set shiftwidth=2
	\|                              set expandtab
	autocmd FileType markdown       nnoremap <silent> <leader>r :PrevimOpen<CR>
augroup END

augroup ProgramTypes
	autocmd FileType text set tabstop=2
	\|                    set shiftwidth=2
	\|                    set expandtab
	\|                    set textwidth=0

"}}}


"-------------------------"
"     Ignore Setting      "
"-------------------------"
"{{{

" Special File's Setting is write on here.

function! s:unload_file_event()  " {{{
	augroup FilePositSave
		autocmd!
	augroup END
endfunction  " }}}
augroup FileEvents
	autocmd FileType vimshell call s:unload_file_event()
augroup END

"}}}


"-------------------------"
"    Environment Pref     "
"-------------------------"
"{{{

if filereadable(expand('~/.vimrc_env'))
	so ~/.vimrc_env
endif

"}}}


let g:vimrc_loaded = 1

