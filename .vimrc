scriptencoding utf8


"----------------------------"
"  Target of this config     "
"    - UNIX like OS          "
"    - Cygwin                "
"    - Windows Kaoriya       "
"----------------------------"
"     Eigo                   "
"----------------------------"
"{{{

"---------------------------------------"
"*** Verified Functioned Environment ***"
"---------------------------------------"

"* Windows Kaoriya GVim cooperate to Cygwin
  "1. Download Kaoriya Vim
  "2. PATH /cygwin/{,usr/}bin add to Windows
  "3. Copy Examples/_{,g}vimrc to kaoriya-vim dir
  "4. Start GVim
    "- nazo(dekitari cannot-tari)

"* Windows Kaoriya GVim(Manually Update)
  "1. New Version download by Kaoriya site
  "2. Reference this config file
  "3. Starting GVim
    "- > Completely Succeeded.

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
" -- Functional Command
" -- Alias
" -- KeyMap
" -- FileTypes
" -- Ignore Setting
" -- Environment Pref
" ---
" Ideas {{{

"-- point marker line num in a file
"  -- I can jump marked line and list up mark lines

"-- implement command that print format (%0, %1, %2), ({0}, {1}, {2}) replace arguments on real time
"  -- covered multi line

"-- ahoge auto include yanked words

"-- View prev and next fold head text ...on echo or other buffer ?

"-- Want to Unite buffer delete

" }}}
" Issues {{{

"-- C-o hard use when vimshell

"-- automatic mkdir './C:' when execute NeoBundleInstall in windows kaoriya

"-- color highlight 'var' is not highlight when ...executed vsp|b hoge..?
"  -- What is best event ?

"-- 'gist:aiya000/ec5f6b2375a639831953' cannot divide configure
"  -- if diveding then install it as no named plugin

"-- submode fold_move do not functioned when not exists fold under cursor

"-- not returned foldenabled on visual leaved by zf

"-- duplicated helptag 'g:restart_menu_lang' in openbrowser and restart.vim

"}}}
" Todo {{{

"-- Visualizability for Vim Tab
"  -- showing window num when one window only

"-- Devide autocmds to just augroup

"-- One hand view mode

"-- View column on status line always

"-- highlight prefs devide to vimrc and gvimrc

"-- fix mystery pattern ?\+

" }}}



"----------------------------------------
" {- Hints -} "
" @Bugs         => This hoge has the bugs.
" @Incompleted  => This is not completed making.
" @Unchecked    => This was not unchecked that is operate right
" @Unsupported  => Do not supported functions when now.
" @Unknowned    => I don't know why this functioned.
"     ／人◕ ‿‿ ◕人＼ <  Wakega wakaranaiyo!
" @Unused       => Not used this yet now, needs inquires deleting this.
" @Deprecated   => Deprecated This vimrc Version.
" @Experiment   => This is experimental implementation.


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

let g:vimrc_loaded = get(g:, 'vimrc_loaded', 0)

let s:isWindows = has('win32')
let s:isCygwin  = has('win32unix')
let s:isKaoriya = has('kaoriya')
let s:isDosWin  = s:isWindows && !s:isCygwin &&!s:isKaoriya
let s:isUnix    = has('unix')
let s:isMac     = has('mac')

let s:hasCygwin = isdirectory('/cygwin/bin')
let s:hasMingw  = 0  " dummy

let s:vimHome   = expand('~/.vim')

let s:backupdir = expand('~/.backup/vim_backup')
let s:directory = s:backupdir.'/swp'
let s:undodir   = s:backupdir.'/undo'
let s:viewdir   = s:backupdir.'/view'

let s:username  = $USER
let s:groupname = $GROUP != '' ? $GROUP : $USER

"}}}


"---------------------"
"    Local Function   "
"---------------------"
"{{{

function! s:system(cmd)
	if exists('*vimproc#system()')
		return vimproc#system(a:cmd)
	else
		return system(a:cmd)
	endif
endfunction


function! s:echo_error(msg)
	echohl ERROR
	echo a:msg
	echohl NONE
endfunction

"}}}


"-------------------------"
"       Initialize        "
"-------------------------"
filetype plugin indent on
" autocmd Groups {{{

augroup plugin_pref
	autocmd!
augroup END

augroup file_event
	autocmd!
augroup END

augroup file_visit
	autocmd!
augroup END

augroup extension_type
	autocmd!
augroup END

augroup def_highlight
	autocmd!
augroup END

augroup key_map
	autocmd!
augroup END

augroup key_event
	autocmd!
augroup END

"}}}
" For Support Kaoriya Vim {{{

if s:isKaoriya

	" Set Environment
	let $HOME = $VIM
	let s:vimHome = expand('~/.vim')  " Reset with $HOME
	let &runtimepath = &runtimepath.','.s:vimHome
	if s:hasCygwin
		let $PATH = '/cygwin/bin;/cygwin/usr/bin;/cygwin/usr/sbin;'.$PATH
		let $PATH = $HOME.'/bin;'.$PATH
	endif


	" Build Base Directories
	if !isdirectory(s:vimHome)
		call mkdir(s:vimHome)
	endif


	" For Using No Default vimproc
	let suppress = $VIM.'/switches/enabled/disable-vimproc.vim'
	if s:isWindows && !s:hasMingw && filereadable(suppress)
		call delete(suppress)
	elseif s:isWindows && s:hasMingw && !filereadable(suppress)
		call writefile([], suppress)
	endif
	unlet suppress


	" Unset Kaoriya Preference
	set noignorecase
	set nosmartcase

	augroup file_event
		autocmd BufRead $MYVIMRC setl enc=utf8 | setl fenc=utf8
	augroup END
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
		echo 'Cannot introduce NeoBundle.'
		echohl None
		throw 'neobundle.vim clone failed.'
	endif
endfunction  " }}}

if has('vim_starting')
	try
		let &runtimepath = &runtimepath.','.s:vimHome.'/bundle/neobundle.vim'
		" Throws Error when nothing neobundle in runtime path
		call neobundle#begin()
	catch
		if isdirectory(s:neobundleDir) && !exists(':NeoBundle')
			" Plugin Directories may be empty when git cloned new.
			call s:remove_empty_bundledir()
			echo 'bundle directories initialized.'
		endif
		try
			call s:fetch_neobundle()
			call neobundle#begin()
			echo 'NeoBundle installed.'
			echo 'Please closing vim and reopening vim once,'
			echo 'and executing :NeoBundleInstall .'
		catch
			call s:echo_error('neobundle.vim clone failed.')
			call s:echo_error('>> Vim Config Error <<')
		endtry
	endtry
endif

unlet s:neobundleDir
unlet s:bundleDir
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
NeoBundle 'kashewnuts/gmail.vim'
NeoBundle 'basyura/J6uil.vim'
NeoBundle 'osyo-manga/vim-gyazo'
NeoBundle 'yuratomo/w3m.vim'
NeoBundle 'mattn/learn-vimscript'
NeoBundle 'rbtnn/vimconsole.vim'
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
NeoBundle 'thinca/vim-ref'
NeoBundle 'ujihisa/ref-hoogle'
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
NeoBundle 'aiya000/arot13.vim'
NeoBundle 'aiya000/ahoge-put.vim'
NeoBundle 'kannokanno/previm'
NeoBundle 'gist:aiya000/ec5f6b2375a639831953', {
\	'name'        : 'gitlogviewer.vim',
\	'script_type' : 'plugin'
\}
NeoBundle 'kamichidu/vim-vdbc'
NeoBundle 'mattn/vdbi-vim'
NeoBundle 'LeafCage/foldCC'
NeoBundle 'katono/rogue.vim'
NeoBundleDisable 'aiya000/asql.vim'
NeoBundle 'kamichidu/vim-benchmark'
NeoBundle 'kana/vim-submode'
NeoBundle 'gist:aiya000/58931585f8ba6aa43b87', {
\	'name'        : 'conceal-javadoc.vim',
\	'script_type' : 'plugin'
\}
NeoBundle 'mfumi/ref-dicts-en'
NeoBundle 'thinca/vim-painter'
NeoBundle 'osyo-manga/vim-anzu'
NeoBundle 'osyo-manga/vim-over'
NeoBundle 'tyru/restart.vim'


call neobundle#end()

helptags ~/.vim/bundle/.neobundle/doc

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
call neobundle#config('ref-dicts-en', {
\	'depends' : ['thinca/vim-ref']
\})

" }}}


"------------------------"
"*** Plugin Configure ***"
"------------------------"
"--- vim-quickrun ---" {{{

let g:quickrun_config = {
\	'_' : {
\		'split'  : '',
\		'runner' : exists('*vimproc#system()') ? 'vimproc' : 'system',
\		'runner/vimproc/updatetime' : 10,
\		'hook/time/enable' : 1
\	},
\	'cpp' : {
\		'command' : 'g++',
\		'cmdopt'  : '-I/usr/include/c++/4.9 -I/usr/include/c++/4.9/x86_64-linux-gnu -std=c++11'
\	},
\	'java' : {
\		'cmdopt' : '-source 1.8',
\		'runner' : exists('*vimproc#system()') ? 'process_manager' : 'system'
\	}
\}

if s:isWindows
	let g:quickrun_config['cs'] = {
	\	'command'  : 'csc.exe',
	\	'exec'     : ['%c %o %s:p', '%s:p:r.exe', 'del %s:p:r.exe'],
	\	'hook/output_encode/encoding' : 'cp932:utf8'
	\}
elseif s:isUnix
	let g:quickrun_config['cs'] = {
	\	'command'  : 'gmcs',
	\	'exec'     : ['%c %o %s:p > /dev/null', 'mono %s:p:r.exe', 'rm %s:p:r.exe'],
	\	'tempfile' : '{tempname()}.cs'
	\}
endif

if s:isCygwin
	let g:quickrun_config['java'] = {
	\	'command' : 'javac',
	\	'exec'    : ['%c %o `echo %s | sed s:\:/:g | cygpath -w -f -`', '%c %s:t:r %a'],
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

augroup plugin_pref
	autocmd FileType quickrun setlocal wrap
augroup END

" }}}
"--- vimproc.vim ---"{{{

if s:isWindows && !s:hasMingw
	"NeoBundleDisable 'Shougo/vimproc.vim'
	"@Incompleted('I should use a like NeoBundleDisable')
	set runtimepath-=~/.vim/bundle/vimproc.vim/
endif

" }}}
"--- TweetVim ---"{{{

let g:tweetvim_async_post = 1
augroup plugin_pref
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

augroup plugin_pref
	autocmd FileType vimshell
	\	call vimshell#altercmd#define('thanks', "echo \"(*^o^)< You're welcome!\"")
	\|	call vimshell#set_alias('sp',  ':sp  | VimShellCreate')
	\|	call vimshell#set_alias('vsp', ':vsp | VimShellCreate')

	autocmd FileType vimshell  setl fdm=marker
	autocmd FileType vimshell  setl nolist
	autocmd FileType vimshell  setl wrap
augroup END

"}}}
"--- J6uil ---"{{{

augroup plugin_pref
	autocmd FileType J6uil setlocal wrap
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

"@See('momonga-san no sugoi blog')
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

"@Code(':Unite javasrc')
let g:unite_source_alias_aliases = {
\	'javasrc' : {
\		'source' : 'file_rec',
\		'args'   : '~/Documents/workspace/Java/src',
\	},
\}

"}}}
"--- foldCC ---"{{{

"let g:foldCCtext_enable_autofdc_adjuster = 0

"}}}
"--- vim-submode ---"{{{

let g:submode_timeout = 0


" Window Resizer
call submode#enter_with('window_resize', 'n', '', '<C-s>w')
call submode#map('window_resize', 'n', '', 'j', '<C-w>+')
call submode#map('window_resize', 'n', '', 'k', '<C-w>-')
call submode#map('window_resize', 'n', '', 'h', '<C-w><')
call submode#map('window_resize', 'n', '', 'l', '<C-w>>')


" Fold Mover
call submode#enter_with('fold_move', 'n', '', '<C-s>z')
call submode#map('fold_move', 'n', '', 'j', 'zczjzozz')
call submode#map('fold_move', 'n', '', 'k', 'zczkzozz')

" Incremental Search Commands
"call submode#enter_with('incsearch_command', 'c', '', '<C-@><C-p>', '<Up>')
"call submode#enter_with('incsearch_command', 'c', '', '<C-@><C-n>', '<Down>')
"call submode#map('incsearch_command', 'c', '', '<C-p>', '<Up>')
"call submode#map('incsearch_command', 'c', '', '<C-n>', '<Down>')

"}}}
"--- ref-dicts-en ---" {{{
"@See('http://d.hatena.ne.jp/akishin999/20131024/1382569289')

let g:ref_use_vimproc = exists('*vimproc#system()') ? 1 : 0
let g:ref_source_webdict_sites = {
\	'weblio' : {
\		'url' : 'http://ejje.weblio.jp/content/%s'
\	}
\}

let g:ref_source_webdict_sites['default'] = 'weblio'

function! s:webdict_filter(output)
	return join(split(a:output, "\n")[101 : ], "\n")
endfunction
let g:ref_source_webdict_sites['weblio'].filter = function('s:webdict_filter')

" }}}
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

" Do not wrap line
set nowrap

" View line number
set number

" Indent Width
set tabstop=4

" Highlight Hit Keyword
set hlsearch

" ☆ Fix View 2byte Code (Not support gnome-terminal)
set ambiwidth=double

" One More Set for 2byte Code !!
syntax sync fromstart

" Syntax Highlight On
syntax on

" Visualize Tab and Space $ See('listchars : Event Method => file_event')
set list

" Powered Up Syntax Highlight
" {{{

augroup def_highlight
	"autocmd Colorscheme * highlight Normal       cterm=NONE      ctermfg=Cyan
	autocmd ColorScheme * highlight Visual       cterm=underline ctermfg=White ctermbg=Cyan
	autocmd ColorScheme * highlight IncSearch                    ctermfg=Black ctermbg=Cyan
	autocmd ColorScheme * highlight Pmenu        cterm=standout  ctermfg=Blue
	autocmd ColorScheme * highlight PmenuSel                     ctermfg=Black ctermbg=White
	autocmd ColorScheme * highlight TabLine      cterm=standout  ctermfg=Blue
	autocmd ColorScheme * highlight TabLineSel   cterm=NONE      ctermfg=Cyan
	autocmd ColorScheme * highlight TabLineFill  cterm=standout  ctermfg=Blue
	autocmd ColorScheme * highlight VertSplit    cterm=NONE      ctermfg=Cyan  ctermbg=Blue
	autocmd ColorScheme * highlight StatusLine                   ctermfg=Cyan  ctermbg=Black
	autocmd ColorScheme * highlight StatusLineNC                 ctermfg=Blue
	autocmd ColorScheme * highlight LineNr                       ctermfg=Blue
	autocmd ColorScheme * highlight CursorLine   cterm=underline ctermfg=Cyan

	"@Incompleted('not functioned'){Ubuntu:vim_7.4.427}
	autocmd VimEnter,BufWinEnter * match RcEmSpace /　/
	autocmd ColorScheme * highlight RcEmSpace cterm=standout ctermfg=LightBlue

	autocmd VimEnter,BufWinEnter * match RcMyHint /\s*"@\w\+/
	autocmd ColorScheme * highlight RcMyHint cterm=standout ctermfg=Red
augroup END

augroup def_highlight
	autocmd InsertEnter * highlight StatusLine ctermfg=Black ctermbg=Cyan
	autocmd InsertLeave * highlight StatusLine ctermfg=Cyan  ctermbg=Black
augroup END

" }}}

" Set Color Scheme
colorscheme desert

" Set Base Color
set background=dark

" Indent Wrapped Text
if exists('+breakindent')
	set breakindent
	set linebreak
endif

" Invisible character visualize to hex
"set display=uhex

" Scroll Margin
set scrolloff=4

" Reset search highlight
nohlsearch

" View cursor column on <C-g>
set noruler

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

" Fold Text with foldmarker and fold sets
set foldmethod=marker
set foldtext=foldCC#foldtext()
set foldcolumn=1
let &fillchars = 'vert:|,fold: '
set foldopen=search,jump,mark,percent,insert,tag,undo
set foldclose=all

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

" Disable Auto Commentalize New Line
set formatoptions-=ro

" Split Method on BufOpen
"set splitbelow
"set splitright

" Ignore case on Insert completion
set noinfercase

" Unlimited Cursor move in screen
"set virtualedit=all

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
set runtimepath+=~/.vim/makes/asql.vim

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
"@Experiment('2014-10-04')
set fileencodings=utf-8,sjis,euc-jp,cp932,ucs-bom,iso-2022-jp-3,iso-2022-jp,eucjp-ms,euc-jisx0213,ucs-bom,latin1,default

" Generate HelpTags My Help
if isdirectory('~/.vim/doc')
	helptags ~/.vim/doc
endif

" If buffer doesn't has filetype then set filetype 'none'
augroup file_event
	autocmd VimEnter,BufNew * if &ft == '' | setf none | endif
augroup END

"}}}


"-------------------------"
"      Event Method       "
"-------------------------"
"{{{

" Save Cursor Position when file closed
augroup file_visit
	autocmd BufWinLeave ?\+ silent mkview
	autocmd BufWinEnter ?\+ silent loadview
augroup END


" Powered Auto File Backup when written
set nobackup
function! s:update_backup_by_date() "{{{
	let l:dailydir = s:backupdir . '/' . strftime("%Y-%m-%d")
	if !isdirectory(l:dailydir)
		call mkdir(l:dailydir, 'p', 0755)
		if s:isUnix
			call s:system(printf('chown -R %s:%s %s', s:username, s:groupname, l:dailydir))
		endif
	endif

	let l:filepath = split(expand('%'), '/')
	let l:filename = s:isWindows
		\ ? filepath[len(filepath)-1] . strftime('_at_%H-%M')
		\ : filepath[len(filepath)-1] . strftime('_at_%H:%M')

	call writefile(getline(1, '$'), l:dailydir.'/'.l:filename)
endfunction "}}}
augroup file_event
	autocmd BufWritePre ?\+ silent call s:update_backup_by_date()

	"autocmd UserGettingBored * echo 'Naijan!!!!'

	autocmd VimEnter,BufWinEnter *
		\	if &encoding == 'cp932'
		\		let &listchars = 'tab:>_,trail:_,extends:>,precedes:<,nbsp:%'
		\	else
		\		let &listchars = 'tab:» ,trail:_,extends:»,precedes:«,nbsp:%,eol:↲'
		\	endif
augroup END

augroup key_event
	" For noinfercase
	autocmd InsertEnter * setl ignorecase
	autocmd InsertLeave * setl noignorecase
augroup END

"}}}

"-------------------------"
"   Functional Command    "
"-------------------------"
" Utility Function {{{

" Revese string of current line $ @See('reverse() tukaeYo')
function! s:reverse_line()  " {{{
	let l:reverse = ""
	let l:str = getline('.')
	let l:len = strlen(l:str)
	for i in range(1, l:len)
		let l:reverse .= l:str[l:len - i]
	endfor
	let l:reverse .= "\n"

	let l:r = @"
	execute 'normal! dd'
	let @" = l:reverse
	execute 'normal! P'
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
			execute 'normal! "_dd'
			execute 'normal! i' . lines[a:lastline - i]
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


" Low accuracy randome integer
function! s:random_int(max) "{{{
	let l:matchEnd = matchend(reltimestr(reltime()), '\d\+\.') + 1
	return reltimestr(reltime())[l:matchEnd :] % (a:max + 1)
endfunction "}}}
command! -nargs=1 RandomPut execute 'normal! a' . s:random_int(<q-args>) )


" Time Watcher  $ @See('http://leafcage.hateblo.jp/entry/2013/08/02/001600')
command! -bar TimerStart let  s:startTime = reltime()
command! -bar TimerEcho  echo reltimestr( reltime(s:startTime) )
command! -bar TimerPut   execute 'normal! o' . reltimestr(reltime(s:startTime))


"}}}
" Action Function {{{

let g:rc_temporary_dir = get(g:, 'rc_temporary_dir', 'undefined')
command! TDirPwd           echo g:rc_temporary_dir
function! s:set_temporary_dir(path) "{{{
	if isdirectory(a:path)
		let g:rc_temporary_dir =
		\	a:path == '.' ? expand('%:p:h')
		\	              : a:path
		echo g:rc_temporary_dir
	else
		call s:echo_error('No such temporary root dir')
	endif
endfunction "}}}
command! -nargs=1  TDirSet call s:set_temporary_dir(<q-args>)
command! TDirSetCurrentDir call s:set_temporary_dir('.')
function! s:cd_temporary_dir() "{{{
	if g:rc_temporary_dir == 'undefined'
		call s:echo_error('Not set temporary root dir')
	else
		execute 'cd '.g:rc_temporary_dir
		echo g:rc_temporary_dir
	endif
endfunction "}}}
command! TDirCd            call s:cd_temporary_dir()

" }}}
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
		call delete( printf('%s*.class', l:javaname) )
	endfunction "}}}
	command! JavaRun call s:java_run_func()
endif

if executable('python')
	function! s:put_python_import_for_jp() "{{{
		execute 'normal! o' . "#!/usr/bin/env python"
		execute 'normal! o' . "# -*- coding: utf-8 -*-"
		execute 'normal! o' . "import sys"
		execute 'normal! o' . "import codecs"
		execute 'normal! o' . "sys.stdout = codecs.getwriter('utf_8')(sys.stdout)"
	endfunc "}}}
	command! ImportPythonJp call s:put_python_import_for_jp()
endif


command! PutShortSeparator
	\	execute 'normal! a'.'/* -=-=-=-=-=-=-=-=- */'
	\|	execute 'normal =='
command! PutLongSeparator
	\	execute 'normal! a'.'/* ---===---===---===---===---===---===--- */'
	\|	execute 'normal =='


function! s:put_html_base() "{{{
	execute 'normal! o' . '<html lang="ja">'
	execute 'normal! o' . '<head>'
	execute 'normal! o' . '<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">'
	execute 'normal! o' . '<title></title>'
	execute 'normal! o' . '</head>'
	execute 'normal! o' . '<body>'
	execute 'normal! o'
	execute 'normal! o' . '</body>'
	execute 'normal! o' . '</html>'
endfunction "}}}
command! PutHtmlBase call s:put_html_base()


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

" }}}


"-------------------------"
"          Alias          "
"-------------------------"
" Utils {{{

call altercmd#load()

" Vim Utils {{{
command! VimConfig         e  $MYVIMRC
command! VimConfigTab      tabnew | e $MYVIMRC
command! Reload            so $MYVIMRC
	\|	if has('gui_running')
	\|		so $MYGVIMRC
	\|	endif
if executable('sudo')
	command! ForceSave     w !sudo tee > /dev/null %
endif
command! Resetf            let &ft = &ft  " for Event [FileType * ]

command! ColorPreview      Unite colorscheme -auto-preview

" }}}
" Twitter {{{

"-- Basic --"
command! Twitter            TweetVimHomeTimeline
command! TwitterTab         tabnew | call TwitterTabFunc()
command! Tweet              TweetVimSay


"-- Private Account --"
function! TwitterPrivateFunc() "{{{
	execute ':TweetVimSwitchAccount '.g:rc_private['twitter']['priv_ac']
	TweetVimHomeTimeline
endfunction "}}}
command! TwitterPrivate     call TwitterPrivateFunc()
command! TwitterPrivateTab  tabnew | call TwitterPrivateFunc()
function! TweetPrivateFunc() "{{{
	execute ':TweetVimSwitchAccount '.g:rc_private['twitter']['priv_ac']
	TweetVimSay
endfunction "}}}
command! TweetPrivate       call TweetPrivateFunc()


"-- Public Account --"
function! TwitterPublicFunc() "{{{
	execute ':TweetVimSwitchAccount '.g:rc_private['twitter']['publ_ac']
	TweetVimHomeTimeline
endfunction "}}}
command! TwitterPublic      call TwitterPublicFunc()
command! TwitterPublicTab   tabnew | call TwitterPublicFunc()
function! TweetPublicFunc() "{{{
	execute ':TweetVimSwitchAccount '.g:rc_private['twitter']['publ_ac']
	TweetVimSay
endfunction "}}}
command! TweetPublic        call TweetPublicFunc()


command!      Bitly         TweetVimBitly
AlterCommand  tvs           TweetVimSwitchAccount

" }}}

" Beautifull Life
command!      JazzUpdate    JazzradioUpdateChannels
command!      JazzList      Unite jazzradio
AlterCommand  JazzPlay      JazzradioPlay
command!      JazzStop      JazzradioStop

" Translates Languages
command!          Translate     ExciteTranslate
function! s:weblio_translate_cmdline(...) "{{{
	let l:line = ''
	for word in a:000
		let l:line .= word . '+'
	endfor

	execute 'Ref webdict weblio ' . l:line
endfunction "}}}
command! -nargs=* TranslateWeblio call s:weblio_translate_cmdline(<f-args>)

command! -nargs=* GrepNow       vimgrep <f-args> % | cwindow

"}}}
" Developments{{{

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
" PluginSwitcher {{{

" These use by this configration
command! VitalOn          NeoBundleSource vital.vim
command! FtCoqInstancyOn  NeoBundleSource coq.vim

"}}}


"-------------------------"
"         KeyMap          "
"-------------------------"
"--- Roles ---"{{{

"* <C-k> is Primary prefix key
"  - Use for be big frequency of operation

"* <C-@> is Secondary prefix key
"  - Use for be little frequency of operation

"}}}
" Global KeyMaps {{{

" Disable Default Keys {{{

augroup key_map
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
augroup END

" }}}
" Override Defined Keys {{{

augroup key_map
	autocmd FileType * nnoremap Q gQ
augroup END

" }}}
" Bashnize Command Mode {{{

augroup key_map
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
" Customize Keys {{{

" To All buffers
augroup key_map
	"-- With Prefixes --"
	" for case remapped by plugin <C-z> (ex:vimsh)
	autocmd FileType * nnoremap                  <C-k><C-z>  <C-z>
	autocmd FileType * inoremap                  <C-k><C-l>  <Esc>
	autocmd FileType * inoremap                  <C-k><C-k>  <C-O>d$
	autocmd FileType * cnoremap                  <C-k><C-p>  <Up>
	autocmd FileType * cnoremap                  <C-k><C-n>  <Down>

	autocmd FileType * nnoremap <silent>         <C-@><C-r>  :Reload<CR>
	autocmd FileType * nnoremap <silent>         <C-@><C-b><C-n>  :bn<CR>
	autocmd FileType * nnoremap <silent>         <C-@><C-b><C-p>  :bp<CR>
	autocmd FileType * nnoremap <silent><buffer> <C-@><C-l>  :nohlsearch<CR>
	autocmd FileType * nnoremap <silent>         <C-@>l      :so %<CR>
	autocmd FileType * nnoremap <silent>         <C-@>r      :Resetf<CR>
	autocmd FileType * nnoremap                  <leader>j   i<C-x><C-e><Esc>
	autocmd FileType * nnoremap                  <leader>k   i<C-x><C-y><Esc>

	"-- Overwrite exists map --"
	autocmd FileType * inoremap <C-l> <Esc>
	autocmd FileType * vnoremap <C-l> <Esc>
	autocmd FileType * cnoremap <C-l> <Esc>

	"-- Customize --"
	autocmd FileType * nnoremap <silent> <C-m> :normal! o<CR>
	" for window
	autocmd FileType * nnoremap <silent> <C-w>t  :tabnew<CR>
	autocmd FileType * nnoremap <silent> <C-w>T  :tabclose<CR>
	autocmd FileType * nnoremap <silent> <C-w>bd :bd<CR>
	autocmd FileType * nnoremap <silent> <C-w>Bd :bd!<CR>
	" for folds
	autocmd FileType * nnoremap <expr> h foldclosed('.') > -1 ? 'zo' : 'h'
	autocmd FileType * nnoremap <expr> l foldclosed('.') > -1 ? 'zo' : 'l'
	autocmd FileType * nnoremap zj     zjzo
	autocmd FileType * nnoremap zk     zkzo
	autocmd FileType * nnoremap z[     [z
	autocmd FileType * nnoremap z]     ]z

	"-- Plugins --"
	autocmd FileType * nmap              <leader>w          <Plug>(openbrowser-open)
	"Note: Happened Heavy Motion
	autocmd FileType * nnoremap <silent> :%s/               :OverCommandLine<CR>%s/
	autocmd FileType * nnoremap <silent> :s/                :OverCommandLine<CR>s/
	autocmd FileType * vnoremap <silent> :s/                :OverCommandLine<CR>s/
	" for vimshell
	autocmd FileType * nnoremap <silent> <leader>v          :VimShell -split-command=vsp -toggle<CR>
	autocmd FileType * nnoremap <silent> <leader><leader>v  :VimShellPop<CR>
	autocmd FileType * nmap              <leader>V          <Plug>(vimshell_create)
	autocmd FileType * nnoremap <silent> <leader><leader>V  :VimShellTab<CR>
	" for anzu
	autocmd FileType * nmap              n                  <Plug>(anzu-n-with-echo)zv
	autocmd FileType * nmap              N                  <Plug>(anzu-N-with-echo)zv
	autocmd FileType * nmap              *                  <Plug>(anzu-star-with-echo)zv
	autocmd FileType * nmap              #                  <Plug>(anzu-sharp-with-echo)zv
augroup END

" }}}
" Keys With Function {{{

"--- Functions ---" {{{

" Move Cursor Line's Center
function! s:cursor_move_to_center() "{{{
	execute 'normal! 0'
	for i in range(strlen(getline('.'))/2)
		execute 'normal! l'
	endfor
endfunction "}}}

" For Movement in a Indent Block
" {{{

function! s:cursor_up_to_lid() "{{{
	if &ft == 'netrw' | return | endif

	while 1
		let l:p = getpos('.')[2]
		execute 'normal! k'

		let l:isIndentChanged = l:p != getpos('.')[2]
		if l:isIndentChanged || getpos('.')[1] == 1
			if l:isIndentChanged | execute 'normal! j' | endif
			break
		endif
	endwhile
endfunction "}}}
function! s:cursor_down_to_ground() "{{{
	if &ft == 'netrw' | return | endif

	let l:eol = len(readfile(@%))
	while 1
		let l:p = getpos('.')[2]
		execute 'normal! j'

		let l:isIndentChanged = l:p != getpos('.')[2]
		if l:isIndentChanged || getpos('.')[1] == l:eol
			if l:isIndentChanged | execute 'normal! k' | endif
			break
		endif
	endwhile
endfunction "}}}

" }}}

" Foldopen all on VisualEnter, and Foldclose all on VisualLeave
let s:visualFoldToggle = get(s:, 'visualFoldToggle', 0) "{{{
function! s:visual_fold_all()
	if mode() =~# "^[vV\<C-v>]"
		if !s:visualFoldToggle && &foldenable
			execute 'normal! zizz'
			let s:visualFoldToggle = 1
		endif
	else
		if s:visualFoldToggle
			execute 'normal! zizz'
			let s:visualFoldToggle = 0
		endif
	endif
endfunction "}}}

" Toggle Enable CursorKeys
let s:enableCursorKeys = get(s:, 'enableCursorKeys', 0) "{{{
function! s:enable_cursor_keys_toggle()
	if !s:enableCursorKeys
		nnoremap <Up>    <Up>
		nnoremap <Down>  <Down>
		nnoremap <Left>  <Left>
		nnoremap <Right> <Right>
		inoremap <Up>    <Up>
		inoremap <Down>  <Down>
		inoremap <Left>  <Left>
		inoremap <Right> <Right>
		cnoremap <Left>  <Left>
		cnoremap <Right> <Right>
		let s:enableCursorKeys = 1
	else
		nnoremap <Up>    <NOP>
		nnoremap <Down>  <NOP>
		nnoremap <Left>  <NOP>
		nnoremap <Right> <NOP>
		inoremap <Up>    <NOP>
		inoremap <Down>  <NOP>
		inoremap <Left>  <NOP>
		inoremap <Right> <NOP>
		cnoremap <Left>  <NOP>
		cnoremap <Right> <NOP>
		let s:enableCursorKeys = 0
	endif
endfunction "}}}

" Temporary Buffer Utils
"{{{
if s:isWindows  " is different operated sp ubuntu and kaoriya?
	command! ScratchUp  execute ':Scratch' | resize 5
else  " for operate ubuntu
	function! s:scratch_up_by_condition()
		if !&modified
			execute ':sp|Scratch' | resize 5
		else
			execute ':Scratch' | resize 5
		endif
	endfunction
	command! ScratchUp  call <SID>scratch_up_by_condition()
endif
command! EmptyBufUp execute ':new' | resize 5
"}}}

" }}}

augroup key_map
	autocmd FileType * nnoremap <silent> gc :call <SID>cursor_move_to_center()<CR>
	autocmd FileType * nnoremap <silent> gk :call <SID>cursor_up_to_lid()<CR>
	autocmd FileType * nnoremap <silent> gj :call <SID>cursor_down_to_ground()<CR>
	"autocmd FileType * xnoremap <silent> gk :call <SID>cursor_up_to_lid()<CR>
	"autocmd FileType * xnoremap <silent> gj :call <SID>cursor_down_to_ground()<CR>
	"autocmd FileType * cnoremap <silent> gk :call <SID>cursor_up_to_lid()<CR>
	"autocmd FileType * cnoremap <silent> gj :call <SID>cursor_down_to_ground()<CR>

	autocmd FileType * nnoremap <silent> <C-@><C-w>    setl wrap! wrap?
	autocmd FileType * nnoremap <silent> <C-@>jkjkjkjk :call <SID>enable_cursor_keys_toggle()<CR>
	autocmd CursorMoved * call s:visual_fold_all()

	autocmd FileType * nnoremap <silent> <leader>b  :ScratchUp<CR>
	autocmd FileType * nnoremap <silent> <leader>B  :EmptyBufUp<CR>
augroup END

" }}}

"}}}
" Buffer Local KeyMaps {{{

" To Plugin buffers
augroup plugin_pref
	autocmd FileType netrw nunmap   L
	autocmd FileType netrw nmap     <buffer> H -
	autocmd FileType netrw nnoremap <silent><buffer> Q  :quit<CR>

	autocmd FileType tweetvim nmap <buffer> <leader>R  <Plug>(tweetvim_action_remove_status)
	autocmd FileType tweetvim nmap <buffer> <C-r>      <Plug>(tweetvim_action_reload)
	autocmd FileType tweetvim nnoremap <silent><buffer> s      :TweetVimSay <CR>
	autocmd FileType tweetvim nnoremap         <buffer> <C-a>  :TweetVimSwitchAccount<Space>
	autocmd FileType tweetvim nnoremap         <buffer> U      :TweetVimUserTimeline<Space>
	autocmd FileType tweetvim nnoremap <silent><buffer> Q      :bd<CR>
	autocmd FileType tweetvim_say nnoremap <buffer> q      <NOP>
	autocmd FileType tweetvim_say inoremap <buffer> <C-i>  <Space><Space>

	"@Bugs('error when second startup')
	autocmd FileType vimshell nunmap <buffer> Q
	autocmd FileType vimshell nunmap <buffer> q
	autocmd FileType vimshell imap   <buffer> <C-l>       <Plug>(vimshell_clear)
	autocmd FileType vimshell imap   <buffer> <C-k><C-p>  <Plug>(vimshell_history_unite)
	autocmd FileType vimshell iunmap <buffer> <C-p>  " Using default completion
	autocmd FileType vimshell iunmap <buffer> <C-n>  "_

	autocmd FileType w3m nnoremap         <buffer> H          <BS>
	autocmd FileType w3m nnoremap <silent><buffer> <C-u>      :W3mAddressBar <CR>
	autocmd FileType w3m nnoremap <silent><buffer> <leader>E  :W3mShowExtenalBrowser <CR>

	autocmd FileType J6uil            nnoremap <silent><buffer> Q     :bd<CR>
	autocmd FileType J6uil_say        nmap             <buffer> <C-j> <CR>     " Enter to Say
	autocmd FileType git-log.git-diff nnoremap         <buffer> Q     :bd<CR>
augroup END

" }}}


"-------------------------"
"        FileTypes        "
"-------------------------"
"{{{

" Set for "Vi Improved"
augroup extension_type
	autocmd FileType vim  NeoBundleSource 'vim-themis'
	autocmd VimEnter,BufWinEnter,BufEnter * syntax match rcHint /\s*"@\w\+/
	autocmd FileType vim highlight rcHint cterm=standout ctermfg=DarkYellow
augroup END

" Set for C-Sharp
augroup extension_type
	"autocmd VimEnter,WinEnter    *  syntax match TypeInference /var\s\+/
	"autocmd FileType             cs highlight TypeInference cterm=bold ctermfg=11
	autocmd VimEnter,BufWinEnter,BufEnter,WinEnter * syntax match Identifier /\<var\>/
	autocmd FileType cs highlight Identifier
augroup END

" Set for Haskell
augroup extension_type
	autocmd FileType yesod      setl ts=4|setl sw=4|setl et
	autocmd VimEnter,BufWinEnter,BufEnter * syntax match rcHfSpace /^\s\s*/
	autocmd FileType haskell highlight rcHfSpace cterm=underline ctermfg=Cyan
augroup END

" Set for extension(*.v)
augroup extension_type
	autocmd BufNewFile,BufRead *.v let &ft='coq'
	autocmd FileType coq execute ':FtCoqInstancyOn'
augroup END

" Plain Text like types
augroup extension_type
	autocmd BufNewFile,BufRead *.md   set filetype=markdown
	autocmd FileType markdown         nnoremap <silent> <leader>r :PrevimOpen<CR>
	autocmd FileType markdown,text    setl ts=2|setl sw=2|setl et
	autocmd FileType git-log.git-diff setl nolist
augroup END

" FileTypes's commentstring
augroup extension_type
	autocmd FileType vim            let &commentstring = ' "%s'
	autocmd FileType c,cpp,java,cs  let &commentstring = " /*%s*/"
	autocmd FileType haskell        let &commentstring = " --%s"
	autocmd FileType coq            let &commentstring = " (*%s*)"
	autocmd FileType markdown,text  let &commentstring = " %s"
augroup END

"}}}


"-------------------------"
"     Ignore Setting      "
"-------------------------"
"{{{

" Special File's Setting is write on here.

function! s:unload_file_event()  " {{{
	augroup file_visit
		autocmd!
	augroup END
endfunction  " }}}
augroup file_event
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

