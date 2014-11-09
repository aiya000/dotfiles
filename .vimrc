scriptencoding utf8

"-------------------
"--  Recipe Menu  --
"-------------------
" -- Parameter
" -- Local_Function
" -- Initialize
" -- Plugin_Manage
" -- Plugin_Configure
" -- View_Setting
" -- Action_Setting
" -- Inner_Setting
" -- Event_Method
" -- Functional_Command
" -- Alias
" -- Key_Map
" -- File_Types
" -- Ignore_Setting
" -- Environment_Pref
" ---



"----------------------------"
"  Target of this config     "
"    - UNIX like OS          "
"    - Cygwin                "
"    - Windows Kaoriya       "
"----------------------------"
"     Eigo - Ingulisshu      "
"----------------------------"
" Ideas {{{

"-- point marker line num in a file
"  -- I can jump marked line and list up mark lines

"-- View prev and next fold head text ...on echo or other buffer ?

" }}}
" Issues {{{

"-- C-o hard use when vimshell

"-- automatic mkdir './C:' when execute NeoBundleInstall in windows kaoriya
"  -- neobundle thinks that is repository...?

"-- color highlight 'var' is not highlight when ...executed vsp|b hoge..?
"  -- What is best event ?

"-- 'gist:aiya000/ec5f6b2375a639831953' cannot divide configure

"-- submode fold_move do not functioned when not exists fold under cursor

"-- not functioned ? conceal-javadoc .

"}}}
" Todo {{{

"-- Eigo to English

"-- read options.jax

"-- Unite outline -> auto view C-Sharp <summary>~</summary>

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

let g:vimrc = get(g:, 'vimrc', {})
let g:vimrc['loaded'] = get(g:vimrc, 'loaded', 0)

let s:isNvim    = has('nvim')

let s:isWindows = has('win32')
let s:isCygwin  = has('win32unix')
let s:isKaoriya = has('kaoriya')
let s:isDosWin  = s:isWindows && !has('gui')
let s:isUnix    = has('unix')
let s:isMac     = has('mac')

let s:hasCygwin = isdirectory('/cygwin')
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
"    Local_Function   "
"---------------------"
"{{{

function! s:system(cmd)
	if exists('*vimproc#system')
		return vimproc#system(a:cmd)
	else
		return system(a:cmd)
	endif
endfunction

"}}}


"-------------------------"
"       Initialize        "
"-------------------------"
" {{{

" Load ftplugin.vim and indent.vim
filetype plugin indent on

" }}}
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

augroup highlight_pref
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
	let switch_dir = $VIM.'/switches/enabled/'
	let suppress = switch_dir . 'disable-vimproc.vim'
	if s:isWindows && !s:hasMingw && filereadable(suppress)
		call delete(suppress)
	elseif s:isWindows && s:hasMingw && !filereadable(suppress)
		call writefile([], suppress)
	endif
	unlet suppress

	for disf in map(['utf-8.vim', 'vimdoc-ja.vim'], "switch_dir . v:val")
		if !filereadable(disf)
			call writefile([], disf)
		endif
	endfor
	unlet switch_dir


	" Unset Kaoriya Preference
	set noignorecase nosmartcase

	augroup file_event
		autocmd BufRead $MYVIMRC setl enc=utf8 fenc=utf8
	augroup END
endif

"}}}
" Check NeoBundle exists {{{
let s:bundleDir    = s:vimHome.'/bundle'
let s:neobundleDir = s:bundleDir.'/neobundle.vim'

if !isdirectory(s:bundleDir)
	call mkdir(s:bundleDir)
endif

function! s:remove_empty_bundledir()  "{{{
	let dirs = split(s:system('ls '.s:bundleDir), '\n')
	for dir in dirs
		let pluginDir = s:bundleDir.'/'.dir
		let isEmpty = s:system('ls '.pluginDir) == ''
		if isEmpty
			call s:system('rmdir '.pluginDir)
		endif
	endfor
endfunction  "}}}
function! s:fetch_neobundle()  " {{{
	if executable('git')
		echo 'NeoBundle was not installed...'
		echo 'Installing NeoBundle.'

		execute '!git clone http://github.com/Shougo/neobundle.vim ' s:neobundleDir
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
			echoerr 'neobundle.vim clone failed.'
			echoerr '>> Vim Config Error <<'
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
"     Plugin_Manage       "
"-------------------------"
"*** Plugin List ***"{{{

NeoBundleFetch   'Shougo/neobundle.vim'
NeoBundleLazy    'basyura/twibill.vim'
NeoBundle        'tyru/open-browser.vim'
NeoBundleLazy    'basyura/bitly.vim'
NeoBundle        'Shougo/unite.vim'
NeoBundle        'Shougo/vimproc.vim'
NeoBundleLazy    'basyura/TweetVim'
NeoBundle        'mattn/webapi-vim'
NeoBundle        'Shougo/vimshell.vim'
NeoBundle        'rhysd/wandbox-vim'
NeoBundle        'thinca/vim-quickrun'
NeoBundleLazy    'kashewnuts/gmail.vim'
NeoBundleLazy    'basyura/J6uil.vim'
NeoBundle        'osyo-manga/vim-gyazo'
NeoBundle        'yuratomo/w3m.vim'
NeoBundle        'mattn/learn-vimscript'
NeoBundleLazy    'rbtnn/vimconsole.vim'
NeoBundle        'supermomonga/thingspast.vim'
NeoBundle        'supermomonga/vimshell-kawaii.vim'
NeoBundle        'mattn/excitetranslate-vim'
NeoBundle        'mattn/unite-advent_calendar'
NeoBundleLazy    'thinca/vim-splash'
NeoBundle        'supermomonga/jazzradio.vim'
NeoBundle        'mattn/favstar-vim'
NeoBundle        'ujihisa/unite-colorscheme'
NeoBundle        'Shougo/vinarise.vim'
NeoBundle        'mattn/gist-vim'
NeoBundle        'thinca/vim-ref'
NeoBundle        'ujihisa/ref-hoogle'
NeoBundleLazy    'vim-jp/vital.vim'
NeoBundle        'Shougo/unite-outline'
NeoBundle        'ebc-2in2crc/vim-ref-jvmis'
NeoBundleLazy    'rbtnn/puyo.vim'
NeoBundleLazy    'mattn/benchvimrc-vim'
NeoBundle        'tacroe/unite-alias'
NeoBundle        'mattn/ideone-vim'
NeoBundleLazy    'mattn/yamada-vim'
NeoBundleLazy    'jvoorhis/coq.vim'
NeoBundleLazy    'eagletmt/coqtop-vim'
NeoBundle        'duff/vim-scratch'
NeoBundle        'rhysd/vim-grammarous'
NeoBundleLazy    'thinca/vim-themis'
NeoBundle        'tomasr/molokai'
NeoBundle        'soramugi/auto-ctags.vim'
NeoBundleDisable 'aiya000/arot13.vim'
NeoBundleDisable 'aiya000/ahoge-put.vim'
NeoBundleLazy    'kannokanno/previm'
NeoBundle        'gist:aiya000/ec5f6b2375a639831953', {
\	'name'        : 'gitlogviewer.vim',
\	'script_type' : 'plugin'
\}
NeoBundle        'kamichidu/vim-vdbc'
NeoBundle        'mattn/vdbi-vim'
NeoBundle        'LeafCage/foldCC'
NeoBundleLazy    'katono/rogue.vim'
NeoBundleDisable 'aiya000/asql.vim'
NeoBundleLazy    'kamichidu/vim-benchmark'
NeoBundle        'kana/vim-submode'
NeoBundle        'gist:aiya000/58931585f8ba6aa43b87', {
\	'name'        : 'conceal-javadoc.vim',
\	'script_type' : 'plugin'
\}
NeoBundle        'mfumi/ref-dicts-en'
NeoBundle        'thinca/vim-painter'
NeoBundle        'osyo-manga/vim-anzu'
NeoBundle        'osyo-manga/vim-over'
NeoBundle        'tyru/restart.vim'
NeoBundle        'koron/minimap-vim'
NeoBundleLazy    'mattn/excelview-vim'
NeoBundle        'glidenote/memolist.vim'
NeoBundle        'vim-jp/vimdoc-ja'
NeoBundle        'mattn/googletranslate-vim'
NeoBundle        'Shougo/vimfiler.vim'
NeoBundleLazy    'rbtnn/game_engine.vim'
NeoBundle        'h1mesuke/vim-alignta'
NeoBundle        'haya14busa/incsearch.vim'
NeoBundle        'thinca/vim-scouter'


call neobundle#end()

try
	helptags ~/.vim/bundle/.neobundle/doc
catch /.*E154.*/
	" Suppressed helptags duplication error
endtry

"}}}
"*** Plugin Depends and Auto Config ***" {{{

let vimproc_config = {
\	'build' : {
\		'unix' : 'make -f make_unix.mak',
\		'mac'  : 'make -f make_mac.mak'
\	}
\}
if s:isCygwin
	let vimproc_config.build['cygwin']  = 'make -f make_cygwin.mak'
elseif s:hasMingw
	let vimproc_config.build['windows'] = 'make -f make_mingw32.mak'
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
\	'autoload' : {'commands' : 'TweetVim'}
\})
call neobundle#config('vimshell.vim', {
\	'depends' : 'Shougo/vimproc.vim'
\})
call neobundle#config('gmail.vim', {
\	'depends'  : 'Shougo/vimproc.vim',
\	'autoload' : {'commands' : 'Gmail'}
\})
call neobundle#config('J6uil.vim', {
\	'depends' : [
\		'mattn/webapi-vim',
\		'Shougo/vimproc.vim',
\		'tyru/open-browser.vim',
\		'Shougo/unite.vim'
\	],
\	'autoload' : {'commands' : 'J6uil'}
\})
call neobundle#config('vim-gyazo', {
\	'depends' : [
\		'tyru/open-browser.vim',
\		'basyura/TweetVim'
\	]
\})
call neobundle#config('vimshell-kawaii.vim', {
\	'depends'  : 'Shougo/vimshell.vim'
\})
call neobundle#config('vimconsole.vim', {
\	'autoload' : {'filetypes' : 'vim'}
\})
call neobundle#config('unite-advent_calendar', {
\	'depends' : 'Shougo/unite-outline'
\})
call neobundle#config('vim-splash', {
\	'autoload' : {'commands' : 'Splash'}
\})
call neobundle#config('jazzradio.vim', {
\	'depends'  : 'Shougo/unite.vim'
\})
call neobundle#config('ref-hoogle', {
\	'depends'  : 'thinca/vim-ref'
\})
call neobundle#config('vital.vim', {
\	'autoload' : {'filetypes' : 'vim'}
\})
call neobundle#config('puyo.vim', {
\	'autoload' : {'commands' : 'Puyo'},
\	'depends'  : 'rbtnn/game_engine.vim'
\})
call neobundle#config('benchvimrc-vim', {
\	'autoload' : {'commands' : 'BenchVimrc'}
\})
call neobundle#config('unite-alias', {
\	'depends' : 'Shougo/unite.vim'
\})
call neobundle#config('yamada-vim', {
\	'autoload' : {'commands' : 'Yamada'}
\})
call neobundle#config('coq.vim', {
\	'autoload' : {'filetypes' : 'coq'}
\})
call neobundle#config('coqtop-vim', {
\	'autoload' : {'filetypes' : 'coq'},
\	'depends'  : 'Shougo/vimproc.vim'
\})
call neobundle#config('vim-grammarous', {
\	'disabled' : !executable('java')
\})
call neobundle#config('vim-themis', {
\	'autoload' : {'filetypes' : 'vim'}
\})
call neobundle#config('previm', {
\	'autoload' : {'filetypes' : 'markdown'}
\})
call neobundle#config('rogue.vim', {
\	'autoload' : {'commands' : [
\		'Rogue',
\		'RogueRestore',
\		'RogueResume',
\		'RogueScores'
\	]}
\})
call neobundle#config('ref-dicts-en', {
\	'depends' : 'thinca/vim-ref'
\})
call neobundle#config('excelview-vim', {
\	'depends'  : 'mattn/webapi-vim',
\	'autoload' : {'commands' : 'ExcelView'}
\})
call neobundle#config('vimfiler.vim', {
\	'depends' : 'Shougo/unite.vim'
\})

" }}}


"------------------------"
"*** Plugin_Configure ***"
"------------------------"
"--- netrw ---"{{{

augroup plugin_pref
	autocmd FileType netrw setl ignorecase
augroup END

"}}}
"--- vim-quickrun ---" {{{

let g:quickrun_config = {
\	'_' : {
\		'split'  : '',
\		'runner' : exists('*vimproc#system') ? 'vimproc' : 'system',
\		'runner/vimproc/updatetime' : 10,
\		'hook/time/enable' : 1
\	},
\	'cpp' : {
\		'command' : 'g++',
\		'cmdopt'  : '-I/usr/include/c++/4.9 -I/usr/include/c++/4.9/x86_64-linux-gnu -std=c++11'
\	},
\	'java' : {
\		'cmdopt' : '-source 1.8',
\		'runner' : exists('*vimproc#system') ? 'process_manager' : 'system'
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
	autocmd FileType quickrun setl wrap
augroup END

" }}}
"--- vimproc.vim ---"{{{

if s:isWindows && !s:hasMingw
	"NeoBundleDisable 'Shougo/vimproc.vim'
	"@Incompleted('I should use a like NeoBundleDisable ... ?')
	set runtimepath-=~/.vim/bundle/vimproc.vim/
endif

" }}}
"--- TweetVim ---"{{{

let g:tweetvim_async_post = 1

augroup plugin_pref
	autocmd FileType         tweetvim     setl wrap
	autocmd FileType         tweetvim_say setl ts=2 sw=2 et
augroup END

"}}}
"--- vimshell.vim ---"{{{

" Add to VimShell Commands Directory of My Home
let &runtimepath = &runtimepath.','.s:vimHome.'/autoload/vimshell/commands'


let g:vimshell_no_save_history_commands = {
\	'history': 1,
\	'ls'     : 1,
\	'clear'  : 1
\}
let g:vimshell_enable_transient_user_prompt = 1
let g:vimshell_force_overwrite_statusline = 1


augroup plugin_pref
	autocmd FileType vimshell setl fdm=marker nolist wrap
augroup END

"}}}
"--- J6uil ---"{{{

augroup plugin_pref
	autocmd FileType J6uil setl wrap
augroup END

"}}}
"--- vimshell-kawaii.vim ---"{{{

let g:vimshell_kawaii_smiley = 1

"}}}
"--- w3m.vim ---"{{{

let g:w3m#homepage = 'http://www.google.co.jp/'

"}}}
"--- vimconsole.vim ---"{{{

let g:vimconsole#auto_redraw = 1

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
\		'args'   : '~/Documents/workspace/Java/src'
\	}
\}

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
"call submode#enter_with('incsearch_command', 'c', '', '<C-h><C-p>', '<Up>')
"call submode#enter_with('incsearch_command', 'c', '', '<C-h><C-n>', '<Down>')
"call submode#map('incsearch_command', 'c', '', '<C-p>', '<Up>')
"call submode#map('incsearch_command', 'c', '', '<C-n>', '<Down>')

" Window Buffer Changer
call submode#enter_with('buffer_change', 'n', '', '<C-s>b')
call submode#map('buffer_change', 'n', 's', 'n', ':bnext<CR>')
call submode#map('buffer_change', 'n', 's', 'p', ':bprevious<CR>')

"}}}
"--- vim-ref ---" {{{

let g:ref_use_vimproc = 1

" }}}
"--- ref-dicts-en ---" {{{
"@See('http://d.hatena.ne.jp/akishin999/20131024/1382569289')

let g:ref_source_webdict_sites = {
\	'weblio' : {
\		'url' : 'http://ejje.weblio.jp/content/%s'
\	},
\	'wikipedia' : {
\		'url' : 'http://ja.wikipedia.org/wiki/%s'
\	}
\}

let g:ref_source_webdict_sites['default'] = 'weblio'

function! s:webdict_filter(output)
	return join(split(a:output, "\n")[56 : ], "\n")
endfunction
let g:ref_source_webdict_sites['weblio'].filter = function('s:webdict_filter')

" }}}
"--- restart.vim ---" {{{

let g:restart_sessionoptions = 'blank,curdir,folds,help,localoptions,tabpages'

" }}}
"--- memolist.vim ---"{{{

let g:memolist_path = expand('~/.memolist')
if !isdirectory(g:memolist_path)
	call mkdir(g:memolist_path)
endif
let g:memolist_memo_suffix = 'md'
let g:memolist_prompt_tags = 1
let g:memolist_prompt_categories = 1
let g:memolist_unite = 1
let g:memolist_unite_option = '-auto-preview -tab'

"}}}
"--- vimfiler.vim ---"{{{

if !s:isDosWin
	let g:vimfiler_tree_opened_icon = '▾'
	let g:vimfiler_tree_closed_icon = '▸'
	let g:vimfiler_marked_file_icon = '*'
endif
let g:vimfiler_file_icon = ' '
let g:vimfiler_ignore_pattern = ''

"}}}
"--- For Private ---"{{{

" Read Privacy Config
if filereadable(expand('~/.vimrc_private'))
	source ~/.vimrc_private
endif

"}}}

"-------------------------"
"      View_Setting       "
"-------------------------"
"{{{

" Highlight enable
syntax enable

" Set Basic Preferences
set number nowrap hlsearch list scrolloff=6
let s:listchars = s:isDosWin
	\	? 'tab:> ,trail:_,extends:>,precedes:<,nbsp:%'
	\	: 'tab:› ,trail:_,extends:»,precedes:«,nbsp:%,eol:↲'

" Status Bar always displayed
set laststatus=2

" Status Bar format $ @See('http://sourceforge.jp/magazine/07/11/06/0151231')
set statusline=%F%m\%=[FileType=%y][Format=%{&ff}]

" ☆ Fix View 2byte Code (Not support gnome-terminal)
set ambiwidth=double

" Powered Up Syntax Highlight
" {{{

augroup highlight_pref
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

	autocmd ColorScheme       * highlight RcEmSpace cterm=standout ctermfg=LightBlue
	autocmd VimEnter,WinEnter * call matchadd('RcEmSpace', '　')
augroup END

augroup highlight_pref
	autocmd InsertEnter * highlight StatusLine ctermfg=Black ctermbg=Cyan
	autocmd InsertLeave * highlight StatusLine ctermfg=Cyan  ctermbg=Black
augroup END

nohlsearch

" }}}

" Set Color Scheme
set background=dark
colorscheme desert

" Indent Wrapped Text
if exists('+breakindent')
	set breakindent linebreak
endif

" View cursor column on <C-g>
set noruler

" Hard Conceal
set conceallevel=2

" Spatto view tabline $ @See('http://d.hatena.ne.jp/thinca/20111204/1322932585')
function! s:tabpage_label(n) "{{{
	let title = gettabvar(a:n, 'title')
	if title !=# ''
		return title
	endif

	let bufnrs = tabpagebuflist(a:n)
	let hi = a:n is tabpagenr() ? '%#TabLineSel#' : '%#TabLine#'

	let no = len(bufnrs)
	if no is 1
		let no = ''
	endif

	let mod = len(filter(copy(bufnrs), 'getbufvar(v:val, "&modified")')) ? '+' : ''
	let sp = (no . mod) == '' ? '' : ' '

	let curbufnr = bufnrs[tabpagewinnr(a:n) - 1]
	let fname = pathshorten(bufname(curbufnr))
	if fname == ''
		let fname .= '[ NoName ]'
	endif

	let label = no . mod . sp . fname
	return '%' . a:n . 'T' . hi . label . '%T%#TabLineFill#'
endfunction "}}}
function! WithDelimitterTabLine() "{{{
	let titles     = map(range(1, tabpagenr('$')), 's:tabpage_label(v:val)')
	let delimitter = ' | '
	let tabpages   = delimitter . join(titles, delimitter) . delimitter . '%#TabLineFill#%T'
	return tabpages
endfunction "}}}
set tabline=%!WithDelimitterTabLine()

" Always view the changed line num in Ex-command
set report=0

"}}}


"-------------------------"
"     Action_Setting      "
"-------------------------"
"{{{

" Set Compatibility with vi is off
set nocompatible

" Set Backspace can delete empty line
if v:version < 704  " Is this suitable condition ?
	set whichwrap=b,s,h,l,<,>,[,]
	set backspace=indent,eol,start
endif

" Input Sets
set textwidth=0 tabstop=4 shiftwidth=4

" C Type Auto Indent on
set autoindent cindent

" Incremental Searching
"set incsearch

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
set undofile
let &undodir = s:undodir

" Bell Sound is instead of Screen flash.
set visualbell

" Disable Auto Commentalize New Line
set formatoptions-=ro

" Ignore case on Insert completion
set noinfercase

" No timeout key maps
set notimeout

"}}}


"-------------------------"
"     Inner_Setting       "
"-------------------------"
"{{{


" Default File Encoding
set fileencoding=utf-8 encoding=utf-8

" Auto Judge file encode
set fileencodings=utf-8,sjis,euc-jp,cp932,ucs-bom,iso-2022-jp-3,iso-2022-jp,eucjp-ms,euc-jisx0213,ucs-bom,latin1,default

" Leaving a history and it limit is a 500 pieces
set history=500

" Adding Runtime Path
set runtimepath+=~/.vim/vimball
set runtimepath+=~/.vim/makes/asql.vim
set runtimepath+=~/.vim/makes/arot13.vim/
set runtimepath+=~/.vim/makes/ahoge-put.vim/

" Set Vimball Install place
let g:vimball_home = s:vimHome.'/vimball'

" Display Command Complement
set wildmenu

" Path Delimiter is Slash
set shellslash

" Add Match Pairs
set matchpairs+=<:>,?::

" Load Target for ctags
set tags=./tags,~/tags

" Explore wake up default dir
set browsedir=buffer

" Set spell lang
if exists('+spelllang')
	set spelllang=en_US
endif

" Generate HelpTags My Help
if isdirectory('~/.vim/doc')
	helptags ~/.vim/doc
endif

"}}}


"-------------------------"
"      Event_Method       "
"-------------------------"
"{{{

" Save Cursor Position when file closed
augroup file_visit
	autocmd BufWinLeave ?\+ silent if expand('%') !=? '' | mkview   | endif
	autocmd BufWinEnter ?\+ silent if expand('%') !=? '' | loadview | endif
augroup END


" Powered Up Auto File Backup when written
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

	autocmd VimEnter,WinEnter,BufWinEnter,BufRead,EncodingChanged *
		\	if &encoding == 'utf-8'
		\|		let &listchars = s:listchars
		\|	else
		\|		let &listchars = 'tab:> ,trail:_,extends:>,precedes:<,nbsp:%'
		\|	endif
augroup END

augroup key_event
	"autocmd UserGettingBored * echo "What's this !?"

	" Set ignorecase completion only insert mode
	autocmd InsertEnter * setl ignorecase
	autocmd InsertLeave * setl noignorecase
augroup END

"}}}

"-------------------------"
"   Functional_Command    "
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
		if a:firstline is a:lastline
			return
		endif
		let lines = []
		let posit = getpos('.')
		for line in range(a:firstline, a:lastline)
			call add(lines, substitute(getline(line)."\n", "\t", "", 'g'))
		endfor

		for i in range(a:firstline, a:lastline)
			execute 'normal! "_dd'
			execute 'normal! i' lines[a:lastline - i]
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
command! -nargs=1 PutRandom execute 'normal! a' . s:random_int(<q-args>)


" Time Watcher  $ @See('http://leafcage.hateblo.jp/entry/2013/08/02/001600')
command! TimerStart let  s:startTime = reltime()
command! TimerEcho  echo reltimestr( reltime(s:startTime) )
command! TimerPut   execute 'normal! o' . reltimestr(reltime(s:startTime))


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
		echoerr 'No such temporary root dir'
	endif
endfunction "}}}
command! -nargs=1  TDirSet call s:set_temporary_dir(<q-args>)
command! TDirSetCurrentDir call s:set_temporary_dir('.')
function! s:cd_temporary_dir() "{{{
	if g:rc_temporary_dir ==# 'undefined'
		echoerr 'Not set temporary root dir'
	else
		execute 'cd ' g:rc_temporary_dir
		echo g:rc_temporary_dir
	endif
endfunction "}}}
command! TDirCd            call s:cd_temporary_dir()

" }}}
" Development Support {{{

" If use *NIX then use QuickRun else if cannot use it, you can use this.
function! s:java_run_func() "{{{
	let l:javaname = expand('%:t:r')
	let l:javav = s:system('java -version')
	if l:javav =~# "1\.8"
		let l:command  = ['javac -source 1.8 -encoding utf8', 'java']
	elseif l:javav =~# "1\.7"
		let l:command  = ['javac -source 1.7 -encoding utf8', 'java']
	else
		let l:command  = ['javac -encoding utf8', 'java']
	endif
	if s:isCygwin
		if executable('cocot')
			let l:command[0] = 'cocot ' . l:command[0]
			let l:command[1] = 'cocot ' . l:command[1]
		else
			echo 'You must be get [cocot] command.'
			return
		endif
	endif

	execute '!'.
	\	printf('%s %s.java',   l:command[0], l:javaname) . ';' .
	\	printf('%s %s',        l:command[1], l:javaname) . ';'
	call delete(l:javaname . '.class')
endfunction "}}}
command! JavaRun call s:java_run_func()

function! s:put_python_import_for_jp() "{{{
	let paste = &paste
	set paste
	execute 'normal! O' "#!/usr/bin/env python"
	execute 'normal! o' "# -*- coding: utf-8 -*-"
	execute 'normal! o' "import sys"
	execute 'normal! o' "import codecs"
	execute 'normal! o' "sys.stdout = codecs.getwriter('utf_8')(sys.stdout)"
	let &paste = paste
endfunc "}}}
command! ImportPythonJp call s:put_python_import_for_jp()


command! PutShortSeparator
	\	execute 'normal! a' '/* -=-=-=-=-=-=-=-=- */'
	\|	execute 'normal =='
command! PutLongSeparator
	\	execute 'normal! a' '/* ---===---===---===---===---===---===--- */'
	\|	execute 'normal =='
command! Date
	\	execute 'normal! a' .
	\	substitute(s:isUnix ? system('date') : system('echo %date% %time%'), "\n", "", 'g')


function! s:put_html_base() "{{{
	execute 'normal! O' '<html lang="ja">'
	execute 'normal! o' '<head>'
	execute 'normal! o' '<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">'
	execute 'normal! o' '<title></title>'
	execute 'normal! o' '</head>'
	execute 'normal! o' '<body>'
	execute 'normal! o'
	execute 'normal! o' '</body>'
	execute 'normal! o' '</html>'
endfunction "}}}
command! PutHtmlBase call s:put_html_base()


"@Incompleted('"+" deleted in sql sytax')
"@Code('Select it, and execute this.')
" {{{
"  $ "SELECT *" +
"  $ " FROM table;";
"  Yank => SELECT * FROM table; 

function! s:sql_yank_normalize() range 
	let sql = ""
	for i in range(a:firstline, a:lastline)
		let line = getline(i)
		let lineOfSql = substitute(substitute(
		\		substitute(line, "\"", "", 'g'),
		\	"\v(^\s*+|+\s*$)", "", 'g'), "\t", "", 'g')

		let sql .= lineOfSql . "\n"
	endfor
	let @" = substitute(sql, "\s\s\+", " ", 'g')
endfunction
" }}}
command! -range SqlCopy :<line1>,<line2>call s:sql_yank_normalize()

" }}}


"-------------------------"
"          Alias          "
"-------------------------"
" Utils {{{

" Vim Utils {{{
command! VimConfig         e  $MYVIMRC
command! VimConfigTab      tabnew | e $MYVIMRC
command! Reload            so $MYVIMRC
	\|	if has('gui_running')
	\|		so $MYGVIMRC
	\|	endif
command! ForceSave     w !sudo tee > /dev/null %

command! CdBufDir          cd %:p:h
command! Resetf            let &ft = &ft  " for Event [FileType * ]

command! ColorPreview      Unite colorscheme -auto-preview

" }}}
" Twitter {{{

"Note: if ask more, add hooks
"    (let g:vimrc.private['twitter']['curr_ac'] = ~~)
"    on tweetvim's TweetVimSwitchAccount command.

"-- Basic --"
command! Twitter            TweetVimHomeTimeline
command! TwitterTab         tabnew | Twitter
command! Tweet              TweetVimSay


"-- Private Account --"
function! TwitterPrivateFunc() "{{{
	execute ':TweetVimSwitchAccount ' g:vimrc.private['twitter']['priv_ac']
	let g:vimrc.private['twitter']['curr_ac'] = g:vimrc.private['twitter']['priv_ac']
	TweetVimHomeTimeline
endfunction "}}}
command! TwitterPrivate     call TwitterPrivateFunc()
command! TwitterPrivateTab  tabnew | TwitterPrivate
function! TweetPrivateFunc() "{{{
	execute ':TweetVimSwitchAccount ' g:vimrc.private['twitter']['priv_ac']
	TweetVimSay
	"@Incompleted('wait here')
	"execute ':TweetVimSwitchAccount ' g:vimrc.private['twitter']['curr_ac']
endfunction "}}}
command! TweetPrivate       call TweetPrivateFunc()


"-- Public Account --"
function! TwitterPublicFunc() "{{{
	execute ':TweetVimSwitchAccount ' g:vimrc.private['twitter']['publ_ac']
	let g:vimrc.private['twitter']['curr_ac'] = g:vimrc.private['twitter']['publ_ac']
	TweetVimHomeTimeline
endfunction "}}}
command! TwitterPublic      call TwitterPublicFunc()
command! TwitterPublicTab   tabnew | TwitterPublic
function! TweetPublicFunc() "{{{
	execute ':TweetVimSwitchAccount ' g:vimrc.private['twitter']['publ_ac']
	TweetVimSay
	"@Incompleted('wait here')
	"execute ':TweetVimSwitchAccount ' g:vimrc.private['twitter']['curr_ac']
endfunction "}}}
command! TweetPublic        call TweetPublicFunc()


command! Bitly TweetVimBitly
cabbr    tvs   TweetVimSwitchAccount

" }}}

" To Service Name
cabbr Lingr J6uil

" Beautifull Life
command! JazzUpdate    JazzradioUpdateChannels
command! JazzList      Unite jazzradio
cabbr    JazzPlay      JazzradioPlay
command! JazzStop      JazzradioStop

" Translates Languages
command!          Translate     ExciteTranslate
function! s:weblio_translate_cmdline(...) "{{{
	let l:line = ''
	for word in a:000
		let l:line .= word . '+'
	endfor

	execute 'Ref webdict weblio ' l:line
endfunction "}}}
command! -nargs=* Weblio        call s:weblio_translate_cmdline(<f-args>)

command! -nargs=* GrepNow       vimgrep <f-args> % | cwindow
command!          MinimapReSync execute 'MinimapStop' | execute 'MinimapSync'

" }}}
" Developments {{{

command! -nargs=1 Log VimConsoleLog <args>
command! LogClear VimConsoleClear

command! -nargs=*  Ghc      !runghc % <q-args>
command!           Ghci     enew!|VimShellInteractive ghci
command!           Sghci    sp|VimShellInteractive ghci
command!           Vghci    vsp|VimShellInteractive ghci
command!           GhciTab  tabnew|VimShellInteractive ghci
command! -nargs=1  Hoogle Ref hoogle <args>

" }}}


"-------------------------"
"         Key_Map         "
"-------------------------"
"--- Roles ---"{{{

"* <leader> is utility type key map

"* <C-k> is Primary prefix key
"  - Use for be big frequency of operation

"* <C-h> is Secondary prefix key
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
	autocmd FileType * cnoremap <C-k><C-k> <C-\>e getcmdpos() < 2 ?'':getcmdline()[:getcmdpos()-2]<CR>

	" for incsearch.vim
	autocmd FileType * IncSearchNoreMap <C-j> <CR>
	autocmd FileType * IncSearchNoreMap <C-b> <Left>
	autocmd FileType * IncSearchNoreMap <C-f> <Right>
	autocmd FileType * IncSearchNoreMap <C-a> <Home>
	autocmd FileType * IncSearchNoreMap <C-h> <Backspace>
	autocmd FileType * IncSearchNoreMap <C-d> <Del>
	autocmd FileType * IncSearchNoreMap <C-e> <End>
augroup END

" }}}
" Customize Keys {{{

" To All buffers
augroup key_map
	" ✠ God Of The Vim
	autocmd FileType * nnoremap Q gQ

	"-- Overwrite mapping --"
	autocmd FileType * nnoremap <C-n> gt
	autocmd FileType * nnoremap <C-p> gT
	autocmd FileType * nnoremap zl    8zl
	autocmd FileType * nnoremap zh    8zh
	autocmd FileType * inoremap <C-l> <Esc>
	autocmd FileType * vnoremap <C-l> <Esc>
	autocmd FileType * cnoremap <C-l> <Esc>

	"-- With Prefixes --"
	" for case duplicated maps by plugin map (ex:vimshell => <C-l> : clean)
	autocmd FileType * nnoremap                  <C-k><C-n> gt
	autocmd FileType * nnoremap                  <C-k><C-p> gT
	autocmd FileType * nnoremap <silent><expr>   <C-k><C-s> ':OverCommandLine<CR>%s/\<' . expand('<cword>') . '\>/'
	autocmd FileType * inoremap                  <C-k><C-l> <Esc>
	autocmd FileType * inoremap                  <C-k><C-k> <C-o>"_d$
	autocmd FileType * inoremap                  <C-k><C-z> <C-o>:normal! <C-z><CR>
	" Customize with prefix
	autocmd FileType * nnoremap <silent>         <C-k><C-u><C-f> :Unite -ignorecase outline:foldings<CR>
	autocmd FileType * nnoremap <silent>         <C-k><C-u><C-m> :Unite mapping<CR>
	autocmd FileType * nnoremap <silent>         <C-k><C-u><C-b> :Unite -ignorecase buffer<CR>
	autocmd FileType * cnoremap                  <C-k><C-p>      <Up>
	autocmd FileType * cnoremap                  <C-k><C-n>      <Down>

	autocmd FileType * nnoremap <silent>         <C-h><C-r>      :Reload<CR>
	autocmd FileType * nnoremap <silent>         <C-h><C-l>      :noh<CR>
	autocmd FileType * nnoremap <silent>         <C-h>l          :so %<CR>
	autocmd FileType * nnoremap <silent>         <C-h>r          :Resetf<CR>
	autocmd FileType * nnoremap <silent>         <C-h><C-w>      :setl wrap! wrap?<CR>
	autocmd FileType * nnoremap <silent>         <C-h><C-Space>  :let __t=@/<CR>:s/\s\s\+/ /g<CR>:exe 'norm! =='<CR>:noh<CR>:let @/=__t<CR>:unlet __t<CR>
	autocmd FileType * nnoremap <silent>         <C-h><C-p><C-l> :PutLongSeparator<CR>
	autocmd FileType * nnoremap <silent>         <C-h><C-p><C-s> :PutShortSeparator<CR>

	"-- Customize --"
	autocmd FileType * nnoremap <silent> <C-m> :normal! o<CR>
	autocmd FileType * nnoremap          q:    :digraphs<CR>   " Oh, digraphs!
	" for window or buffer
	autocmd FileType * nnoremap <silent> <C-w>t     :tabnew<CR>
	autocmd FileType * nnoremap <silent> <C-w>T     :tabclose<CR>
	autocmd FileType * nnoremap <silent> <C-w>bd    :bd<CR>
	autocmd FileType * nnoremap <silent> <C-w>Bd    :bd!<CR>
	autocmd FileType * nnoremap <silent> <C-w><C-w> :w<CR>
	function! s:buf_open_new_tab() "{{{
		let l:lnum = line('.')
		execute 'tabnew| ' bufnr('%') . 'b'
		execute 'normal! ' l:lnum . 'Gzvzz'
	endfunction "}}}
	autocmd FileType * nnoremap <silent> <C-w>bt    :call <SID>buf_open_new_tab()<CR>
	autocmd FileType * nnoremap <silent> <C-w>N     :enew!<CR>
	" for folds
	autocmd FileType * nnoremap <expr> h foldclosed('.') > -1 ? 'zo' : 'h'
	autocmd FileType * nnoremap <expr> l foldclosed('.') > -1 ? 'zo' : 'l'
	autocmd FileType * nnoremap zj     zjzo
	autocmd FileType * nnoremap zk     zkzo

	"-- With Plugins --"
	autocmd FileType * nmap              <leader>w <Plug>(openbrowser-open)
	autocmd FileType * nnoremap <silent> <leader>t :Translate<CR>
	" Visualize substitute target and new word (Increase Gravity!!)
	autocmd FileType * nnoremap <silent> :%s/               :OverCommandLine<CR>%s/
	autocmd FileType * nnoremap <silent> :s/                :OverCommandLine<CR>s/
	autocmd FileType * vnoremap <silent> :s/                :OverCommandLine<CR>s/
	" for vimshell
	autocmd FileType * nnoremap <silent> <leader>v          :VimShell -split-command=vsp -toggle<CR>
	autocmd FileType * nnoremap <silent> <leader><leader>v  :VimShell -split-command=sp  -toggle<CR>
	autocmd FileType * nnoremap <silent> <leader>V          :VimShellBufferDir   -create<CR>
	autocmd FileType * nnoremap <silent> <leader><leader>V  :tabnew<CR>:VimShell -create<CR>
	" for vimfiler
	autocmd FileType * nnoremap <silent> <leader>e          :VimFilerExplorer  -status -parent -toggle<CR>
	autocmd FileType * nnoremap <silent> <leader><leader>e  :VimFilerBufferDir -status -split -horizontal -force-quit<CR>
	autocmd FileType * nnoremap <silent> <leader>E          :VimFilerBufferDir -status -force-quit<CR>
	autocmd FileType * nnoremap <silent> <leader><leader>E  :VimFilerBufferDir -status -tab -force-quit<CR>
	" for anzu-chan
	autocmd FileType * nmap              n                  <Plug>(anzu-n-with-echo)zv
	autocmd FileType * nmap              N                  <Plug>(anzu-N-with-echo)zv
	autocmd FileType * nmap              *                  <Plug>(anzu-star-with-echo)zv
	autocmd FileType * nmap              #                  <Plug>(anzu-sharp-with-echo)zv
	autocmd FileType * nmap              <C-w>*             <C-w><C-v><Plug>(anzu-star-with-echo)zv
	autocmd FileType * nmap              <C-w>#             <C-w><C-v><Plug>(anzu-sharp-with-echo)zv
	" for incsearch.vim
	autocmd FileType * nmap <expr>       /                  foldclosed('.') > -1 ? 'zv<Plug>(incsearch-forward)'  : '<Plug>(incsearch-forward)'
	autocmd FileType * nmap <expr>       ?                  foldclosed('.') > -1 ? 'zv<Plug>(incsearch-backward)' : '<Plug>(incsearch-backward)'
	autocmd FileType * nmap <expr>       g/                 foldclosed('.') > -1 ? 'zv<Plug>(incsearch-stay)'     : '<Plug>(incsearch-stay)'
augroup END


augroup key_map
	function! s:cursor_move_to_center() "{{{
		execute 'normal! 0'
		for i in range(strlen(getline('.'))/2)
			execute 'normal! l'
		endfor
	endfunction "}}}
	autocmd FileType * nnoremap <silent> gc :call <SID>cursor_move_to_center()<CR>
	function! s:cursor_up_to_lid() "{{{
		while 1
			let l:p = getpos('.')[2]
			execute 'normal! k'

			let l:isIndentChanged = l:p != getpos('.')[2]
			if l:isIndentChanged || line('.') is 1
				if l:isIndentChanged | execute 'normal! j' | endif
				break
			endif
		endwhile
	endfunction "}}}
	autocmd FileType * nnoremap <silent> gk :call <SID>cursor_up_to_lid()<CR>
	function! s:cursor_down_to_ground() "{{{
		while 1
			let l:p = getpos('.')[2]
			execute 'normal! j'

			let l:isIndentChanged = l:p != getpos('.')[2]
			if l:isIndentChanged || line('.') is line('$')
				if l:isIndentChanged | execute 'normal! k' | endif
				break
			endif
		endwhile
	endfunction "}}}
	autocmd FileType * nnoremap <silent> gj :call <SID>cursor_down_to_ground()<CR>
	"autocmd FileType * xnoremap <silent> gk :call <SID>cursor_up_to_lid()<CR>
	"autocmd FileType * xnoremap <silent> gj :call <SID>cursor_down_to_ground()<CR>
	function! s:toggle_case() "{{{
		let pos  = getpos('.')

		let word     = expand('<cword>')
		let is_snake = word =~? '_' ? 1 : 0

		execute 'normal! b'
		if expand('<cword>') !=# word
			execute 'normal! w'
		endif

		if is_snake
		else
			execute 'normal! ce' . substitute(word, '[A-Z]', '_\l\0', 'g')
		endif

		call setpos('.', pos)
	endfunction "}}}
	autocmd FileType * nnoremap <silent> <C-k><C-Space> :call <SID>toggle_case()<CR>

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
	autocmd FileType * nnoremap <silent> <C-h>jkjkjkjk :call <SID>enable_cursor_keys_toggle()<CR>
	let s:visualFoldToggle = get(s:, 'visualFoldToggle', 0) "{{{
	function! s:visual_fold_all()
		if mode() =~# "^[vV\<C-v>]"
			if !s:visualFoldToggle && &foldenable
				set nofoldenable
				execute 'normal! zz'
				let s:visualFoldToggle = 1
			endif
		else
			if s:visualFoldToggle
				set foldenable
				execute 'normal! zz'
				let s:visualFoldToggle = 0
			endif
		endif
	endfunction "}}}
	autocmd CursorMoved * call s:visual_fold_all()

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
	autocmd FileType * nnoremap <silent> <leader>b  :ScratchUp<CR>
	autocmd FileType * nnoremap <silent> <leader>B  :EmptyBufUp<CR>
augroup END

" }}}

"}}}
" Buffer Local KeyMaps {{{

" To Plugin buffers
augroup plugin_pref
	autocmd FileType help nnoremap <buffer> Q :q<CR>

	autocmd FileType netrw nmap             <buffer> H -
	autocmd FileType netrw nnoremap <silent><buffer> Q :q<CR>
	autocmd FileType netrw nnoremap         <buffer> L G

	autocmd FileType quickrun nnoremap <silent> Q :q<CR>

	autocmd FileType tweetvim     nmap             <buffer> <leader>R <Plug>(tweetvim_action_remove_status)
	autocmd FileType tweetvim     nmap             <buffer> <C-r>     <Plug>(tweetvim_action_reload)
	autocmd FileType tweetvim     nnoremap <silent><buffer> s         :TweetVimSay<CR>
	autocmd FileType tweetvim     nnoremap         <buffer> <C-a>     :TweetVimSwitchAccount<Space>
	autocmd FileType tweetvim     nnoremap         <buffer> U         :TweetVimUserTimeline<Space>
	autocmd FileType tweetvim     nnoremap <silent><buffer> Q         :bd<CR>
	autocmd FileType tweetvim_say inoremap         <buffer> <C-i>     <C-i>

	autocmd FileType vimshell nunmap   <buffer> Q
	autocmd FileType vimshell nunmap   <buffer> q
	autocmd FileType vimshell nnoremap <buffer> gk <NOP>
	autocmd FileType vimshell nnoremap <buffer> gj <NOP>
	autocmd FileType vimshell imap     <buffer> <C-l>      <Plug>(vimshell_clear)
	autocmd FileType vimshell imap     <buffer> <C-k><C-p> <Plug>(vimshell_history_unite)
	autocmd FileType vimshell iunmap   <buffer> <C-p>
	autocmd FileType vimshell iunmap   <buffer> <C-n>
	autocmd FileType int-*    nmap     <buffer> Q          <Plug>(vimshell_int_exit)
	autocmd FileType int-*    imap     <buffer> <C-l>      <C-o><C-l>
	autocmd FileType int-*    imap     <buffer> <C-k><C-p> <Plug>(vimshell_int_history_unite)

	autocmd FileType vimfiler nmap         <buffer> <C-j> <Plug>(vimfiler_cd_or_edit)
	autocmd FileType vimfiler nmap         <buffer> <C-h> <C-h>
	autocmd FileType vimfiler nmap         <buffer> h     <Plug>(vimfiler_expand_or_edit)
	autocmd FileType vimfiler nmap         <buffer> l     <Plug>(vimfiler_expand_or_edit)
	autocmd FileType vimfiler nmap <silent><buffer> H     <Plug>(vimfiler_pushd)ggk<CR>
	autocmd FileType vimfiler nmap <silent><buffer> L     <Plug>(vimfiler_popd)<CR><Plug>(vimfiler_popd)d:q<CR>

	autocmd FileType w3m nnoremap         <buffer> H         <BS>
	autocmd FileType w3m nnoremap <silent><buffer> <C-u>     :W3mAddressBar <CR>
	autocmd FileType w3m nnoremap <silent><buffer> <leader>E :W3mShowExtenalBrowser <CR>

	autocmd FileType J6uil     nnoremap <silent><buffer> Q     :bd<CR>
	autocmd FileType J6uil_say nmap             <buffer> <C-j> <CR>  " Enter to Say

	autocmd FileType git-log.git-diff nnoremap <silent><buffer> Q         :bd<CR>
	autocmd FileType markdown         nnoremap <silent><buffer> <leader>r :PrevimOpen<CR>
augroup END

" }}}


"-------------------------"
"        File_Types       "
"-------------------------"
"{{{

" If buffer doesn't has filetype then set filetype 'none'
augroup file_event
	autocmd VimEnter,BufNew * if &ft == '' | setf none | endif
augroup END


"@Incompleted('when window split not functioned')
" Set for "Vi Improved"
augroup extension_type
	autocmd FileType *   highlight RcMyHint cterm=standout ctermfg=DarkYellow
	autocmd FileType vim call matchadd('RcMyHint', '\s*"\zs@\w\+\ze')
augroup END

" Set for C-Sharp
augroup extension_type
	autocmd FileType *  highlight TypeInference cterm=bold ctermfg=11
	autocmd FileType cs call matchadd('TypeInference', '\<var\>')
augroup END

" Set for Haskell
augroup extension_type
	autocmd FileType  *       highlight RcHeadHfSpace cterm=underline ctermfg=Cyan
	autocmd FileType  haskell call matchadd('RcHeadHfSpace', '^\s\+')
	autocmd FileType  yesod   setl ts=4 sw=4 et
augroup END

" Plain Text like types
augroup extension_type
	autocmd FileType markdown,text    setl ts=2 sw=2 et
	autocmd FileType git-log.git-diff setl nolist
augroup END

" FileTypes commentstrings
augroup extension_type
	autocmd FileType vim                let &commentstring = ' "%s'
	autocmd FileType c,cpp,java,cs      let &commentstring = " /*%s*/"
	autocmd FileType haskell            let &commentstring = " --%s"
	autocmd FileType coq                let &commentstring = " (*%s*)"
	autocmd FileType markdown,text,none let &commentstring = " %s"
augroup END

"}}}


"-------------------------"
"     Ignore_Setting      "
"-------------------------"
"{{{
" Special File's Setting is write on here.

function! s:unload_file_visit()  " {{{
	augroup file_visit
		autocmd!
	augroup END
endfunction  " }}}
augroup file_event
	"@Bugs('not functioned ?')
	autocmd FileType vimshell call s:unload_file_visit()
	autocmd FileType vimfiler call s:unload_file_visit()
augroup END

"}}}


"-------------------------"
"    Environment_Pref     "
"-------------------------"
"{{{

if filereadable(expand('~/.vimrc_env'))
	source ~/.vimrc_env
endif

"}}}


let g:vimrc['loaded'] = 1

