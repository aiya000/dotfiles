scriptencoding utf8

"-------------------
"--  Recipe Menu  --
"-------------------
" -- Parameter
" -- Local_Function
" -- Initialize
"
" -- Plugin_Manage
"
"
" -- Plugin_Configure
" -- View_Setting
" -- Action_Setting
" -- Inner_Setting
" -- Event_Method
" -- Functional_Command
" -- Command_Alias
" -- Key_Mappings
" -- File_Types
" -- Environment_Pref
" ---



"----------------------------"
"  Target of this config     "
"    - UNIX Type             "
"    - Cygwin                "
"    - Windows Kaoriya GVim  "
"----------------------------"
"     Eigo - Ingulisshu      "
"----------------------------"
" Ideas {{{

"-- View prev and next fold head text ...on echo or other buffer ? on submode-foldings

"-- <C-/> alter key [asdfghjkl;] to [1234567890]

"-- Unite outline -> view C-Sharp <summary>~</summary>

" }}}
" Issues {{{

"-- C-o hard use when vimshell

"-- automatic mkdir './C:' when execute NeoBundleInstall in windows kaoriya
"  -- neobundle thinks that is repository...?
"
"-- 'gist:aiya000/ec5f6b2375a639831953' cannot divide configure

"-- does not functioned conceal-javadoc ?

"-- shot-f not functioned in <C-o> temporary normal mode

"-- couldn't auto make vimproc at anywhere

"-- happend error when opened vrapperrc or vimperatorrc

"-- lost highlight of buffer when delete other buffer

"-- conflicted? vim-ruby and rspec.vim

"-- not loaded rspec.vim on load filetype 'ruby'

"}}}
" Todo {{{

"-- Eigo to English

"-- read help options.jax

"-- read help windows.txt

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
" @Marked       => I have eye on this.
" @See          => Referred URL, Saw Document, and etc...
" @Code         => A sample code using it
"-------------------
" Designating the target platform.
" @Hoge{Win|Ubuntu}  : This Hint for Win and Ubuntu.
" @Hoge!{Mac}        : This Hint for other than Mac.
"----------------------------------------
" {- Booked something -}
" mark     Z => comand[BufMoveNewTab]
" register z => inoremap[<C-k>Y]
"----------------------------------------


"---------------------"
"      Parameter      "
"---------------------"
"{{{

let g:vimrc = get(g:, 'vimrc', {})
let g:vimrc['loaded'] = get(g:vimrc, 'loaded', 0)

let s:is_nvim    = has('nvim')
let s:is_windows = has('win32')
let s:is_cygwin  = has('win32unix')
let s:is_kaoriya = has('kaoriya')
let s:is_doswin  = s:is_windows && !s:is_cygwin && !has('gui')
let s:is_unix    = has('unix')
let s:is_mac     = has('mac')
let s:is_mac_osx = has('macunix')

let s:has_cygwin = isdirectory('/cygwin')
let s:has_mingw  = 0  "@Incomplete('dummy')

let s:vim_home = expand('~/.vim')

let s:backupdir = expand('~/.backup/vim_backup')
let s:directory = s:backupdir . '/swp'
let s:undodir   = s:backupdir . '/undo'
let s:viewdir   = s:backupdir . '/view'

let s:username  = $USER
let s:groupname = !empty($GROUP) ? $GROUP : $USER

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

function! s:echo_error(msg)
	echohl Error
	echo a:msg
	echohl None
endfunction

"}}}


"-------------------------"
"       Initialize        "
"-------------------------"
" autocmd Groups {{{

augroup PluginPrefs
	autocmd!
augroup END

augroup FileEvent
	autocmd!
augroup END

augroup ExtensionType
	autocmd!
augroup END

augroup HighlightPref
	autocmd!
augroup END

augroup KeyMapping
	autocmd!
augroup END

augroup KeyEvent
	autocmd!
augroup END

"}}}
" For Support Kaoriya Vim {{{

if s:is_kaoriya
	" Set Environment
	let $HOME        = $VIM
	let s:vim_home   = expand('~/.vim')  " Reset with $HOME
	let &runtimepath = &runtimepath . ',' . s:vim_home
	if s:has_cygwin
		let $PATH = '/cygwin/bin;/cygwin/usr/bin;/cygwin/usr/sbin;' . $PATH
		let $PATH = $HOME . '/bin;' . $PATH
	endif


	" Build Base Directories
	if !isdirectory(s:vim_home)
		call mkdir(s:vim_home)
	endif


	" For Using No Default vimproc
	let s:switch_dir = $VIM . '/switches/enabled'
	let s:suppress   = s:switch_dir . '/disable-vimproc.vim'

	if s:is_windows && !s:has_mingw && filereadable(s:suppress)
		call delete(s:suppress)
	elseif s:is_windows && s:has_mingw && !filereadable(s:suppress)
		call writefile([], s:suppress)
	endif

	for s:disf in map(['/utf-8.vim', '/vimdoc-ja.vim'], 's:switch_dir . v:val')
		if !filereadable(s:disf)
			call writefile([], s:disf)
		endif
	endfor
	unlet s:switch_dir s:suppress s:disf


	" Unset Kaoriya Preference
	set noignorecase nosmartcase

	augroup FileEvent
		autocmd BufRead $MYVIMRC setl enc=utf8 fenc=utf8
	augroup END
endif

"}}}
" Check NeoBundle exists {{{
let s:bundledir    = s:vim_home . '/bundle'
let s:neobundledir = s:bundledir . '/neobundle.vim'

if !isdirectory(s:bundledir)
	call mkdir(s:bundledir)
endif

function! s:remove_empty_bundledir()  "{{{
	let l:dirs = split(s:system('ls ' . s:bundledir), '\n')

	for l:dir in l:dirs
		let l:plugin_dir = s:bundledir . '/' . l:dir
		let l:is_empty   = empty(s:system('ls ' . l:plugin_dir))

		if l:is_empty
			call s:system('rmdir ' . l:plugin_dir)
		endif
	endfor
endfunction  "}}}
function! s:fetch_neobundle() " {{{
	if executable('git')
		echo 'NeoBundle was not installed...'
		echo 'Installing NeoBundle.'

		execute '!git clone http://github.com/Shougo/neobundle.vim ' s:neobundledir
		return
	else
		call s:echo_error('Sorry, You do not have git command.')
		call s:echo_error('Cannot introduce NeoBundle.')

		throw 'neobundle.vim clone failed.'
	endif
endfunction " }}}

if has('vim_starting')
	try
		let &runtimepath = &runtimepath . ',' . s:vim_home . '/bundle/neobundle.vim'

		" Throws Error when nothing neobundle in runtime path
		call neobundle#begin()
	catch
		if isdirectory(s:neobundledir) && !exists(':NeoBundle')
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

unlet s:neobundledir
unlet s:bundledir
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
" Enable matchit.vim {{{

if !exists('loaded_matchit')
	" Load it
	runtime macros/matchit.vim

	" If I don't have matchit document, I get it
	let s:matchit_doc_from = expand('$VIMRUNTIME/macros/matchit.txt')
	let s:matchit_doc_to   = expand('~/.vim/doc/matchit.txt')

	if !filereadable(s:matchit_doc_to)
		call writefile(readfile(s:matchit_doc_from), s:matchit_doc_to)
	endif

	unlet s:matchit_doc_to
	unlet s:matchit_doc_from
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
NeoBundleLazy    'mattn/yamada-vim'
NeoBundleLazy    'jvoorhis/coq.vim'
NeoBundleLazy    'eagletmt/coqtop-vim'
NeoBundle        'duff/vim-scratch'
NeoBundle        'rhysd/vim-grammarous'
NeoBundleLazy    'thinca/vim-themis'
NeoBundle        'tomasr/molokai'
NeoBundle        'aiya000/arot13.vim'
NeoBundle        'aiya000/ahoge-put.vim'
NeoBundleLazy    'kannokanno/previm'
NeoBundle        'gist:aiya000/ec5f6b2375a639831953', {
\	'name'        : 'gitlogviewer.vim',
\	'script_type' : 'plugin'
\}
NeoBundle        'kamichidu/vim-vdbc'
NeoBundle        'mattn/vdbi-vim'
NeoBundle        'LeafCage/foldCC'
NeoBundleLazy    'katono/rogue.vim'
NeoBundle        'aiya000/asql.vim'
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
NeoBundle        'glidenote/memolist.vim'
NeoBundle        'vim-jp/vimdoc-ja'
NeoBundleLazy    'rbtnn/game_engine.vim'
NeoBundle        'h1mesuke/vim-alignta'
NeoBundle        'haya14busa/incsearch.vim'
NeoBundle        'thinca/vim-scouter'
NeoBundle        'deris/vim-shot-f'
NeoBundle        'oplatek/Conque-Shell'
NeoBundle        'vim-scripts/TaskList.vim'
NeoBundle        'tyru/vim-altercmd'
NeoBundle        'mbbill/undotree'
NeoBundle        'Shougo/neomru.vim'
NeoBundle        'vim-scripts/dbext.vim'
NeoBundle        'aiya000/adrone.vim'
NeoBundleFetch   'Shougo/fakecygpty'
NeoBundle        'nathanaelkane/vim-indent-guides'
NeoBundleLazy    'LeafCage/vimhelpgenerator'
NeoBundleLazy    'thinca/vim-threes'
NeoBundleLazy    'vim-ruby/vim-ruby'
NeoBundleLazy    'Keithbsmiley/rspec.vim'
NeoBundle        'tsukkee/unite-help'
NeoBundle        'altercation/vim-colors-solarized'


call neobundle#end()

try
	helptags ~/.vim/bundle/.neobundle/doc
catch /E154/
	" Suppressed helptags duplication error
endtry

"}}}
"*** Plugin Depends and Auto Config ***" {{{

call neobundle#config('vimproc.vim', {
\	'build' : {
\		'unix'    : 'make -f make_unix.mak',
\		'mac'     : 'make -f make_mac.mak',
\		'cygwin'  : 'make -f make_cygwin.mak',
\		'windows' : 'make -f make_mingw32.mak'
\	}
\})
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
"call neobundle#config('arot13.vim', {
"\	'autoload' : {'commands' : 'MyPluginOn'}
"\})
"call neobundle#config('ahoge-put.vim', {
"\	'autoload' : {'commands' : 'MyPluginOn'}
"\})
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
"call neobundle#config('asql.vim', {
"\	'autoload' : {'commands' : 'MyPluginOn'}
"\})
call neobundle#config('ref-dicts-en', {
\	'depends' : 'thinca/vim-ref'
\})
call neobundle#config('adrone.vim', {
\	'autoload' : {'commands' : 'MyPluginOn'}
\})
call neobundle#config('fakecygpty', {
\	'build' : {
\		'windows' : expand('gcc fakecygpty.c -o ~/bin/fakecygpty.exe')
\	}
\})
call neobundle#config('vimhelpgenerator', {
\	'autoload' : {'commands' : [
\		'VimHelpGenerator',
\		'HelpIntoMarkdown'
\	]}
\})
call neobundle#config('vim-threes', {
\	'autoload' : {'commands' : [
\		'ThreesShowRecord',
\		'ThreesStart'
\	]}
\})
call neobundle#config('vim-ruby', {
\	'autoload' : {'filetype' : 'ruby'}
\})
call neobundle#config('rspec.vim', {
\	'autoload' : {'filetype' : 'ruby'}
\})

" }}}


"------------------------"
"*** Plugin_Configure ***"
"------------------------"
"--- netrw ---" {{{

let g:netrw_preview = 1

augroup PluginPrefs
	autocmd FileType netrw setl nolist
augroup END

" }}}
"--- matchit.vim ---" {{{

"augroup FileEvent
"	" uooooooooooooo... oh, my triple operator !!!!!!!!!!!
"	" Why if set you, happend an error when doing match it...
"	autocmd FileType * let b:match_words = &matchpairs . ',?::'
"augroup END

" }}}
"--- unite.vim ---"{{{

"@Code(':Unite javasrc')
let g:unite_source_alias_aliases = {
\	'javasrc' : {
\		'source' : 'file_rec',
\		'args'   : '~/Documents/workspace/Java/src'
\	}
\}

"}}}
"--- vim-quickrun ---" {{{
"
let g:quickrun_config = {
\	'_' : {
\		'split'  : '',
\		'runner' : 'vimproc',
\		'runner/vimproc/updatetime' : 10,
\		'hook/time/enable' : 1
\	},
\	'cpp' : {
\		'command' : 'g++',
\		'cmdopt'  : '-std=c++14'
\	},
\	'java' : {
\		'cmdopt' : '-source 1.8',
\		'runner' : 'process_manager'
\	}
\}

if s:is_windows
	let g:quickrun_config['cs'] = {
	\	'command'  : 'csc.exe',
	\	'exec'     : ['%c %o %s:p', '%s:p:r.exe', 'del %s:p:r.exe'],
	\	'hook/output_encode/encoding' : 'cp932:utf8'
	\}
elseif s:is_unix
	let g:quickrun_config['cs'] = {
	\	'command'  : 'gmcs',
	\	'exec'     : ['%c %o %s:p > /dev/null', 'mono %s:p:r.exe', 'rm %s:p:r.exe'],
	\	'tempfile' : '{tempname()}.cs'
	\}
endif

if s:is_cygwin
	let g:quickrun_config['java'] = {
	\	'command' : 'javac',
	\	'exec'    : ['%c %o `echo %s | sed s:\:/:g | cygpath -w -f -`', '%c %s:t:r %a'],
	\	'hook/output_encode/encoding' : 'Shift_JIS'
	\}

	let s:javav = s:system('java -version')

	let g:quickrun_config.java['cmdopt'] =
	\	s:javav =~# '1\.8' ? '-source 1.8 -encoding UTF-8' :
	\	s:javav =~# '1\.7' ? '-source 1.7 -encoding UTF-8'
	\	                   : '-encoding UTF-8'

	unlet s:javav
endif

augroup PluginPrefs
	autocmd FileType quickrun setl wrap
augroup END

" }}}
"--- vimproc.vim ---"{{{

if s:is_windows && !s:has_mingw
	"@Incompleted('I couldn't use a like NeoBundleDisable on this situation')
	"NeoBundleDisable 'Shougo/vimproc.vim'
	set runtimepath-=~/.vim/bundle/vimproc.vim/
endif

" }}}
"--- TweetVim ---"{{{

let g:tweetvim_async_post = 1

augroup PluginPrefs
	autocmd FileType tweetvim     setl wrap
	autocmd FileType tweetvim_say setl ts=2 sw=2 et
augroup END

"}}}
"--- vimshell.vim ---"{{{

" Add to VimShell Commands Directory of My Home
let &runtimepath = &runtimepath.','.s:vim_home.'/autoload/vimshell/commands'

let g:vimshell_no_save_history_commands = {
\	'history': 1,
\	'ls'     : 1,
\	'clear'  : 1
\}
let g:vimshell_enable_transient_user_prompt = 1
let g:vimshell_force_overwrite_statusline = 1
let g:vimshell_max_command_history = 10000
let g:vimshell_scrollback_limit = 10000

augroup PluginPrefs
	autocmd FileType vimshell setl fdm=marker nolist wrap
	" Depends the command that is defined by this vimrc
	autocmd FileType vimshell call vimshell#altercmd#define('tdirset', ':TDirSet')
	autocmd FileType vimshell call vimshell#altercmd#define('tdircd',  ':TDirCd')
	autocmd FileType vimshell call vimshell#altercmd#define('tdirpwd', ':TDirPwd')
	autocmd FileType vimshell call vimshell#altercmd#define('tab',     ':BufOpenNewTab')
	autocmd FileType vimshell call vimshell#altercmd#define('tabopen', ':BufMoveNewTab')
augroup END


" for My Command

" connect to bash's hereis
let g:vimshell_hereis_file = expand('~/.bashrc_places')

"}}}
"--- J6uil ---"{{{

augroup PluginPrefs
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
"--- foldCC ---"{{{

let g:foldCCtext_maxchars = 120

"}}}
"--- rogue.vim ---"{{{

let g:rogue#directory = expand('~/.rogue_vim')

if !isdirectory(g:rogue#directory)
	call mkdir(g:rogue#directory)
endif

"}}}
"--- vim-submode ---"{{{

let g:submode_timeout = 0

" Window Resizer
call submode#enter_with('window_resize', 'n', '', '<C-s>w')
call submode#map('window_resize', 'n', '', 'j', '<C-w>+')
call submode#map('window_resize', 'n', '', 'k', '<C-w>-')
call submode#map('window_resize', 'n', '', 'h', '<C-w><')
call submode#map('window_resize', 'n', '', 'l', '<C-w>>')
call submode#map('window_resize', 'n', '', '=', '<C-w>=')
call submode#map('window_resize', 'n', '', '_', '<C-w>_')

" Fold Mover
call submode#enter_with('fold_move', 'n', '', '<C-s>z')
call submode#map('fold_move', 'n', 'e', 'j', "foldlevel('.') > 0 ? 'zczjzozz'   : 'zjzozz'")
call submode#map('fold_move', 'n', 'e', 'k', "foldlevel('.') > 0 ? 'zczkzo[zzz' : 'zkzo[zzz'")
call submode#map('fold_move', 'n', '',  'h', '[z')
call submode#map('fold_move', 'n', '',  'l', ']z')

" Buffer Changer
call submode#enter_with('buffer_change', 'n', '', '<C-s>b')
call submode#map('buffer_change', 'n', 's', 'n', ':bnext<CR>')
call submode#map('buffer_change', 'n', 's', 'p', ':bprevious<CR>')

" Tab Mover
" s:loopable_tab_move_prev() "{{{

function! s:loopable_tab_move_prev()
	if tabpagenr() is 1
		execute ':tabmove' tabpagenr('$')
	else
		execute ':tabmove -1'
	endif
endfunction

let g:vimrc['LoopableTabMovePrev'] = function('s:loopable_tab_move_prev')

"}}}
" s:loopable_tab_move_next() "{{{

function! s:loopable_tab_move_next()
	if tabpagenr() is tabpagenr('$')
		execute ':tabmove 0'
	else
		execute ':tabmove +1'
	endif
endfunction

let g:vimrc['LoopableTabMoveNext'] = function('s:loopable_tab_move_next')

"}}}
call submode#enter_with('tab_move', 'n', '', '<C-s>t')
call submode#map('tab_move', 'n', 's', 'n', ':call g:vimrc.LoopableTabMoveNext()<CR>')
call submode#map('tab_move', 'n', 's', 'p', ':call g:vimrc.LoopableTabMovePrev()<CR>')

" WinTab Mover
call submode#enter_with('wintab_move', 'n', '', '<C-s>N', ':BufTabMoveNext<CR>')
call submode#enter_with('wintab_move', 'n', '', '<C-s>P', ':BufTabMovePrev<CR>')
call submode#map('wintab_move', 'n', '', 'N', ':BufTabMoveNext<CR>')
call submode#map('wintab_move', 'n', '', 'n', ':BufTabMoveNext<CR>')
call submode#map('wintab_move', 'n', '', 'P', ':BufTabMovePrev<CR>')
call submode#map('wintab_move', 'n', '', 'p', ':BufTabMovePrev<CR>')

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

function! s:weblio_filter(output)
	let l:lines  = split(a:output, "\n")
	"@Incompleted('')
	let l:lines1 = map(l:lines, 'substitute(v:val, "\v(発音記号|音声を聞く|ダウンロード再生)\n", "", "g")')
	return join(l:lines1[60 : ], "\n")
endfunction
let g:ref_source_webdict_sites['weblio'].filter = function('s:weblio_filter')

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
"--- vimdoc-ja ---"{{{

" vimdoc-ja is secondary
set helplang=en,ja

"}}}
"--- Conque-Shell ---"{{{

let g:ConqueTerm_CloseOnEnd     = 1
let g:ConqueTerm_SessionSupport = 1
let g:ConqueTerm_ReadUnfocused  = 1
let g:ConqueTerm_Color          = 1
let g:ConqueTerm_InsertOnEnter  = 0
let g:ConqueTerm_StartMessages  = 1

"}}}
"--- dbext.vim ---"{{{

let g:dbext_default_history_file = expand('~/.dbext_sql_history')

"}}}
"--- adrone.vim ---"{{{

augroup PluginPrefs
	autocmd FileType adrone_home nnoremap <silent><buffer> Q     :<C-u>bdelete<CR>
	autocmd FileType adrone_say  nnoremap <silent><buffer> <C-j> <CR>
	autocmd FileType adrone_say  setl tabstop=2 shiftwidth=2 expandtab
augroup END

"}}}
"--- vim-indent-guides ---"{{{

let g:indent_guides_default_mapping = 0
let g:indent_guides_guide_size = 1

" If matched file extension pattern, indent-guides is enabled (patter match)
augroup FileEvent
	autocmd WinEnter,BufWinEnter *                  IndentGuidesDisable
	autocmd WinEnter,BufWinEnter *.html,*.xml,*.erb IndentGuidesEnable
augroup END

"}}}
"--- For Debug ---"{{{

" Local my plugins
let s:makes = ['arot13.vim',
\              'ahoge-put.vim',
\              'asql.vim',
\              'adrone.vim']
"set runtimepath+=~/.vim/makes/vital.vim

let s:makes_dir = '~/.vim/makes/'


" If valid local plugin, disable bundled same plugin
for s:plug in s:makes
	let s:plug_dir = s:makes_dir . s:plug

	if isdirectory(expand(s:plug_dir))
		let &runtimepath .= ',' . s:plug_dir
		NeoBundleDisable s:plug
	end
endfor


unlet s:plug_dir s:plug s:makes_dir s:makes

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

" Set Basic Preferences
set number nowrap hlsearch list scrolloff=8

" Status Bar always displayed
set laststatus=2

"@See('http://sourceforge.jp/magazine/07/11/06/0151231')
" Status Bar format
set statusline=%F%m\%=[FileType=%y][Format=%{&ff}]

" ☆ Fix View 2byte Code (Not support gnome-terminal)
set ambiwidth=double

" Powered Up Syntax Highlight
" {{{

augroup HighlightPref
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
augroup END


augroup HighlightPref
	autocmd ColorScheme       * highlight RcEmSpace cterm=standout ctermfg=LightBlue
	autocmd VimEnter,WinEnter * call matchadd('RcEmSpace', '　')
augroup END


augroup HighlightPref
	autocmd InsertEnter * highlight StatusLine ctermfg=Black ctermbg=Cyan
	autocmd InsertLeave * highlight StatusLine ctermfg=Cyan  ctermbg=Black
augroup END

" }}}

" Set for Color Scheme
if !g:vimrc['loaded']
	set background=dark
	colorscheme desert
endif

" Indent Wrapped Text
set breakindent linebreak

" View cursor column on <C-g>
set noruler

" Hard Conceal
"set conceallevel=2

"@See('http://d.hatena.ne.jp/thinca/20111204/1322932585')
" Sugoi view tabline
function! s:tabpage_label(n) "{{{
	let l:title = gettabvar(a:n, 'title')
	if !empty(l:title)
		return l:title
	endif

	let l:bufnrs = tabpagebuflist(a:n)
	let l:hi = a:n is tabpagenr() ? '%#TabLineSel#' : '%#TabLine#'

	let l:no = len(l:bufnrs)
	if l:no is 1
		let l:no = ''
	endif

	let l:mod = len(filter(copy(l:bufnrs), "getbufvar(v:val, '&modified')")) ? '+' : ''
	let l:sp = empty(l:no . l:mod) ? '' : ' '

	let l:curbufnr = bufnrs[tabpagewinnr(a:n) - 1]
	let l:fname = pathshorten(bufname(l:curbufnr))
	if empty(l:fname)
		let l:fname .= '[ NoName ]'
	endif

	let l:label = l:no . l:mod . l:sp . l:fname
	return '%' . a:n . 'T' . l:hi . l:label . '%T%#TabLineFill#'
endfunction "}}}
function! WithDelimitterTabLine() "{{{
	let l:titles     = map(range(1, tabpagenr('$')), 's:tabpage_label(v:val)')
	let l:delimitter = ' | '
	let l:tabpages   = l:delimitter . join(l:titles, l:delimitter) . l:delimitter . '%#TabLineFill#%T'
	return l:tabpages
endfunction "}}}
set tabline=%!WithDelimitterTabLine() showtabline=2

" Always view the changed line num in Ex-command
"set report=0

" Turn off highlight
nohlsearch

"}}}


"-------------------------"
"     Action_Setting      "
"-------------------------"
"{{{

" Backspace can delete it
set backspace=indent,eol,start

" No auto Carriage Return and Set tab style
set textwidth=0 tabstop=4 shiftwidth=4

" C type auto indent on
set autoindent cindent

" Incremental Searching
"set incsearch

" Fold Text with foldmarker and fold sets
set foldmethod=marker
set foldtext=FoldCCtext()
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

" Ignore case on NormalMode searching and InsertMode completion
set ignorecase noinfercase

" No timeout key maps
set notimeout

" Do not set file name order priority on c-mode completion
set suffixes=

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

" Set Vimball Install place
let g:vimball_home = s:vim_home . '/vimball'

" Display Command Complement
set wildmenu

" Path Delimiter is Slash
set shellslash

" Add Match Pairs
set matchpairs+=<:>

" Load Target for ctags
set tags=./tags,~/tags

" Explore wake up default dir
set browsedir=buffer

" Set spell lang
set spelllang=en_US

" Set reference path, using by :find, gf and more
set path=.,,./**

" Generate HelpTags My Help
if isdirectory(expand('~/.vim/doc'))
	helptags ~/.vim/doc
endif

"}}}


"-------------------------"
"      Event_Method       "
"-------------------------"
"{{{

" Save Cursor Position when file closed
augroup FileEvent
	function! s:visit_past_position() "{{{
		let l:past_posit = line("'\"")

		if l:past_posit > 0 && l:past_posit <= line('$')
			execute 'normal! g`"'
		endif
	endfunction "}}}
	autocmd BufReadPost * call <SID>visit_past_position()
augroup END


" Powered Up Auto File Backup when written
set nobackup
function! s:update_backup_by_date() "{{{
	let l:dailydir = s:backupdir . '/' . strftime('%Y-%m-%d')

	if !isdirectory(l:dailydir)
		call mkdir(l:dailydir, 'p', 0755)
		if s:is_unix
			call s:system(printf('chown -R %s:%s %s', s:username, s:groupname, l:dailydir))
		endif
	endif

	let l:filepath = split(expand('%'), '/')
	let l:filename = l:filepath[len(l:filepath)-1] . strftime(s:is_windows ? '_at_%H-%M' : '_at_%H:%M')
	let l:location = l:dailydir . '/' . l:filename

	call writefile(getline(1, '$'), l:location)
endfunction "}}}

augroup FileEvent
	autocmd BufWritePre ?\+ silent call s:update_backup_by_date()

	autocmd VimEnter,WinEnter,BufWinEnter,BufRead,EncodingChanged *
		\	if &encoding == 'utf-8'
		\|		let &listchars = 'tab:»_,trail:_,extends:»,precedes:«,nbsp:%,eol:↲'
		\|	else
		\|		let &listchars = 'tab:>_,trail:_,extends:>,precedes:<,nbsp:%'
		\|	endif
augroup END

augroup KeyEvent
	"autocmd UserGettingBored * echo 'HA HA HA'
augroup END

"}}}


"-------------------------"
"   Functional_Command    "
"-------------------------"
" Utility Function {{{

" Revese Lines
function! s:reverse_line() range " {{{
	if executable('tac')
		execute "'<,'>!tac"
	else
		if a:firstline is a:lastline
			return
		endif

		let l:lines = []
		let l:posit = getpos('.')

		for l:line in range(a:firstline, a:lastline)
			call add(l:lines, substitute(getline(l:line)."\n", "\t", '', 'g'))
		endfor

		for l:i in range(a:firstline, a:lastline)
			execute 'normal! "_dd'
			execute 'normal! i' lines[a:lastline - l:i]
		endfor

		call setpos('.', l:posit)
	endif
endfunction " }}}
command! -range=%
\	ReverseLine :<line1>, <line2> call s:reverse_line()


" Catenate and echo files
function! s:cat_file(...) "{{{
	let l:catenate = ''

	if executable('cat')
		for l:filePath in a:000
			let l:catenate .= s:system('cat ' . l:filePath)
		endfor
	else
		for l:filePath in a:000
			let l:catenate .= join(readfile(l:filePath), "\n")
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

" Save a Temporary Directory
autocmd FileEvent FileType * let b:tdir_dir = get(b:, 'tdir_dir', 'Not set tdir')
command! TDirPwd           echo b:tdir_dir
function! s:set_temporary_dir(path) "{{{
	if isdirectory(a:path)
		let b:tdir_dir = a:path ==# '.' ? expand('%:p:h')
		\                               : a:path
		echo b:tdir_dir
	else
		call s:echo_error('No such temporary root dir')
	endif
endfunction "}}}
command! -nargs=1  TDirSet call s:set_temporary_dir(<q-args>)
command! TDirSetCurrentDir call s:set_temporary_dir('.')
function! s:cd_temporary_dir() "{{{
	if b:tdir_dir ==# 'Not set tdir'
		call s:echo_error('Not set temporary root dir')
	else
		execute 'cd' b:tdir_dir
		echo b:tdir_dir
	endif
endfunction "}}}
command! TDirCd            call s:cd_temporary_dir()


" Pop up Scratch Buffers
" command! ScratchUp "{{{

" The cushion of the environments
if s:is_windows
	" Is different operated :sp ubuntu and kaoriya ?
	command! ScratchUp execute ':Scratch' | resize 5
else
	function! s:scratch_up_by_condition()
		if !&modified
			execute ':sp|Scratch' | resize 5
		else
			execute ':Scratch' | resize 5
		endif
	endfunction

	command! ScratchUp call s:scratch_up_by_condition()
endif

"}}}
command! EmptyBufUp execute ':new' | resize 5


" Yank all to plus register
command! CPAllPlus execute 'normal! ggVG"+y<C-o><C-o>'


" Current buffer move to next_tab
command! BufTabMovePrev execute 'normal! mZ<C-w>cgT<C-w>v`Z'
command! BufTabMoveNext execute 'normal! mZ' . (winnr('$') <= 1 ? '<C-w>c' : '<C-w>cgt') . '<C-w>v`Z'


" Current buffer open or move to new_tab
command! BufOpenNewTab execute 'normal! mZ:tabnew<CR>`Zzz'
command! BufMoveNewTab execute 'normal! mZ<C-w>c:tabnew<CR>`Zzz'

" }}}
" Development Support {{{

"@Incompleted('does not removed another temporary class')
" If you cannot use QuickRun or want to use stdio, you can use this.
function! s:java_run_func() "{{{
	let l:javaname = expand('%:t:r')
	let l:javav = s:system('java -version')

	if l:javav =~# '1.8'
		let l:command = ['javac -source 1.8 -encoding utf8', 'java']
	elseif l:javav =~# '1.7'
		let l:command = ['javac -source 1.7 -encoding utf8', 'java']
	else
		let l:command = ['javac -encoding utf8', 'java']
	endif

	if s:is_cygwin
		if executable('cocot')
			let l:command[0] = 'cocot ' . l:command[0]
			let l:command[1] = 'cocot ' . l:command[1]
		else
			call s:echo_error('You must be get [cocot] command.')
			return
		endif
	endif

	execute ':!' .
	\	printf('%s %s.java',   l:command[0], l:javaname) . ';' .
	\	printf('%s %s',        l:command[1], l:javaname) . ';'

	call delete(l:javaname . '.class')
endfunction "}}}
command! RunJava call s:java_run_func()

" Same as RunJava for Ruby
cnoreabbr RunRuby !ruby %

"function! s:put_python_import_for_jp() "{{{
"	let l:paste = &paste
"	set paste
"	execute 'normal! ggO' '#!/usr/bin/env python'
"	execute 'normal! o'   '# -*- coding: utf-8 -*-'
"	execute 'normal! o'   'import sys'
"	execute 'normal! o'   'import codecs'
"	execute 'normal! o'   "sys.stdout = codecs.getwriter('utf_8')(sys.stdout)"
"	let &paste = l:paste
"endfunc "}}}
"command! ImportPythonJp call s:put_python_import_for_jp()


" command! PutShortSeparator {{{

augroup FileEvent
	autocmd FileType,WinEnter,BufWinEnter * let s:short_sparator =
	\	&ft ==# 'vim'                ? '" --- --- --- "'
	\:	&ft =~# '\v(java|cs|cpp|c)'  ? '/* -=-=-=-=-=-=-=-=- */'
	\:	&ft ==# 'haskell'            ? '-- - - - - - --'
	\:	&ft ==# 'coq'                ? '(* - - - - - *)'
	\:	&ft ==# 'mysql'              ? '-- - - - - - --'
	\:	&ft =~# '\v(markdown|eruby)' ? '<!-- - - - - - -->'
	\:	&ft =~# '\v(ruby|sh)'        ? '#- - - - - - -#'
	\:	&ft =~# '\v(text|none)'      ? '- - - - - - - - - -'
	\:	'short_separator_undefined'
augroup END

command! PutShortSeparator
	\	execute 'normal! o' s:short_sparator
	\|	execute 'normal! =='

" }}}
" command! PutLongSeparator {{{

augroup FileEvent
	autocmd FileType,WinEnter,BufWinEnter * let s:long_separator =
	\	&ft ==# 'vim'                ? '" --- --- --- --- --- --- --- --- --- "'
	\:	&ft =~# '\v(java|cs|cpp|c)'  ? '/* ---===---===---===---===---===---===--- */'
	\:	&ft ==# 'haskell'            ? '-- - - - - - - - - - - - - - - - --'
	\:	&ft ==# 'coq'                ? '(* - - - - - - - - - - - - - - - *)'
	\:	&ft ==# 'mysql'              ? '-- - - - - - - - - - - - - - - - --'
	\:	&ft =~# '\v(markdown|eruby)' ? '<!-- - - - - - - - - - - - - - - - -->'
	\:	&ft =~# '\v(ruby|sh)'        ? '#- - - - - - - - - - - - - - - - -#'
	\:	&ft =~# '\v(text|none)'      ? '- - - - - - - - - - - - - - - - - - - -'
	\:	'long_separator_undefined'
augroup END

command! PutLongSeparator
	\	execute 'normal! o' s:long_separator
	\|	execute 'normal! =='

" }}}
command! PutDate execute 'normal! a' strftime('%c')


function! s:put_html_base() "{{{
	let l:paste = &paste
	set paste
	execute 'normal! O' '<html lang="ja">'
	execute 'normal! o' '<head>'
	execute 'normal! o' '<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">'
	execute 'normal! o' '<title></title>'
	execute 'normal! o' '</head>'
	execute 'normal! o' '<body>'
	execute 'normal! o'
	execute 'normal! o' '</body>'
	execute 'normal! o' '</html>'
	let &paste = l:paste
endfunction "}}}
command! PutHtmlBase call s:put_html_base()


" Echo script local values in this file
command! ShowRcDict for s:v in items(s:) | echo s:v | endfor

" }}}


"-------------------------"
"      Command_Alias      "
"-------------------------"
" Override {{{

call altercmd#load()

" buffer open commands with filetype 'none'
command! -bang NewOverride new<bang> | setf none
AlterCommand new NewOverride

command! -bang VnewOverride vnew<bang> | setf none
AlterCommand vnew VnewOverride

command! -bang EnewOverride enew<bang> | setf none
AlterCommand enew EnewOverride

command! -bang TabnewOverride tabnew<bang> | setf none
AlterCommand tabnew TabnewOverride

" }}}
" Utils {{{

" Vim Utils {{{
command! VimConfig         e $MYVIMRC
command! VimConfigTab      tabnew | e $MYVIMRC
command! Reload            so $MYVIMRC
	\|	if has('gui_running')
	\|		so $MYGVIMRC
	\|	endif
command! ForceSave         w !sudo tee > /dev/null %

command! CdBufDir          cd %:p:h
command! Resetf            let &ft = &ft  " for actuate autocmd Events [FileType *]

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
	if !exists('g:vimrc.private["twitter"]["priv_ac"]')
		call s:echo_error('Not set env variable => g:vimrc.private["twitter"]["priv_ac"]')
		return
	endif

	execute ':TweetVimSwitchAccount' g:vimrc.private['twitter']['priv_ac']
	let g:vimrc.private['twitter']['curr_ac'] = g:vimrc.private['twitter']['priv_ac']

	execute ':TweetVimHomeTimeline'
endfunction "}}}
command! TwitterPrivate     call TwitterPrivateFunc()
command! TwitterPrivateTab  tabnew | TwitterPrivate
function! TweetPrivateFunc() "{{{
	if !exists('g:vimrc.private["twitter"]["priv_ac"]')
		call s:echo_error('Not set env variable => g:vimrc.private["twitter"]["priv_ac"]')
		return
	endif

	execute ':TweetVimSwitchAccount' g:vimrc.private['twitter']['priv_ac']
	execute ':TweetVimSay'

	"@Incompleted('wait sync here')
	"execute ':TweetVimSwitchAccount' g:vimrc.private['twitter']['curr_ac']
endfunction "}}}
command! TweetPrivate       call TweetPrivateFunc()


"-- Public Account --"
function! TwitterPublicFunc() "{{{
	if !exists("g:vimrc.private['twitter']['publ_ac']")
		call s:echo_error("Not set env variable => g:vimrc.private['twitter']['publ_ac']")
		return
	endif

	execute ':TweetVimSwitchAccount ' g:vimrc.private['twitter']['publ_ac']
	let g:vimrc.private['twitter']['curr_ac'] = g:vimrc.private['twitter']['publ_ac']

	execute ':TweetVimHomeTimeline'
endfunction "}}}
command! TwitterPublic      call TwitterPublicFunc()
command! TwitterPublicTab   tabnew | TwitterPublic
function! TweetPublicFunc() "{{{
	if !exists('g:vimrc.private["twitter"]["publ_ac"]')
		call s:echo_error('Not set env variable => g:vimrc.private["twitter"]["publ_ac"]')
		return
	endif

	execute ':TweetVimSwitchAccount ' g:vimrc.private['twitter']['publ_ac']
	execute ':TweetVimSay'

	"@Incompleted('wait here')
	"execute ':TweetVimSwitchAccount ' g:vimrc.private['twitter']['curr_ac']
endfunction "}}}
command! TweetPublic        call TweetPublicFunc()


command!  Bitly TweetVimBitly
cnoreabbr tvs   TweetVimSwitchAccount

" }}}

" To Service Name
cnoreabbr Lingr J6uil

" Beautifull Life
command!  JazzUpdate    JazzradioUpdateChannels
command!  JazzList      Unite jazzradio
cnoreabbr JazzPlay      JazzradioPlay
command!  JazzStop      JazzradioStop

" Translates Languages
cnoreabbr Translate ExciteTranslate
cnoreabbr Weblio    Ref webdict weblio

" Grep and Open current buffer
command! -nargs=1 GrepNow vimgrep <args> % | cwindow

" }}}
" Developments {{{

" vimconsole.vim
cnoreabbr Log      VimConsoleLog
command!  LogClear VimConsoleClear

" GHCi
cnoreabbr RunGhc   !runghc %
cnoreabbr Ghci     VimShellInteractive ghci
cnoreabbr Sghci    VimShellInteractive --split='sp' ghci
cnoreabbr Vghci    VimShellInteractive --split='vsp' ghci
cnoreabbr GhciTab  VimShellInteractive --split='tabnew' ghci
cnoreabbr Hoogle   Ref hoogle

" js
cnoreabbr Js       VimShellInteractive js
cnoreabbr Sjs      VimShellInteractive --split='sp' js
cnoreabbr Vjs      VimShellInteractive --split='vsp' js
cnoreabbr JsTab    VimShellInteractive --split='tabnew' js

" irb
cnoreabbr Irb      VimShellInteractive irb
cnoreabbr Sirb     VimShellInteractive --split='sp' irb
cnoreabbr Virb     VimShellInteractive --split='vsp' irb
cnoreabbr IrbTab   VimShellInteractive --split='tabnew' irb

" }}}


"-------------------------"
"      Key_Mappings       "
"-------------------------"
" Disables {{{

augroup KeyMapping
	autocmd FileType * nnoremap <C-c>   <NOP>  " for terminal intterupt
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

	autocmd FileType * cnoremap [Left] <Left>
augroup END

" }}}
" Bashnize Command Mode {{{

augroup KeyMapping
	autocmd FileType * nmap     <C-j> <CR>
	autocmd FileType * imap     <C-j> <CR>

	autocmd FileType * cnoremap <C-b> <Left>
	autocmd FileType * cnoremap <C-f> <Right>
	autocmd FileType * cnoremap <C-a> <Home>
	autocmd FileType * cnoremap <C-h> <BS>
	autocmd FileType * cnoremap <C-d> <Del>
	autocmd FileType * cnoremap <C-e> <End>
	autocmd FileType * cnoremap <C-k><C-k> <C-\>e getcmdpos() < 2 ?'':getcmdline()[:getcmdpos()-2]<CR>

	" for incsearch.vim
	autocmd FileType * IncSearchNoreMap <C-j> <CR>
	autocmd FileType * IncSearchNoreMap <C-b> <Left>
	autocmd FileType * IncSearchNoreMap <C-f> <Right>
	autocmd FileType * IncSearchNoreMap <C-a> <Home>
	autocmd FileType * IncSearchNoreMap <C-h> <BS>
	autocmd FileType * IncSearchNoreMap <C-d> <Del>
	autocmd FileType * IncSearchNoreMap <C-e> <End>
augroup END

" }}}
" Customize Keys {{{

" Prepare temporary functions {{{

" Compress continuous space
function! s:compress_spaces() "{{{
	let l:recent_pattern = @/

	try
		silent execute ':substitute/\s\s\+/ /g'
		silent execute 'normal! =='
	catch
		" GanMusi
	finally
		let @/ = l:recent_pattern
		unlet l:recent_pattern
	endtry

	execute ':noh'
endfunction "}}}


" Easy toggle virtualedit
function! s:toggle_virtual_edit() "{{{
	if empty(&virtualedit)
		set virtualedit=all
	else
		set virtualedit=
	endif

	set virtualedit?
endfunction "}}}


" Easy toggle diffthis and diffoff
function! s:toggle_diff_mode() "{{{
	if &diff
		execute ':diffoff'
	else
		execute ':diffthis'
	endif

	set diff?
endfunction "}}}


" Move cursor to topmost of this indent
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


" Move cursor to bottommost of this indent
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


" Toggle camel case and sneak case
"@Incompleted('do not implemented a part of this')
function! s:toggle_case() "{{{
	let l:pos  = getpos('.')

	let l:word     = expand('<cword>')
	let l:is_snake = l:word =~? '_' ? 1 : 0

	execute 'normal! b'
	if expand('<cword>') !=# l:word
		execute 'normal! w'
	endif

	if l:is_snake
		throw 'not implemented'
	else
		execute 'normal! ce' . substitute(l:word, '[A-Z]', '_\l\0', 'g')
	endif

	call setpos('.', l:pos)
endfunction "}}}


" If visualmode then Open all fold line
" s:visual_fold_all()"{{{
let s:visual_fold_toggle = get(s:, 'visual_fold_toggle', 0)

function! s:visual_fold_all()
	if mode() =~# "^[vV\<C-v>]"
		if !s:visual_fold_toggle && &foldenable
			set nofoldenable
			execute 'normal! zz'
			let s:visual_fold_toggle = 1
		endif
	else
		if s:visual_fold_toggle
			set foldenable
			execute 'normal! zz'
			let s:visual_fold_toggle = 0
		endif
	endif
endfunction
"}}}


" If you has nofile buffer, close it.
function! s:nofile_close() "{{{
	for l:w in range(1, winnr('$'))
		let l:buftype = getwinvar(l:w, '&buftype')

		if l:buftype ==# 'nofile'
			execute ':' . l:w . 'wincmd w'
			execute ':quit'

			break
		endif
	endfor
endfunction "}}}

" }}}
" Overwrite mapping {{{

augroup KeyMapping
	" † God Of The Vim
	autocmd FileType * nnoremap Q gQ

	autocmd FileType * nnoremap <C-n> gt
	autocmd FileType * nnoremap <C-p> gT
	autocmd FileType * nnoremap zl    8zl
	autocmd FileType * nnoremap zh    8zh
	autocmd FileType * inoremap <C-l> <Esc>
	autocmd FileType * vnoremap <C-l> <Esc>
	autocmd FileType * cnoremap <C-l> <Esc>
augroup END

" }}}
" Append " {{{

augroup KeyMapping
	autocmd FileType * nnoremap <silent> m:                :<C-u>marks<CR>
	autocmd FileType * nnoremap <silent> q:                :<C-u>register<CR>
	autocmd FileType * nnoremap <silent> z:                :<C-u>tabs<CR>
	autocmd FileType * nnoremap <silent> g:                :<C-u>buffers<CR>
	autocmd FileType * nnoremap <silent> gk                :<C-u>call <SID>cursor_up_to_lid()<CR>
	autocmd FileType * nnoremap <silent> gj                :<C-u>call <SID>cursor_down_to_ground()<CR>
	"autocmd FileType * vnoremap <silent> gk                :<C-u>call <SID>cursor_up_to_lid()<CR>
	"autocmd FileType * vnoremap <silent> gj                :<C-u>call <SID>cursor_down_to_ground()<CR>
	autocmd FileType * nnoremap <silent> <C-m>             o<Esc>
	autocmd FileType * nnoremap <silent> <Space><Space>    :<C-u>call <SID>compress_spaces()<CR>
	autocmd FileType * nnoremap <silent> <leader>b         :<C-u>ScratchUp<CR>
	autocmd FileType * nnoremap <silent> <leader>B         :<C-u>EmptyBufUp<CR>
	autocmd FileType * nnoremap <silent> <leader>pl        :<C-u>PutLongSeparator<CR>
	autocmd FileType * nnoremap <silent> <leader>ps        :<C-u>PutShortSeparator<CR>
	autocmd FileType * nnoremap <silent> <leader>pd        :<C-u>PutDate<CR>
	autocmd FileType * nnoremap <silent> <leader><leader>r :<C-u>call <SID>nofile_close()<CR>

	autocmd FileType * cnoremap <C-]> '<,'>
augroup END

" }}}
" for foldings {{{

augroup KeyMapping
	autocmd FileType * nnoremap <expr> h foldclosed('.') > -1 ? 'zo' : 'h'
	autocmd FileType * nnoremap <expr> l foldclosed('.') > -1 ? 'zo' : 'l'
	autocmd FileType * nnoremap zj     zjzo
	autocmd FileType * nnoremap zk     zkzo
	autocmd FileType * nnoremap z<     V$%zf
augroup END

" }}}
" for window and buffer {{{

augroup KeyMapping
	autocmd FileType * nnoremap <silent> <C-w>t     :<C-u>TabnewOverride<CR>
	autocmd FileType * nnoremap <silent> <C-w>T     :<C-u>tabclose<CR>
	autocmd FileType * nnoremap <silent> <C-w>c     :<C-u>bdelete<CR>
	autocmd FileType * nnoremap <silent> <C-w>C     :<C-u>bdelete!<CR>
	autocmd FileType * nnoremap <silent> <C-w><C-w> :<C-u>write<CR>
	autocmd FileType * nnoremap <silent> <C-w>W     :<C-u>wall<CR>
	autocmd FileType * nnoremap <silent> <C-w>bt    :<C-u>BufOpenNewTab<CR>
	autocmd FileType * nnoremap <silent> <C-w>bT    :<C-u>BufMoveNewTab<CR>
	autocmd FileType * nnoremap <silent> <C-w>N     :<C-u>EnewOverride!<CR>
	autocmd FileType * nnoremap <silent> <C-w>Q     :<C-u>quitall<CR>
augroup END

" }}}
" Another actions {{{

augroup KeyMapping
	autocmd FileType * nnoremap <silent><expr>   <C-k><C-s>     ':OverCommandLine<CR>%s/\<' . expand('<cword>') . '\>/'
	autocmd FileType * nnoremap <silent>         <C-k><C-r>     :<C-u>Reload<CR>
	autocmd FileType * nnoremap <silent>         <C-k><C-l>     :<C-u>nohlsearch<CR>
	autocmd FileType * nnoremap <silent>         <C-k>l         :<C-u>source %<CR>
	autocmd FileType * nnoremap <silent>         <C-k>r         :<C-u>Resetf<CR>
	autocmd FileType * nnoremap <silent>         <C-k><C-Space> :<C-u>call <SID>toggle_case()<CR>
	autocmd FileType * inoremap                  <C-k><C-k>     <C-o>"_d$
	autocmd FileType * inoremap                  <C-k><C-i>     <C-o>:set infercase! infercase?<CR>
	autocmd FileType * inoremap                  <C-k><C-y>     <Esc>k"zyyjV"zp:let @z = ''<CR>A
	autocmd FileType * cnoremap                  <C-k><C-p>     <Up>
	autocmd FileType * cnoremap                  <C-k><C-n>     <Down>

	" If cannot use default, You can use this.
	autocmd FileType * nnoremap                  <C-k><C-z> <C-z>
	autocmd FileType * inoremap                  <C-k><C-z> <C-z>
	autocmd FileType * inoremap                  <C-k><C-l> <Esc>
augroup END

" }}}
" Toggle options {{{

augroup KeyMapping
	autocmd FileType * nnoremap <silent>         <C-h><C-w>      :<C-u>setl wrap!           wrap?<CR>
	autocmd FileType * nnoremap <silent>         <C-h><C-i>      :<C-u>set  ignorecase!     ignorecase?<CR>
	autocmd FileType * nnoremap <silent>         <C-h><C-c>      :<C-u>set  cursorline!     cursorline?<CR>
	autocmd FileType * nnoremap <silent>         <C-h><C-e>      :<C-u>set  expandtab!      expandtab?<CR>
	autocmd FileType * nnoremap <silent>         <C-h><C-r>      :<C-u>set  relativenumber! relativenumber?<CR>
	autocmd FileType * nnoremap <silent>         <C-h><C-l>      :<C-u>set  list!           list?<CR>
	autocmd FileType * nnoremap <silent>         <C-h><C-n>      :<C-u>set  number!         number?<CR>
	autocmd FileType * nnoremap <silent>         <C-h><C-d>      :<C-u>call <SID>toggle_diff_mode()<CR>
	autocmd FileType * nnoremap <silent>         <C-h><C-v>      :<C-u>call <SID>toggle_virtual_edit()<CR>
augroup END

" }}}
" for Plugins {{{

augroup KeyMapping
	" open-browser.vim
	autocmd FileType * nmap              <leader>w          <Plug>(openbrowser-open)

	" excitetranslate-vim
	autocmd FileType * nnoremap <silent> <leader>t          :<C-u>ExciteTranslate<CR>

	" Unite
	autocmd FileType * nnoremap <silent> <C-k><C-u><C-f>    :<C-u>Unite -ignorecase outline:foldings<CR>
	autocmd FileType * nnoremap <silent> <C-w>~             :<C-u>Unite -ignorecase neomru/file<CR>

	" vim-over
	autocmd FileType * nnoremap <silent> :%s/               :<C-u>OverCommandLine<CR>%s/
	autocmd FileType * nnoremap <silent> :s/                :<C-u>OverCommandLine<CR>s/
	autocmd FileType * vnoremap <silent> :s/                :<C-u>OverCommandLine<CR>'<,'>s/

	" vimshell
	autocmd FileType * nnoremap <silent> <leader>v          :<C-u>VimShell -split-command=vsp -toggle<CR>
	autocmd FileType * nnoremap <silent> <leader><leader>v  :<C-u>VimShell -split-command=sp  -toggle<CR>
	autocmd FileType * nnoremap <silent> <leader>V          :<C-u>VimShellBufferDir -create<CR>
	autocmd FileType * nnoremap <silent> <leader><leader>V  :<C-u>VimShell -split-command=tab -create<CR>

	" netrw
	autocmd FileType * nnoremap <silent> <leader>e          :<C-u>Vexplore<CR>
	autocmd FileType * nnoremap <silent> <leader><leader>e  :<C-u>Sexplore<CR>
	autocmd FileType * nnoremap <silent> <leader>E          :<C-u>Explore<CR>
	autocmd FileType * nnoremap <silent> <leader><leader>E  :<C-u>Texplore<CR>

	" anzu-chan
	autocmd FileType * nmap              n                  <Plug>(anzu-n-with-echo)zv
	autocmd FileType * nmap              N                  <Plug>(anzu-N-with-echo)zv
	autocmd FileType * nmap              *                  <Plug>(anzu-star-with-echo)zv
	autocmd FileType * nmap              #                  <Plug>(anzu-sharp-with-echo)zv
	autocmd FileType * nmap              <C-w>*             <C-w><C-v><Plug>(anzu-star-with-echo)zv
	autocmd FileType * nmap              <C-w>#             <C-w><C-v><Plug>(anzu-sharp-with-echo)zv

	" vim-over
	autocmd FileType * OverCommandLineNoremap <C-l> <Esc>

	" incsearch.vim
	autocmd FileType * nmap <expr>       /                  foldclosed('.') > -1 ? 'zv<Plug>(incsearch-forward)'  : '<Plug>(incsearch-forward)'
	autocmd FileType * nmap <silent>     \/                 /\m\C
	autocmd FileType * nmap <silent>     \\/                /\m\C\<\>[Left][Left]
	autocmd FileType * nmap              g/                 /<C-r>"<CR>
	autocmd FileType * nmap <expr>       ?                  foldclosed('.') > -1 ? 'zv<Plug>(incsearch-backward)' : '<Plug>(incsearch-backward)'
	autocmd FileType * nmap <silent>     \?                 ?\m\C
	autocmd FileType * nmap <silent>     \\?                ?\m\C\<\>[Left][Left]
	autocmd FileType * nmap              g?                 ?<C-r>"<CR>

	" TaskList.vim
	autocmd FileType * nnoremap <leader>T :<C-u>TaskList<CR>

	" undotree
	autocmd FileType * nnoremap <leader>u :<C-u>UndotreeToggle<CR>
augroup END

" }}}

" }}}
" Buffer Local KeyMaps {{{

" To Plugin buffers
augroup PluginPrefs
	autocmd FileType help     nnoremap <silent><buffer> Q :<C-u>helpclose<CR>

	autocmd FileType netrw    nmap             <buffer> H         -
	autocmd FileType netrw    nnoremap         <buffer> L         <NOP>
	autocmd FileType netrw    nnoremap <silent><buffer> Q         :<C-u>quit<CR>
	autocmd FileType netrw    nnoremap <silent><buffer> ~         :<C-u>execute 'Explore' expand('~')<CR>
	autocmd FileType netrw    nnoremap <silent><buffer> <leader>e :<C-u>quit<CR>
	autocmd FileType netrw    nnoremap <silent><buffer> V         :<C-u>vertical split<CR>
	autocmd FileType netrw    nnoremap <silent><buffer> S         :<C-u>split<CR>
	autocmd FileType netrw    nnoremap         <buffer> s         <NOP>

	autocmd FileType unite    inoremap         <buffer> <C-l> <Esc>

	autocmd FileType quickrun nnoremap <silent><buffer> Q     :<C-u>quit<CR>

	autocmd FileType tweetvim     nmap             <buffer> <leader>R <Plug>(tweetvim_action_remove_status)
	autocmd FileType tweetvim     nmap             <buffer> <C-r>     <Plug>(tweetvim_action_reload)
	autocmd FileType tweetvim     nnoremap <silent><buffer> s         :<C-u>TweetVimSay<CR>
	autocmd FileType tweetvim     nnoremap         <buffer> <C-a>     :<C-u>TweetVimSwitchAccount<Space>
	autocmd FileType tweetvim     nnoremap         <buffer> U         :<C-u>TweetVimUserTimeline<Space>
	autocmd FileType tweetvim     nnoremap <silent><buffer> Q         :<C-u>bdelete<CR>
	" avoid <C-j> to say
	autocmd FileType tweetvim_say nnoremap         <buffer> <C-j>     <C-o>o
	autocmd FileType tweetvim_say inoremap         <buffer> <C-i>     <Tab>

	"NOTE: I don't use nunmap here,
	"      because happned exception 'no such keymapping' when reload filetype.
	autocmd FileType vimshell  nnoremap <buffer> Q          gQ
	autocmd FileType vimshell  nnoremap <buffer> q          <NOP>
	autocmd FileType vimshell  nnoremap <buffer> <C-n>      gt
	autocmd FileType vimshell  nnoremap <buffer> <C-p>      gT
	autocmd FileType vimshell  nnoremap <buffer> <C-l>      <NOP>
	autocmd FileType vimshell  nmap     <buffer> <C-]>      <Plug>(vimshell_clear)
	autocmd FileType vimshell  nmap     <buffer> gj         <Plug>(vimshell_next_prompt)
	autocmd FileType vimshell  nmap     <buffer> gk         <Plug>(vimshell_previous_prompt)
	autocmd FileType vimshell  nmap     <buffer> <C-]>      <Plug>(vimshell_clear)
	autocmd FileType vimshell  inoremap <buffer> <C-l>      <Esc>
	autocmd FileType vimshell  inoremap <buffer> <C-b>      <Left>
	autocmd FileType vimshell  inoremap <buffer> <C-f>      <Right>
	autocmd FileType vimshell  inoremap <buffer> <C-e>      <End>
	autocmd FileType vimshell  inoremap <buffer> <C-n>      <Esc>gt
	autocmd FileType vimshell  inoremap <buffer> <C-p>      <Esc>gT
	autocmd FileType vimshell  inoremap <buffer> <C-d>      <Del>
	autocmd FileType vimshell  imap     <buffer> <C-n>      <C-o><Plug>(vimshell_next_prompt)<End>
	autocmd FileType vimshell  imap     <buffer> <C-p>      <C-o><Plug>(vimshell_previous_prompt)<End>
	autocmd FileType vimshell  imap     <buffer> <C-]>      <Plug>(vimshell_clear)
	autocmd FileType vimshell  imap     <buffer> <C-j>      <Plug>(vimshell_enter)
	autocmd FileType vimshell  imap     <buffer> <C-k><C-p> <Plug>(vimshell_history_unite)

	autocmd FileType int-*     nnoremap <buffer> q          <NOP>
	autocmd FileType int-*     nnoremap <buffer> <C-n>      gt
	autocmd FileType int-*     nnoremap <buffer> <C-p>      gT
	autocmd FileType int-*     nnoremap <buffer> <C-l>      <NOP>
	autocmd FileType int-*     nmap     <buffer> <C-]>      <Plug>(vimshell_int_clear)
	autocmd FileType int-*     nmap     <buffer> Q          <Plug>(vimshell_int_exit)
	autocmd FileType int-*     nmap     <buffer> gj         <Plug>(vimshell_int_next_prompt)
	autocmd FileType int-*     nmap     <buffer> gk         <Plug>(vimshell_int_previous_prompt)
	autocmd FileType int-*     inoremap <buffer> <C-l>      <Esc>
	autocmd FileType int-*     inoremap <buffer> <C-b>      <Left>
	autocmd FileType int-*     inoremap <buffer> <C-f>      <Right>
	autocmd FileType int-*     inoremap <buffer> <C-e>      <End>
	autocmd FileType int-*     inoremap <buffer> <C-d>      <Del>
	autocmd FileType int-*     imap     <buffer> <C-n>      <C-o><Plug>(vimshell_int_next_prompt)<End>
	autocmd FileType int-*     imap     <buffer> <C-p>      <C-o><Plug>(vimshell_int_previous_prompt)<End>
	autocmd FileType int-*     imap     <buffer> <C-]>      <C-o><Plug>(vimshell_int_clear)
	autocmd FileType int-*     imap     <buffer> <CR>       <Plug>(vimshell_int_execute_line)
	autocmd FileType int-*     imap     <buffer> <C-k><C-p> <Plug>(vimshell_int_history_unite)

	autocmd FileType J6uil     nnoremap <silent><buffer> Q         :<C-u>bdelete<CR>
	autocmd FileType J6uil_say nnoremap         <buffer> <C-j>     <NOP>

	autocmd FileType w3m       nmap             <buffer> H         <BS>
	autocmd FileType w3m       nnoremap <silent><buffer> <C-u>     :<C-u>W3mAddressBar <CR>
	autocmd FileType w3m       nnoremap <silent><buffer> <leader>E :<C-u>W3mShowExtenalBrowser <CR>

	autocmd FileType ref-*     nnoremap <silent><buffer> Q         :<C-u>quit<CR>

	autocmd FileType git-log.git-diff nnoremap <silent><buffer> Q         :<C-u>bdelete<CR>
	autocmd FileType markdown         nnoremap <silent><buffer> <leader>r :<C-u>PrevimOpen<CR>
augroup END

" }}}


"-------------------------"
"       File_Types        "
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


" If buffer does not has filetype, set filetype 'none'
augroup ExtensionType
	autocmd VimEnter,BufNew * if &ft == '' | setf none | endif
augroup END


augroup FileEvent
	" -- Vi Improved --
	autocmd VimEnter,ColorScheme * highlight rcMyHint cterm=standout ctermfg=DarkYellow
	autocmd BufWinEnter          * let s:rcMyHint = s:matchadd_with_filetype('vim', 'rcMyHint', '\s*"\zs@\w\+(.*)\ze', 10, get(s:, 'rcMyHint', 10001))

	" Haskell
	autocmd VimEnter,ColorScheme * highlight rcHeadSpace cterm=underline ctermfg=DarkGray
	autocmd BufWinEnter          * let s:rcHeadSpace = s:matchadd_with_filetype('haskell', 'rcHeadSpace', '^\s\+', 10, get(s:, 'rcHeadSpace', 10002))
	autocmd FileType haskell setl ts=2 sw=2 et
	autocmd FileType yesod   setl ts=4 sw=4 et

	"@Marked('do not depends filetype, but depends extend type...tabun') <- ???
	" C-Sharp
	autocmd VimEnter,ColorScheme * highlight RcTypeInference cterm=bold ctermfg=11
	autocmd VimEnter,WinEnter    *.cs syntax keyword RcTypeInference var

	" Ruby
	autocmd FileType ruby,eruby setl ts=2 sw=2 et

	" CSS
	autocmd FileType css,scss setl ts=2 sw=2 et

	" Plain Text FileTypes
	autocmd FileType markdown,text setl tw=0 ts=2 sw=2 et

	" SQL Languages
	autocmd FileType mysql setl ts=4 sw=4 et

	" Markup Languages
	autocmd FileType html,xml setl ts=4 sw=4 et

	" FileTypes commentstrings
	autocmd FileType vim           let &commentstring = ' "%s'
	autocmd FileType java,cs,cpp,c let &commentstring = ' /*%s*/'
	autocmd FileType haskell,mysql let &commentstring = ' -- %s'
	autocmd FileType coq           let &commentstring = ' (*%s*)'
	autocmd FIleType ruby,sh       let &commentstring = ' #%s'
	autocmd FileType markdown      let &commentstring = '<!--%s-->'
	autocmd FileType text,none     let &commentstring = ' %s'
augroup END


" For Plugin Types
augroup FileEvent
	"@Incomplete('do not functioned')
	"@Incomplete('do not functioned as exchanger')
	" netrw
	autocmd FileType netrw highlight default link CursorLine Visual

	" TweetVim
	autocmd FileType tweetvim setl cursorline

	" vimshell.vim
	autocmd FileType vimshell setl ts=8

	" ConqueTerm
	autocmd FileType conque_term setl nolist

	" gitlogviewer
	autocmd FileType git-log.git-diff setl nolist
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


filetype plugin indent on
syntax enable
let g:vimrc['loaded'] = 1

