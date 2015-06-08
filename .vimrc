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
" -- Command_Util
" -- Key_Mapping
" -- File_Type
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

"-- Unite outline ? -> view C-Sharp <summary>~</summary> with method name

"-- separetaro.vim -> operator between separetor (long|short) and separator (long|short)

"-- Buffer Memo of Marks

" }}}
" Issues {{{

"-- C-o hard use on vimshell

"-- automatic mkdir './C:' when execute NeoBundleInstall in windows kaoriya
"  -- does neobundle think that is repository...?

"-- shot-f doesn't functioned in i_<C-o> temporary normal mode

"-- conflicted? vim-ruby and rspec.vim when those was set NeoBundleLazy
"  -- does not loaded syntax of rspec.vim

"-- echo warning message by netobundle when reloading this

"-- submode wintab-move over 1 previous tab

"-- bashrc or bash_profile lead up burden of load

"-- int-git does not loaded ftplugin int-git.vim (by hyphenate ?)

"}}}
" Todo {{{

"-- Eigo translate to English in this file

"-- read help options.jax

"-- read help windows.txt

"-- read help 'cino'

"-- reference to help 'ftplugin' L2159

"-- foldmethod for C# methods

" }}}


"----------------------------------------
" {- Hints -} "
" @Bugs         => This hoge has the bugs.
" @Incomplete   => This is not completed making.
" @Unchecked    => This was not unchecked that is operate right
" @Unsupported  => Do not supported functions when now.
" @Unknown      => I don't know why this functioned.
"     ／人◕ ‿‿ ◕人＼ <  Wakega wakaranaiyo!
" @Unused       => Not used this yet now, needs inquires deleting this.
" @Deprecated   => Deprecated This vimrc Version.
" @Experiment   => This is experimental implementation.
" @Marked       => I have eye on this.
" @See          => Referred URL, Saw Document, and etc...
" @Code         => A sample code using it
"-------------------
" Designating the target platform.
" @Hoge{Win,Ubuntu}  : This Hint for Win and Ubuntu.
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

let g:vimrc = get(g:, 'vimrc', {'loaded' : 0})

let s:vimrc_env = expand('~/.vimrc_env')
let s:vim_home  = expand('~/.vim')

let s:is_windows = has('win32')
let s:is_cygwin  = has('win32unix')
let s:is_kaoriya = has('kaoriya')
let s:is_doswin  = s:is_windows && !s:is_cygwin && !has('gui')
let s:is_unix    = has('unix')

let s:has_cygwin = executable('cygstart')
let s:has_mingw  = 0  "NOTE: ('dummy')

let s:backupdir = expand('~/.backup/vim_backup')
let s:directory = s:backupdir . '/swp'
let s:undodir   = s:backupdir . '/undo'
let s:viewdir   = s:backupdir . '/view'

let s:username  = $USER
let s:groupname = $GROUP !=# '' ? $GROUP : $USER

"}}}


"---------------------"
"    Local_Function   "
"---------------------"
"{{{

function! s:system(cmd)
	"@Incomplete('vimproc#system was not executed in this script, refer to vital.vim')
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
" file encoding {{{

" Default file encoding
if g:vimrc['loaded']
	set fileencoding=utf-8 encoding=utf-8
endif

" Encoding for this script
scriptencoding utf-8

" }}}
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

if s:is_kaoriya && s:is_windows
	" Set Environment
	let $HOME        = $VIM
	let s:vim_home   = $VIM . '/vimfiles'  " Reset with $HOME
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
	let s:switch_dir       = $VIM . '/switches/enabled'
	let s:suppress_vimproc = s:switch_dir . '/disable-vimproc.vim'

	" If you has mingw, your vim built bundled vimproc.vim...maybe
	if !s:has_mingw && filereadable(s:suppress_vimproc)
		call delete(s:suppress_vimproc)
	elseif s:has_mingw && !filereadable(s:suppress_vimproc)
		call writefile([], s:suppress_vimproc)
	endif

	unlet s:suppress_vimproc


	" Enable kaoriya plugins
	for s:disf in map(['/utf-8.vim', '/vimdoc-ja.vim'], 's:switch_dir . v:val')
		if !filereadable(s:disf)
			call writefile([], s:disf)
		endif
	endfor

	unlet s:switch_dir s:disf


	" Unset Kaoriya Preference
	set noignorecase nosmartcase

	" I said 'you must be open vimrc with utf-8...'
	autocmd FileEvent BufRead $MYVIMRC setl enc=utf8 fenc=utf8
endif

"}}}
" Check NeoBundle exists {{{
let s:bundledir    = s:vim_home . '/bundle'
let s:neobundledir = s:bundledir . '/neobundle.vim'

if !isdirectory(s:bundledir)
	call mkdir(s:bundledir)
endif

function! s:remove_empty_bundledir()  "{{{
	" get bundled plugin directory names
	let l:dirs = split(glob('~/vimfiles/bundle/*'), '\n')
	let l:dir_names = map(l:dirs, 'fnamemodify(v:val, ":t")')

	for l:dir_name in l:dir_names
		let l:plugin_dir   = s:bundledir . '/' . l:dir_name

		let l:is_empty_dir = glob('autoload/*') ==# ''

		if l:is_empty_dir
			"@Deprecated('should not be use unix command')
			call s:system('rmdir ' . l:plugin_dir)
		endif
	endfor
endfunction  "}}}
function! s:fetch_neobundle() " {{{
	if executable('git')
		echo 'NeoBundle was not installed...'
		echo 'Installing NeoBundle.'

		execute '!git clone http://github.com/Shougo/neobundle.vim' s:neobundledir
	else
		call s:echo_error('Sorry, You do not have git command.')
		call s:echo_error('Cannot introduce NeoBundle.')

		throw 'FALIED: cloning neobundle.vim failed.'
	endif
endfunction " }}}

if has('vim_starting')
	try
		let &runtimepath = &runtimepath . ',' . s:vim_home . '/bundle/neobundle.vim'

		call neobundle#begin()
	catch /E117/
		if isdirectory(s:neobundledir) && exists(':NeoBundle') isnot 2
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
		catch /FALIED/
			call s:echo_error('cloning neobundle.vim failed.')
			call s:echo_error('>> Vim Config Error <<')
		endtry
	endtry
endif

unlet s:neobundledir
unlet s:bundledir
"}}}
" Check Backup, Swap and Undo directory exists {{{

if !isdirectory(s:backupdir)
	call mkdir(s:backupdir, 'p', 0700)
	call s:system(printf('chown -R %s:%s %s', s:username, s:groupname, s:backupdir))
endif

if !isdirectory(s:directory)
	call mkdir(s:directory, 'p', 0700)
	call s:system(printf('chown -R %s:%s %s', s:username, s:groupname, s:directory))
endif

if !isdirectory(s:undodir)
	call mkdir(s:undodir, 'p', 0700)
	call s:system(printf('chown -R %s:%s %s', s:username, s:groupname, s:undodir))
endif

"}}}
" Enable matchit.vim {{{

if !exists('loaded_matchit')
	" Load it
	runtime macros/matchit.vim

	"@See('dokka matchit.txt...dokoitta?')
	" If I don't have matchit document, I get it
	let s:matchit_doc_from = expand('$VIMRUNTIME/macros/matchit.txt')
	let s:matchit_doc_to   = s:vim_home . '/doc/matchit.txt'

	if !filereadable(s:matchit_doc_to)
		call writefile(readfile(s:matchit_doc_from), s:matchit_doc_to)
	endif

	unlet s:matchit_doc_to s:matchit_doc_from
endif

"}}}


"-------------------------"
"     Plugin_Manage       "
"-------------------------"
"*** Plugin List ***"{{{

NeoBundleFetch 'Shougo/neobundle.vim'
NeoBundleLazy  'basyura/twibill.vim'
NeoBundle      'tyru/open-browser.vim'
NeoBundleLazy  'basyura/bitly.vim'
NeoBundle      'Shougo/unite.vim'
NeoBundle      'Shougo/vimproc.vim'
NeoBundleLazy  'basyura/TweetVim'
NeoBundle      'mattn/webapi-vim'
NeoBundle      'Shougo/vimshell.vim'
NeoBundle      'thinca/vim-quickrun'
NeoBundleLazy  'basyura/J6uil.vim'
NeoBundle      'osyo-manga/vim-gyazo'
NeoBundleLazy  'yuratomo/w3m.vim'
NeoBundle      'supermomonga/vimshell-kawaii.vim'
NeoBundle      'mattn/excitetranslate-vim'
NeoBundleLazy  'thinca/vim-splash'
NeoBundleLazy  'supermomonga/jazzradio.vim'
NeoBundle      'mattn/favstar-vim'
NeoBundleLazy  'ujihisa/unite-colorscheme'
NeoBundleLazy  'Shougo/vinarise.vim'
NeoBundleLazy  'mattn/gist-vim'
NeoBundle      'thinca/vim-ref'
NeoBundle      'ujihisa/ref-hoogle'
NeoBundleLazy  'vim-jp/vital.vim'
NeoBundle      'Shougo/unite-outline'
NeoBundleLazy  'rbtnn/puyo.vim'
NeoBundleLazy  'mattn/benchvimrc-vim'
NeoBundleLazy  'mattn/yamada-vim'
NeoBundleLazy  'jvoorhis/coq.vim'
NeoBundleLazy  'eagletmt/coqtop-vim'
NeoBundle      'rhysd/vim-grammarous'
NeoBundleLazy  'thinca/vim-themis'
NeoBundle      'tomasr/molokai'
NeoBundleLazy  'kannokanno/previm'
NeoBundle      'LeafCage/foldCC'
NeoBundleLazy  'katono/rogue.vim'
NeoBundleLazy  'kamichidu/vim-benchmark'
NeoBundle      'kana/vim-submode'
NeoBundle      'mfumi/ref-dicts-en'
NeoBundleLazy  'thinca/vim-painter'
NeoBundle      'osyo-manga/vim-anzu'
NeoBundle      'osyo-manga/vim-over'
NeoBundleLazy  'tyru/restart.vim'
NeoBundle      'vim-jp/vimdoc-ja'
NeoBundleLazy  'rbtnn/game_engine.vim'
NeoBundle      'h1mesuke/vim-alignta'
NeoBundle      'haya14busa/incsearch.vim'
NeoBundleLazy  'thinca/vim-scouter'
NeoBundle      'deris/vim-shot-f'
NeoBundleLazy  'sgelb/TaskList.vim'
NeoBundle      'tyru/vim-altercmd'
NeoBundleLazy  'mbbill/undotree'
NeoBundle      'Shougo/neomru.vim'
NeoBundleLazy  'aiya000/adrone.vim'
NeoBundleFetch 'Shougo/fakecygpty'
NeoBundle      'nathanaelkane/vim-indent-guides'
NeoBundleLazy  'LeafCage/vimhelpgenerator'
NeoBundleLazy  'thinca/vim-threes'
NeoBundleLazy  'vim-ruby/vim-ruby'
NeoBundleLazy  'Keithbsmiley/rspec.vim'
NeoBundle      'altercation/vim-colors-solarized'
NeoBundle      'aiya000/aho-bakaup.vim'
NeoBundle      'chrisbra/vim-diff-enhanced'
NeoBundle      'Shougo/neosnippet.vim'
NeoBundle      'Shougo/neosnippet-snippets'
NeoBundle      'aiya000/separetaro.vim'
NeoBundleLazy  'kurocode25/mdforvim'
NeoBundleLazy  'rbtnn/vimconsole.vim'
NeoBundleLazy  'fatih/vim-go'
NeoBundle      'tpope/vim-surround'
NeoBundle      'kana/vim-textobj-user'
NeoBundleLazy  'osyo-manga/vim-itunes-bgm'
NeoBundle      'kana/vim-textobj-indent'
NeoBundle      'Lokaltog/vim-easymotion'

"}}}
"*** Plugin Depends and Auto Config ***" {{{

if neobundle#tap('vimproc.vim')
	call neobundle#config('vimproc.vim', {
	\	'build' : {
	\		'linux'   : 'make -f make_unix.mak',
	\		'unix'    : 'make -f make_unix.mak',
	\		'mac'     : 'make -f make_mac.mak',
	\		'cygwin'  : 'make -f make_cygwin.mak',
	\		'windows' : 'make -f make_mingw32.mak'
	\	},
	\	'disabled' : s:is_kaoriya && s:is_windows && !s:has_mingw
	\})
	call neobundle#untap()
endif
if neobundle#tap('TweetVim')
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
	call neobundle#untap()
endif
if neobundle#tap('vimshell.vim')
	call neobundle#config('vimshell.vim', {
	\	'depends'  : 'Shougo/vimproc.vim'
	\})
	call neobundle#untap()
endif
if neobundle#tap('J6uil.vim')
	call neobundle#config('J6uil.vim', {
	\	'depends' : [
	\		'mattn/webapi-vim',
	\		'Shougo/vimproc.vim',
	\		'tyru/open-browser.vim',
	\		'Shougo/unite.vim'
	\	],
	\	'autoload' : {'commands' : 'J6uil'}
	\})
	call neobundle#untap()
endif
if neobundle#tap('vim-gyazo')
	call neobundle#config('vim-gyazo', {
	\	'depends' : [
	\		'tyru/open-browser.vim',
	\		'basyura/TweetVim'
	\	]
	\})
	call neobundle#untap()
endif
if neobundle#tap('w3m.vim')
	call neobundle#config('w3m.vim', {
	\	'autoload' : {'commands' : [
	\		'W3m',
	\		'W3mHistory',
	\		'W3mHistoryClear',
	\		'W3mLocal',
	\		'W3mSplit',
	\		'W3mTab',
	\		'W3mVSplit'
	\	]}
	\})
	call neobundle#untap()
endif
if neobundle#tap('vimshell-kawaii.vim')
	call neobundle#config('vimshell-kawaii.vim', {
	\	'depends'  : 'Shougo/vimshell.vim'
	\})
	call neobundle#untap()
endif
if neobundle#tap('vim-splash')
	call neobundle#config('vim-splash', {
	\	'autoload' : {'commands' : 'Splash'}
	\})
	call neobundle#untap()
endif
"@Bugs('not fuond jazzradio#channel_id_comlete')
if neobundle#tap('jazzradio.vim')
	call neobundle#config('jazzradio.vim', {
	\	'depends' : 'Shougo/unite.vim',
	\	'external_commands' : 'mplayer',
	\	'autoload' : {
	\		'unite_sources'   : 'jazzradio',
	\		'function_prefix' : 'Jazzradio',
	\		'commands'        : [
	\			'JazzradioUpdateChannels',
	\			'JazzradioStop', {
	\				'name'     : 'JazzradioPlay',
	\				'complete' : 'customlist,jazzradio#channel_id_comlete'
	\			}
	\		]
	\	}
	\})
	call neobundle#untap()
endif
if neobundle#tap('unite-colorscheme')
	call neobundle#config('unite-colorscheme', {
	\	'autoload' : {'unite_sources' : 'colorscheme'}
	\})
	call neobundle#untap()
endif
if neobundle#tap('vinarise.vim')
	call neobundle#config('vinarise.vim', {
	\	'autoload' : {'commands' : [
	\		'Vinarise',
	\		'VinariseDump'
	\	]}
	\})
	call neobundle#untap()
endif
if neobundle#tap('gist-vim')
	call neobundle#config('gist-vim', {
	\	'autoload' : {'commands' : 'Gist'}
	\})
	call neobundle#untap()
endif
if neobundle#tap('ref-hoogle')
	call neobundle#config('ref-hoogle', {
	\	'depends'  : 'thinca/vim-ref'
	\})
	call neobundle#untap()
endif
if neobundle#tap('vital.vim')
	call neobundle#config('vital.vim', {
	\	'autoload' : {'filetypes' : 'vim'}
	\})
	call neobundle#untap()
endif
if neobundle#tap('puyo.vim')
	call neobundle#config('puyo.vim', {
	\	'depends'  : 'rbtnn/game_engine.vim',
	\	'autoload' : {'commands' : 'Puyo'}
	\})
	call neobundle#untap()
endif
if neobundle#tap('benchvimrc-vim')
	call neobundle#config('benchvimrc-vim', {
	\	'autoload' : {'commands' : 'BenchVimrc'}
	\})
	call neobundle#untap()
endif
if neobundle#tap('yamada-vim')
	call neobundle#config('yamada-vim', {
	\	'autoload' : {'commands' : 'Yamada'}
	\})
	call neobundle#untap()
endif
if neobundle#tap('coq.vim')
	call neobundle#config('coq.vim', {
	\	'autoload' : {'filetypes' : 'coq'}
	\})
	call neobundle#untap()
endif
if neobundle#tap('coqtop-vim')
	call neobundle#config('coqtop-vim', {
	\	'depends'  : 'Shougo/vimproc.vim',
	\	'autoload' : {'filetypes' : 'coq'}
	\})
	call neobundle#untap()
endif
if neobundle#tap('vim-themis')
	call neobundle#config('vim-themis', {
	\	'autoload' : {'filetypes' : [
	\		'vim',
	\		'vimspec'
	\	]}
	\})
	call neobundle#untap()
endif
if neobundle#tap('previm')
	call neobundle#config('previm', {
	\	'autoload' : {'commands' : 'PrevimOpen'}
	\})
	call neobundle#untap()
endif
if neobundle#tap('rogue.vim')
	call neobundle#config('rogue.vim', {
	\	'autoload' : {'commands' : [
	\		'Rogue',
	\		'RogueRestore',
	\		'RogueResume',
	\		'RogueScores'
	\	]}
	\})
	call neobundle#untap()
endif
if neobundle#tap('ref-dicts-en')
	call neobundle#config('ref-dicts-en', {
	\	'depends' : 'thinca/vim-ref'
	\})
	call neobundle#untap()
endif
if neobundle#tap('vim-painter')
	call neobundle#config('vim-painter', {
	\	'gui'      : 1,
	\	'autoload' : {'commands' : 'PainterStart'}
	\})
	call neobundle#untap()
endif
if neobundle#tap('restart.vim')
	call neobundle#config('restart.vim', {
	\	'gui'      : 1,
	\	'autoload' : {'commands' : 'Restart'}
	\})
	call neobundle#untap()
endif
if neobundle#tap('vim-scouter')
	call neobundle#config('vim-scouter', {
	\	'autoload' : {'commands' : {
	\		'name'     : 'Scouter',
	\		'complete' : 'file'
	\	}}
	\})
	call neobundle#untap()
endif
if neobundle#tap('TaskList.vim')
	call neobundle#config('TaskList.vim', {
	\	'autoload' : {
	\		'commands' : [
	\			'TaskList',
	\			'TaskListToggle'
	\		],
	\		'mappings' : '<Plug>TaskListToggle'
	\	}
	\})
	call neobundle#untap()
endif
if neobundle#tap('undotree')
	call neobundle#config('undotree', {
	\	'autoload' : {'commands' : [
	\		'UndotreeToggle',
	\		'UndotreeFocus',
	\		'UndotreeShow',
	\		'UndotreeHide'
	\	]}
	\})
	call neobundle#untap()
endif
if neobundle#tap('adrone.vim')
	call neobundle#config('adrone.vim', {
	\	'autoload' : {'commands' : [
	\		'AdroneOpen',
	\		'AdroneSay',
	\		'AdroneVersion'
	\	]}
	\})
	call neobundle#untap()
endif
if neobundle#tap('fakecygpty')
	call neobundle#config('fakecygpty', {
	\	'build' : {
	\		'windows' : expand((executable('clang') ? 'clang' : 'gcc') . ' fakecygpty.c -o ~/bin/fakecygpty.exe')
	\	}
	\})
	call neobundle#untap()
endif
if neobundle#tap('vimhelpgenerator')
	call neobundle#config('vimhelpgenerator', {
	\	'autoload' : {'commands' : [
	\		'VimHelpGenerator',
	\		'VimHelpGeneratorVirtual',
	\		'HelpIntoMarkdown'
	\	]}
	\})
	call neobundle#untap()
endif
if neobundle#tap('vim-threes')
	call neobundle#config('vim-threes', {
	\	'autoload' : {'commands' : [
	\		'ThreesShowRecord',
	\		'ThreesStart'
	\	]}
	\})
	call neobundle#untap()
endif
if neobundle#tap('vim-ruby')
	"@Bugs('rspec.vim do not highlight syntax before loading vim-ruby')
	"@Marked(' -> was fixed by commit a516 ?')
	call neobundle#config('vim-ruby', {
	\	'autoload' : {'filetype' : 'ruby'}
	\})
	call neobundle#untap()
endif
if neobundle#tap('rspec.vim')
	call neobundle#config('rspec.vim', {
	\	'autoload' : {'filetype' : 'ruby'}
	\})
	call neobundle#untap()
endif
if neobundle#tap('mdforvim')
	call neobundle#config('mdforvim', {
	\	'autoload' : {'commands' : [
	\		'MdConvert',
	\		'MdPreview',
	\		'MdStopPreview', {
	\			'name'     : 'MdSaveAs',
	\			'complete' : 'file'
	\		}
	\	]}
	\})
	call neobundle#untap()
endif
if neobundle#tap('vimconsole.vim')
	call neobundle#config('vimconsole.vim', {
	\	'autoload' : {'commands' : [
	\		'VimConsoleLog',
	\		'VimConsoleOpen',
	\		'VimConsoleClose',
	\		'VimConsoleToggle',
	\		'VimConsoleClear',
	\		'VimConsoleRedraw'
	\	]}
	\})
	call neobundle#untap()
endif
if neobundle#tap('vim-go')
	call neobundle#config('vim-go', {
	\	'autoload' : {'filetypes' : 'go'}
	\})
	call neobundle#untap()
endif
"@Incomplete('add hook set updatetime shortly')
if neobundle#tap('vim-itunes-bgm')
	call neobundle#config('vim-itunes-bgm', {
	\	'depends'           : 'vimproc.vim',
	\	'external_commands' : 'mplayer',
	\	'autoload'          : {'commands' : 'ITunesBGMStart'}
	\})
	call neobundle#untap()
endif
if neobundle#tap('vim-textobj-indent')
	call neobundle#config('vim-textobj-indent', {
	\	'depends' : 'vim-textobj-user'
	\})
	call neobundle#untap()
endif
if neobundle#tap('vim-textobj-indent')
	call neobundle#config('vim-textobj-indent', {
	\	'depends' : 'vim-textobj-user'
	\})
	call neobundle#untap()
endif


call neobundle#end()

" }}}


"------------------------"
"*** Plugin_Configure ***"
"------------------------"
"--- netrw ---" {{{

" Enable netrw preview
let g:netrw_preview = 1

" Place for .netwhist and .netrwbook
let g:netrw_home = s:vim_home

" }}}
"--- matchit.vim ---" {{{

"" uooooooooooooo... oh, my triple operator !!!!!!!!!!!
"" Why if set you, happend an error when doing match it...
"autocmd PluginPrefs User MyVimRc let b:match_words = &matchpairs . ',?::'

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
"@Unchecked('java'){Ubuntu}
"@Unchecked('cs'){Ubuntu}

let g:quickrun_config = {
\	'_' : {
\		'split'  : '',
\		'runner' : 'vimproc',
\		'runner/vimproc/updatetime' : 10,
\		'hook/time/enable' : 1
\	},
\	'cpp' : {
\		'cmdopt' : '-std=c++14'
\	},
\	'java' : {
\		'cmdopt' : '-encoding UTF-8 -source 1.8'
\	},
\	'vimspec' : {
\		'command'  : 'themis',
\		'cmdopt'   : '--runtimepath ".."',
\		'exec'     : '%c %o %s:p | tr -d "\r"',
\		'tempfile' :  printf('%s/{tempname()}.vimspec', $TMP)
\	}
\}

" Branch with env
let g:quickrun_config['cs'] = s:is_unix    ? {'command' : 'gmcs'}
\                           : s:is_windows ? {'command' : 'csc.exe', 'hook/output_encode/encoding' : 'cp932:utf-8'}
\                                          : {}

if s:is_windows
	let g:quickrun_config.java['hook/output_encode/encoding'] = 'cp932:utf-8'
elseif s:is_cygwin
	let g:quickrun_config.java['exec']                        = ['%c %o `echo %s | sed s:\:/:g | cygpath -w -f -`', '%c %s:t:r %a']
	let g:quickrun_config.java['hook/output_encode/encoding'] = 'cp932:utf-8'
	let g:quickrun_config.java['tempfile']                    = printf('%s/{tempname()}.java', $TMP)
endif

" }}}
"--- TweetVim ---"{{{

" Smooth post tweet
let g:tweetvim_async_post = 1

"}}}
"--- vimshell.vim ---"{{{

let g:vimshell_no_save_history_commands = {
\	'history': 1,
\	'ls'     : 1,
\	'clear'  : 1
\}
let g:vimshell_enable_transient_user_prompt = 1
let g:vimshell_force_overwrite_statusline   = 1
let g:vimshell_max_command_history          = 10000
let g:vimshell_scrollback_limit             = 10000
let g:vimshell_split_command                = 'split'

" The cd aliases reference to here
let g:vimshell_hereis_file = expand('~/.bashrc_places')

"}}}
"--- vimshell-kawaii.vim ---"{{{

" Prompt is kawaii
let g:vimshell_kawaii_smiley = 1

"}}}
"--- excitetraslate-vim ---"{{{

" Don't yank result to @" register
let g:excitetranslate_options = ["buffer"]

"}}}
"--- w3m.vim ---"{{{

let g:w3m#external_browser = 'firefox'

let g:w3m#homepage = 'http://www.google.co.jp/'

"}}}
"--- vimconsole.vim ---"{{{

" Auto output debug log to console
let g:vimconsole#auto_redraw = 1

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

if neobundle#tap('vim-submode')
	augroup PluginPrefs
		" Window Resizer
		autocmd User MyVimRc call submode#enter_with('window_resize', 'n', '', '<C-s>w')
		autocmd User MyVimRc call submode#map('window_resize', 'n', '', 'j', '<C-w>+')
		autocmd User MyVimRc call submode#map('window_resize', 'n', '', 'k', '<C-w>-')
		autocmd User MyVimRc call submode#map('window_resize', 'n', '', 'h', '<C-w><')
		autocmd User MyVimRc call submode#map('window_resize', 'n', '', 'l', '<C-w>>')
		autocmd User MyVimRc call submode#map('window_resize', 'n', '', '=', '<C-w>=')
		autocmd User MyVimRc call submode#map('window_resize', 'n', '', '_', '<C-w>_')

		" Fold Mover
		autocmd User MyVimRc call submode#enter_with('fold_move', 'n', '', '<C-s>z')
		autocmd User MyVimRc call submode#map('fold_move', 'n', 'e', 'j', 'foldlevel('.') > 0 ? "zczjzozz"   : "zjzozz"')
		autocmd User MyVimRc call submode#map('fold_move', 'n', 'e', 'k', 'foldlevel('.') > 0 ? "zczkzo[zzz" : "zkzo[zzz"')
		autocmd User MyVimRc call submode#map('fold_move', 'n', '',  'h', '[z')
		autocmd User MyVimRc call submode#map('fold_move', 'n', '',  'l', ']z')

		" Buffer Changer
		autocmd User MyVimRc call submode#enter_with('buffer_change', 'n', '', '<C-s>b')
		autocmd User MyVimRc call submode#map('buffer_change', 'n', 's', 'n', ':bnext<CR>')
		autocmd User MyVimRc call submode#map('buffer_change', 'n', 's', 'p', ':bprevious<CR>')

		" Tab Mover
	" s:loopable_tab_move_prev() "{{{

	function! LoopableTabMovePrev()
		if tabpagenr() is 1
			execute ':tabmove' tabpagenr('$')
		else
			execute ':tabmove -1'
		endif
	endfunction

	"}}}
	" s:loopable_tab_move_next() "{{{

	function! LoopableTabMoveNext()
		if tabpagenr() is tabpagenr('$')
			execute ':tabmove 0'
		else
			execute ':tabmove +1'
		endif
	endfunction

	"}}}
		autocmd User MyVimRc call submode#enter_with('tab_move', 'n', '', '<C-s>t')
		autocmd User MyVimRc call submode#map('tab_move', 'n', 's', 'n', ':call LoopableTabMoveNext()<CR>')
		autocmd User MyVimRc call submode#map('tab_move', 'n', 's', 'p', ':call LoopableTabMovePrev()<CR>')

		" WinTab Mover
		" Current buffer move to next tab "{{{

		command! -bar BufTabMovePrev execute 'normal! mZ:hide<CR>gT:vsp<CR>`Z'
		command! -bar BufTabMoveNext execute 'normal! mZ' . (winnr('$') <= 1 ? ':hide<CR>' : ':hide<CR>gt') . ':vsp<CR>`Z'

		"}}}
		autocmd User MyVimRc call submode#enter_with('wintab_move', 'n', '', '<C-s>N', ':BufTabMoveNext<CR>')
		autocmd User MyVimRc call submode#enter_with('wintab_move', 'n', '', '<C-s>P', ':BufTabMovePrev<CR>')
		autocmd User MyVimRc call submode#map('wintab_move', 'n', '', 'N', ':BufTabMoveNext<CR>')
		autocmd User MyVimRc call submode#map('wintab_move', 'n', '', 'P', ':BufTabMovePrev<CR>')
		autocmd User MyVimRc call submode#map('wintab_move', 'n', '', 'H', '<C-w>H')
		autocmd User MyVimRc call submode#map('wintab_move', 'n', '', 'J', '<C-w>J')
		autocmd User MyVimRc call submode#map('wintab_move', 'n', '', 'K', '<C-w>K')
		autocmd User MyVimRc call submode#map('wintab_move', 'n', '', 'L', '<C-w>L')
	augroup END
endif

"}}}
"--- ref-dicts-en ---" {{{
"@See('http://d.hatena.ne.jp/akishin999/20131024/1382569289')

let g:ref_source_webdict_sites = {
\	'weblio' : {
\		'url' : 'http://ejje.weblio.jp/content/%s'
\	}
\}

let g:ref_source_webdict_sites['default'] = 'weblio'

function! s:weblio_filter(output) "{{{
	let l:lines = split(a:output, "\n")

	return join(l:lines[60 : ], "\n")
endfunction "}}}
let g:ref_source_webdict_sites['weblio'].filter = function('s:weblio_filter')

" }}}
"--- restart.vim ---" {{{

let g:restart_sessionoptions = 'blank,curdir,folds,help,localoptions,tabpages'

" }}}
"--- vimdoc-ja ---"{{{

" vimdoc-ja is secondary order
set helplang=en,ja

"}}}
"--- TaskList.vim ---"{{{

" TaskList search these
let g:tlTokenList = ["FIXME", "TODO", "XXX", "NOTE"]

" Open window at bottom
let g:tlWindowPosition = 1

" Restore opened position when closed TaskList
let g:tlRememberPosition = 1

"}}}
"--- adrone.vim ---"{{{

let g:adrone_home_default_keymappings = 0

"}}}
"--- vim-indent-guides ---"{{{

let g:indent_guides_default_mapping = 0
let g:indent_guides_guide_size      = 1
let g:indent_guides_auto_colors     = 0

" Define guide colors
augroup HighlightPref
	autocmd VimEnter,ColorScheme * highlight default link IndentGuidesOdd  PmenuSel
	autocmd VimEnter,ColorScheme * highlight default link IndentGuidesEven Pmenu
augroup END

" If matched file extension pattern, indent-guides is enabled
augroup FileEvent
	autocmd WinEnter,BufWinEnter * IndentGuidesDisable
	autocmd WinEnter,BufWinEnter *.xml,*.html,*css,*scss,*.erb IndentGuidesEnable
augroup END

"}}}
"--- vim-colors-solarized ---"{{{

let g:solarized_contrast = "high"

"}}}
"--- aho-bakaup.vim ---"{{{

" Devolute to Bakaup
set nobackup

" Powered Up Auto File Backup when written
let g:bakaup_backup_dir  = s:backupdir
let g:bakaup_auto_backup = 1

"}}}
"--- neosnippet.vim ---"{{{

" for :NeoSnippetEdit
let g:neosnippet#snippets_directory = s:vim_home . '/neosnippets'

"}}}
"--- separetaro.vim ---"{{{

let g:separetaro_short_separator_of = {
\	'php' : '/* -=-=-=-=-=-=-=-=- */',
\	'go'  : '/* -=-=-=-=-=-=-=-=- */'
\}

let g:separetaro_long_separator_of = {
\	'php' : '/* ---===---===---===---===---===---===--- */',
\	'go'  : '/* ---===---===---===---===---===---===--- */'
\}

"}}}
"--- vimconsole.vim ---"{{{

" auto output log to debug console
let g:vimconsole#auto_redraw = 1

"}}}
"--- vim-go ---"{{{

" Disable defaults
"let g:go_fmt_autosave        = 0
"let g:go_def_mapping_enabled = 0

"}}}
"--- vim-textobj-function ---"{{{

let g:textobj_function_no_default_key_mappings = 1

"}}}
"--- vim-textobj-indent ---"{{{

let g:textobj_indent_no_default_key_mappings = 1

"}}}
"--- vim-easymotion ---"{{{

" View hitchars on current column line
let g:EasyMotion_startofline = 0

"}}}
"--- For Debug ---"{{{

" Local my plugins
let s:repos = [ 'arot13.vim'
\             , 'ahoge-put.vim'
\             , 'asql.vim'
\             , 'adrone.vim'
\             , 'aho-bakaup.vim'
\             , 'separetaro.vim'
\             ]

let s:repos_dir = '~/Repository/'


" If valid local plugin, disable bundled same plugin
for s:plug in s:repos
	let s:plug_dir = s:repos_dir . s:plug

	if isdirectory(expand(s:plug_dir))
		execute ':set runtimepath+=' . s:plug_dir
		execute ':NeoBundleDisable'    s:plug
	end
endfor


unlet s:plug_dir s:plug s:repos_dir s:repos

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
set number nowrap hlsearch list scrolloff=16

" Status Bar always displayed
set laststatus=2

"@See('http://sourceforge.jp/magazine/07/11/06/0151231')
" Status Bar format
set statusline=%F%m\%=\ \ [FileType=%y][Format=%{&fileencoding}][Encode=%{&encoding}][%4l,%4v]

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
	autocmd ColorScheme       * highlight RcEmSpace ctermbg=LightBlue
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

" View more info on <C-g>
set noruler

"@See('http://d.hatena.ne.jp/thinca/20111204/1322932585')
" Sugoi view tabline
function! s:tabpage_label(n) "{{{
	let l:title = gettabvar(a:n, 'title')
	if l:title !=# ''
		return l:title
	endif

	let l:bufnrs = tabpagebuflist(a:n)
	let l:hi = a:n is tabpagenr() ? '%#TabLineSel#' : '%#TabLine#'

	let l:no = len(l:bufnrs)
	if l:no is 1
		let l:no = ''
	endif

	let l:mod = len(filter(copy(l:bufnrs), 'getbufvar(v:val, "&modified")')) ? '+' : ''
	let l:sp = (l:no . l:mod) ==# '' ? '' : ' '

	let l:curbufnr = bufnrs[tabpagewinnr(a:n) - 1]
	let l:fname = pathshorten(bufname(l:curbufnr))
	if l:fname ==# ''
		let l:fname = '[ NoName ]'
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

" Don't jump bottom to top and top to bottom when searching
set nowrapscan

" Fold Text with foldmarker and fold sets
set foldmethod=marker
set foldtext=FoldCCtext()
set foldcolumn=1
let &fillchars = 'vert:|,fold: '
set foldopen=search,jump,mark,percent,insert,tag,undo
set foldclose=all

" Collection Swap File
let &directory = s:directory

" Save View Position when execute ':mkview'
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

" Do foldopen all when visual_mode cursor_move
" s:visual_fold_all() "{{{

let s:visual_fold_toggle = get(s:, 'visual_fold_toggle', 0)

function! s:visual_fold_all()
	if mode() =~# "^[vV\<C-v>]"
		if !s:visual_fold_toggle && &foldenable
			set nofoldenable
			normal! zz
			let s:visual_fold_toggle = 1
		endif
	else
		if s:visual_fold_toggle
			set foldenable
			normal! zz
			let s:visual_fold_toggle = 0
		endif
	endif
endfunction

"}}}
autocmd KeyEvent CursorMoved * call s:visual_fold_all()

" no put two space on join (J)
set nojoinspaces

"}}}


"-------------------------"
"     Inner_Setting       "
"-------------------------"
"{{{

" Auto Judge file encode
set fileencodings=ucs-bom,utf-8,sjis,euc-jp,cp932,iso-2022-jp-3,iso-2022-jp,eucjp-ms,euc-jisx0213,ucs-bom,latin1,default

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
set spelllang=en_US,cjk

" Set reference path, using by :find, gf and more
set path=.,,./**

" Manually generate my help tags
if isdirectory(s:vim_home . '/doc')
	execute 'helptags' (s:vim_home . '/doc')
endif

"}}}


"-------------------------"
"      Event_Method       "
"-------------------------"
"{{{

" Save Cursor Position when file closed
function! s:visit_past_position() "{{{
		let l:past_posit = line("'\"")

		if l:past_posit > 0 && l:past_posit <= line('$')
			execute 'normal! g`"'
		endif
	endfunction "}}}

augroup FileEvent
	autocmd BufReadPost * call s:visit_past_position()

	" Auto load filetype dictionary
	autocmd FileType *
	\	if filereadable(printf('%s/dict/filetype/%s.dict', s:vim_home, &filetype))
	\|		execute 'setl dict+=' . printf('%s/dict/filetype/%s.dict', s:vim_home, &filetype)
	\|	endif
augroup END


" If you using windows cmd prompt, listchars using safe chars
autocmd FileEvent VimEnter,WinEnter,BufWinEnter,BufRead,EncodingChanged *
\	if s:is_doswin
\|		let &listchars = 'tab:>_,trail:_,extends:>,precedes:<,nbsp:%'
\|	else
\|		let &listchars = 'tab:»_,trail:_,extends:»,precedes:«,nbsp:%,eol:↲'
\|	endif

"" Foooooo!!!!!!! I hope get this omoshiro event!!
"autocmd KeyEvent UserGettingBored * echo 'oooooiiiii!!!!!'

"}}}


"-------------------------"
"      Command_Util       "
"-------------------------"
" For Override {{{

call altercmd#load()

" buffer open commands with filetype 'none'
command! -bar -bang NewOverridden new<bang> | setf none
AlterCommand new NewOverridden

command! -bar -bang -complete=file -nargs=? VnewOverridden vnew<bang> <args> | if empty(&ft) | setf none | endif
AlterCommand vnew VnewOverridden

command! -bar -bang -complete=file -nargs=? EnewOverridden enew<bang> <args> | if empty(&ft) | setf none | endif
AlterCommand enew EnewOverridden

command! -bar -bang -complete=file -nargs=? TabnewOverridden tabnew<bang> <args> | if empty(&ft) | setf none | endif
AlterCommand tabnew TabnewOverridden

" }}}
" Our Usefull {{{

" Grep and Open current buffer
command! -bar -nargs=1 GrepNow vimgrep <args> % | cwindow


" Save a Temporary Directory
" {{{

autocmd FileEvent BufNew * let b:tdir_dir = get(b:, 'tdir_dir', 'Not set tdir')

command! -bar TDirPwd           echo b:tdir_dir

function! s:set_temporary_dir(path) "{{{
	if isdirectory(a:path)
		let b:tdir_dir = a:path ==# '.' ? expand('%:p:h')
		\                               : a:path
		echo b:tdir_dir
	else
		call s:echo_error('No such temporary root dir')
	endif
endfunction "}}}
command! -bar -nargs=1  TDirSet call s:set_temporary_dir(<q-args>)

command! -bar TDirSetCurrentDir call s:set_temporary_dir('.')

function! s:cd_temporary_dir() "{{{
	if b:tdir_dir ==# 'Not set tdir'
		call s:echo_error('Not set temporary root dir')
	else
		execute 'cd' b:tdir_dir
		echo b:tdir_dir
	endif
endfunction "}}}
command! -bar TDirCd            call s:cd_temporary_dir()

" }}}


" Reverse ranged lines
function! s:reverse_line() range " {{{
	if a:firstline is a:lastline
		return
	endif

	let l:lines = []
	let l:posit = getpos('.')

	let l:z = @z
	for l:line in range(a:firstline, a:lastline)
		execute 'normal! "zdd'
		call add(l:lines, @z)
	endfor

	for l:r in l:lines
		let @z = l:r
		execute 'normal! "zP'
	endfor
	let @z = l:z

	call setpos('.', l:posit)
endfunction " }}}
command! -range=% ReverseLine :<line1>, <line2>call s:reverse_line()


" Rename current buffer file
function! s:rename_to(to_file) abort "{{{
	let l:this_file    = fnameescape(expand('%:t'))
	let l:to_file      = fnameescape(a:to_file)

	if fnamemodify(l:this_file, '%h') ==# fnamemodify(l:to_file, '%h')
		call s:echo_error('New name is same old name, operation abort')
		return
	endif

	let l:editing_file = &modified
	if l:editing_file
		call s:echo_error('Please :write this file')
		return
	endif

	let l:failed = rename(l:this_file, l:to_file)
	if l:failed
		call s:echo_error(printf('Rename %s to %s is failed', l:this_file, a:to_file))
		return
	endif

	execute ':edit' l:to_file
	silent write
	silent execute ':bdelete' fnamemodify(l:this_file, '%h')

	echo printf('Renamed %s to %s', l:this_file, l:to_file)
endfunction "}}}
command! -bar -nargs=1 -complete=file Rename call s:rename_to(<q-args>)

"}}}
" Life Helper {{{

" Vim Utils {{{

command! -bar VimConfig e $MYVIMRC

command! -bar VimConfigTab tabnew $MYVIMRC

command! -bar Reload so $MYVIMRC
	\|	if has('gui_running') && filereadable($MYGVIMRC)
	\|		so $MYGVIMRC
	\|	endif

cnoreabbr w!! w !sudo tee % > /dev/null

cnoreabbr CdBufDir cd %:p:h
command!  CdBufDir NOP

command! -bar ColorPreview Unite colorscheme -auto-preview

command! -bar -nargs=? -complete=filetype FtpluginEditAfter
\	execute ':edit' printf('%s/after/ftplugin/%s.vim', s:vim_home, (empty(<q-args>) ? &filetype : <q-args>))

command! -bar -nargs=? -complete=filetype FtDictionaryEdit
\	execute ':edit' printf('%s/dict/filetype/%s.dict', s:vim_home, (empty(<q-args>) ? &filetype : <q-args>))

" }}}
" Twitter {{{

"Note: if ask more, add hooks
"    (let g:vimrc.private['twitter']['curr_ac'] = ~~)
"    on tweetvim's TweetVimSwitchAccount command.

"-- Basic --"
command! -bar Twitter            TweetVimHomeTimeline
command! -bar TwitterTab         tabnew | Twitter
command! -bar Tweet              TweetVimSay


"-- Private Account --"
function! TwitterPrivateFunc() "{{{
	if !exists('g:vimrc.private["twitter"]["priv_ac"]')
		call s:echo_error('Not set env variable => g:vimrc.private["twitter"]["priv_ac"]')
		return
	endif

	execute ':TweetVimSwitchAccount' g:vimrc.private['twitter']['priv_ac']
	let g:vimrc.private['twitter']['curr_ac'] = g:vimrc.private['twitter']['priv_ac']

	TweetVimHomeTimeline
endfunction "}}}
command! -bar TwitterPrivate     call TwitterPrivateFunc()
command! -bar TwitterPrivateTab  tabnew | TwitterPrivate
function! TweetPrivateFunc() "{{{
	if !exists('g:vimrc.private["twitter"]["priv_ac"]')
		call s:echo_error('Not set env variable => g:vimrc.private["twitter"]["priv_ac"]')
		return
	endif

	execute ':TweetVimSwitchAccount' g:vimrc.private['twitter']['priv_ac']
	TweetVimSay

	"@Incomplete('wait sync here')
	"execute ':TweetVimSwitchAccount' g:vimrc.private['twitter']['curr_ac']
endfunction "}}}
command! -bar TweetPrivate       call TweetPrivateFunc()


"-- Public Account --"
function! TwitterPublicFunc() "{{{
	if !exists("g:vimrc.private['twitter']['publ_ac']")
		call s:echo_error("Not set env variable => g:vimrc.private['twitter']['publ_ac']")
		return
	endif

	execute ':TweetVimSwitchAccount ' g:vimrc.private['twitter']['publ_ac']
	let g:vimrc.private['twitter']['curr_ac'] = g:vimrc.private['twitter']['publ_ac']

	TweetVimHomeTimeline
endfunction "}}}
command! -bar TwitterPublic      call TwitterPublicFunc()
command! -bar TwitterPublicTab   tabnew | TwitterPublic
function! TweetPublicFunc() "{{{
	if !exists('g:vimrc.private["twitter"]["publ_ac"]')
		call s:echo_error('Not set env variable => g:vimrc.private["twitter"]["publ_ac"]')
		return
	endif

	execute ':TweetVimSwitchAccount ' g:vimrc.private['twitter']['publ_ac']
	TweetVimSay

	"@Incomplete('wait here')
	"execute ':TweetVimSwitchAccount ' g:vimrc.private['twitter']['curr_ac']
endfunction "}}}
command! -bar TweetPublic        call TweetPublicFunc()


command! -bar Bitly TweetVimBitly
cnoreabbr tvs TweetVimSwitchAccount

" }}}


" To Service Name
cnoreabbr Lingr J6uil
command!  Lingr NOP


" Beautifull Life
cnoreabbr JazzList Unite jazzradio
command!  JazzList NOP


" Translates Languages
cnoreabbr Translate ExciteTranslate
command!  Translate NOP

cnoreabbr weblio    Ref webdict weblio
cnoreabbr Weblio    Ref webdict weblio
command!  Weblio    NOP


" NeoBundle redirect alias
cnoreabbr NeoBundleInstallsLog NeoBundleLog
command!  NeoBundleInstallsLog NOP


" Markdown help on online (maru nage)
command! -bar MarkdownHelpOnline W3mTab http://qiita.com/Qiita/items/c686397e4a0f4f11683d

" }}}
" Development {{{


" VimShellInteractive executors
" Open Develop Buffer {{{

" vimconsole.vim
cnoreabbr Log      VimConsoleLog
cnoreabbr LogClear VimConsoleClear
cnoreabbr LogOpen  VimConsoleOpen

command!  Log      NOP
command!  LogClear NOP
command!  LogOpen  NOP


" GHCi
cnoreabbr Ghci     VimShellInteractive ghci
cnoreabbr Sghci    VimShellInteractive --split='sp' ghci
cnoreabbr Vghci    VimShellInteractive --split='vsp' ghci
cnoreabbr GhciTab  VimShellInteractive --split='tabnew' ghci
cnoreabbr Hoogle   Ref hoogle

command!  Ghci     NOP
command!  Sghci    NOP
command!  Vghci    NOP
command!  GhciTab  NOP
command!  Hoogle   NOP


" js
cnoreabbr Js       VimShellInteractive js
cnoreabbr Sjs      VimShellInteractive --split='sp' js
cnoreabbr Vjs      VimShellInteractive --split='vsp' js
cnoreabbr JsTab    VimShellInteractive --split='tabnew' js

command!  Js       NOP
command!  Sjs      NOP
command!  Vjs      NOP
command!  JsTab    NOP


" irb
cnoreabbr Irb      VimShellInteractive irb
cnoreabbr Sirb     VimShellInteractive --split='sp' irb
cnoreabbr Virb     VimShellInteractive --split='vsp' irb
cnoreabbr IrbTab   VimShellInteractive --split='tabnew' irb

command!  Irb      NOP
command!  Sirb     NOP
command!  Virb     NOP
command!  IrbTab   NOP

" }}}


" View highlight statuses
" command! HighlightListTab {{{

function! s:highlight_list_tab()
	" open new tab that doesn't need :write
	tabnew
	setl buftype=nofile

	" read highlight list to z register
	let l:z = @z
	redir @z
		" execute no wait key
		silent! highlight
	redir END

	" read detail apply to buffer
	execute 'normal! "zp'
	execute 'normal! "_dd'

	" Restore past z register
	let @z = l:z
endfunction

command! -bar HighlightListTab call s:highlight_list_tab()

" }}}


" Staging current file to git
command! -bar GitAdd !git add %


" }}}


"-------------------------"
"       Key_Mapping       "
"-------------------------"
" Disable keys {{{

augroup KeyMapping
	" I can use some mapping to hoge<C-c>
	autocmd User MyVimRc nnoremap <C-c>      <NOP>
	autocmd User MyVimRc nnoremap <C-c><C-c> <C-c>

	autocmd User MyVimRc nnoremap <Up>    <NOP>
	autocmd User MyVimRc nnoremap <Down>  <NOP>
	autocmd User MyVimRc nnoremap <Left>  <NOP>
	autocmd User MyVimRc nnoremap <Right> <NOP>

	autocmd User MyVimRc inoremap <Up>    <NOP>
	autocmd User MyVimRc inoremap <Down>  <NOP>
	autocmd User MyVimRc inoremap <Left>  <NOP>
	autocmd User MyVimRc inoremap <Right> <NOP>

	autocmd User MyVimRc cnoremap <Left>  <NOP>
	autocmd User MyVimRc cnoremap <Right> <NOP>

	autocmd User MyVimRc cnoremap [Left] <Left>
augroup END

" }}}
" Global keyMaps {{{

" Prepare functions {{{


" Compress continuous space
function! s:compress_spaces() "{{{
	let l:recent_pattern = @/

	try
		substitute/\s\+/ /g
		normal! ==
	finally
		let @/ = l:recent_pattern
	endtry

	nohlsearch
endfunction "}}}


" Clear all lines end space
function! s:clear_ends_space() "{{{
	let l:recent_pattern = @/
	let l:curpos = getcurpos()

	try
		%substitute/\s*\?$//g
	catch /E486/
		echo 'nothing todo'
	finally
		let @/ = l:recent_pattern

		call setpos('.', l:curpos)
	endtry
endfunction "}}}


" Move cursor to topmost of this indent
function! s:cursor_up_to_lid() "{{{
	let l:first_line = 1

	while 1
		let l:p = getcurpos()[2]
		normal! k

		let l:indent_changed = l:p isnot getcurpos()[2]

		if l:indent_changed || line('.') is l:first_line
			if l:indent_changed
				normal! j
			endif

			break
		endif
	endwhile
endfunction "}}}


" Move cursor to bottommost of this indent
function! s:cursor_down_to_ground() "{{{
	let l:last_line = line('$')

	while 1
		let l:p = getcurpos()[2]
		execute 'normal! j'

		let l:indent_changed = l:p isnot getcurpos()[2]

		if l:indent_changed || line('.') is l:last_line
			if l:indent_changed
				execute 'normal! k'
			endif

			break
		endif
	endwhile
endfunction "}}}


" Optimize key operation to one hand
" function! s:toggle_onehand_mode() "{{{

let s:onehand_enabled = get(s:, 'onehand_enabled', 0)

function! s:toggle_onehand_mode()
	if s:onehand_enabled
		nunmap n
		nunmap p
		nunmap f
		nunmap b
		nunmap o
		nunmap i
		nunmap u
		nunmap /

		" restore normal keymapping
		doautocmd User MyVimRc
	else
		nnoremap n gt
		nnoremap p gT
		nnoremap f <C-f>
		nnoremap b <C-b>
		nnoremap o <C-o>
		nnoremap i <C-i>
		nnoremap u <C-w><C-w>
		nnoremap / *
	endif

	let s:onehand_enabled = !s:onehand_enabled
	echo (s:onehand_enabled ? '' : 'no') . 'onehand'
endfunction

"}}}


" If you has nofile buffer, close it.
function! s:bufclose_filetype(filetype) "{{{
	let l:closed = 0

	for l:w in range(1, winnr('$'))
		let l:buf_ft = getwinvar(l:w, '&filetype')

		if l:buf_ft ==# a:filetype
			execute ':' . l:w . 'wincmd w'
			execute ':quit'

			let l:closed = 1
		endif
	endfor

	return l:closed
endfunction "}}}


" Toggle open netrw explorer ( vertical split )
function! s:toggle_netrw_vexplorer() "{{{
	let l:closed = s:bufclose_filetype('netrw')

	if !l:closed
		Vexplore
	endif
endfunction "}}}


" }}}
" Foldings {{{

augroup KeyMapping
	autocmd User MyVimRc nnoremap <expr> h foldclosed('.') > -1 ? 'zo' : 'h'
	autocmd User MyVimRc nnoremap <expr> l foldclosed('.') > -1 ? 'zo' : 'l'

	autocmd User MyVimRc nnoremap zj zjzo
	autocmd User MyVimRc nnoremap zk zkzo
	autocmd User MyVimRc nnoremap {  {zv
	autocmd User MyVimRc nnoremap }  }zv
	autocmd User MyVimRc nnoremap (  (zv
	autocmd User MyVimRc nnoremap )  )zv
augroup END

" }}}
" Windows and Buffers {{{

augroup KeyMapping
	autocmd User MyVimRc nnoremap <Space>h <C-w>h
	autocmd User MyVimRc nnoremap <Space>j <C-w>j
	autocmd User MyVimRc nnoremap <Space>k <C-w>k
	autocmd User MyVimRc nnoremap <Space>l <C-w>l

	autocmd User MyVimRc nnoremap <silent> <C-w>t :<C-u>TabnewOverridden<CR>
	autocmd User MyVimRc nnoremap <silent> <C-w>T :<C-u>tabclose<CR>
	autocmd User MyVimRc nnoremap <silent> <C-w>c :<C-u>bdelete<CR>
	autocmd User MyVimRc nnoremap <silent> <C-w>C :<C-u>bdelete!<CR>
	autocmd User MyVimRc nnoremap <silent> <C-w>N :<C-u>EnewOverridden!<CR>
	autocmd User MyVimRc nnoremap <silent> <C-w>Q :<C-u>quitall<CR>
	autocmd User MyVimRc nnoremap <silent> <C-w>" :<C-u>resize 5<CR>

	autocmd User MyVimRc nnoremap <C-w><C-r> <C-w>r<C-w>p

	autocmd User MyVimRc nnoremap <silent><expr> <C-w>bt 'mZ:tabnew<CR>`Zzz'          . (foldlevel('.') > 0 ? 'zo' : '')
	autocmd User MyVimRc nnoremap <silent><expr> <C-w>bT 'mZ:hide<CR>:tabnew<CR>`Zzz' . (foldlevel('.') > 0 ? 'zo' : '')
augroup END

" }}}
" Toggle options {{{

augroup KeyMapping
	autocmd User MyVimRc nnoremap <silent><expr> <C-h><C-d> (&diff ? ':diffoff' : ':diffthis') . '\|set diff?<CR>'
	autocmd User MyVimRc nnoremap <silent><expr> <C-h><C-v> ':setl virtualedit=' . (&virtualedit ==# ''       ? 'all'    : '')       . ' virtualedit?<CR>'
	autocmd User MyVimRc nnoremap <silent><expr> <C-h><C-f> ':setl foldmethod='  . (&foldmethod  ==# 'marker' ? 'syntax' : 'marker') . ' foldmethod?<CR>'

	autocmd User MyVimRc nnoremap <silent> <C-h>jk    :<C-u>call <SID>toggle_onehand_mode()<CR>
	autocmd User MyVimRc nnoremap <silent> <C-h><C-w> :<C-u>setl wrap!           wrap?          <CR>
	autocmd User MyVimRc nnoremap <silent> <C-h><C-c> :<C-u>setl cursorline!     cursorline?    <CR>
	autocmd User MyVimRc nnoremap <silent> <C-h><C-e> :<C-u>setl expandtab!      expandtab?     <CR>
	autocmd User MyVimRc nnoremap <silent> <C-h><C-r> :<C-u>setl relativenumber! relativenumber?<CR>
	autocmd User MyVimRc nnoremap <silent> <C-h><C-l> :<C-u>setl list!           list?          <CR>
	autocmd User MyVimRc nnoremap <silent> <C-h><C-n> :<C-u>setl number!         number?        <CR>
	autocmd User MyVimRc nnoremap <silent> <C-h><C-s> :<C-u>setl wrapscan!       wrapscan?      <CR>

	autocmd User MyVimRc inoremap <silent> <C-k><C-e> <C-o>:setl expandtab! expandtab?<CR>
	autocmd User MyVimRc inoremap <silent> <C-k><C-w> <C-o>:setl wrap!      wrap?<CR>
augroup END

" }}}
" for Plugins {{{

augroup KeyMapping
	" netrw
	autocmd User MyVimRc nnoremap <silent> <leader>e         :<C-u>call <SID>toggle_netrw_vexplorer()<CR>
	autocmd User MyVimRc nnoremap <silent> <leader><leader>e :<C-u>Sexplore<CR>
	autocmd User MyVimRc nnoremap <silent> <leader>E         :<C-u>Explore<CR>
	autocmd User MyVimRc nnoremap <silent> <leader><leader>E :<C-u>Texplore<CR>


	" open-browser.vim
	autocmd User MyVimRc nmap <leader>w <Plug>(openbrowser-open)


	" vim-quickrun
	autocmd User MyVimRc nnoremap <silent> <leader>R         :<C-u>QuickRun -runner shell<CR>
	autocmd User MyVimRc nnoremap <silent> <leader><leader>r :<C-u>call <SID>bufclose_filetype('quickrun')<CR>


	" vimshell
	autocmd User MyVimRc nnoremap <silent> <leader>v         :<C-u>VimShell -split-command=vsp -toggle<CR>
	autocmd User MyVimRc nnoremap <silent> <leader><leader>v :<C-u>VimShell -split-command=sp  -toggle<CR>
	autocmd User MyVimRc nnoremap <silent> <leader>V         :<C-u>VimShellBufferDir -create<CR>
	autocmd User MyVimRc nnoremap <silent> <leader><leader>V :<C-u>VimShell -split-command=tabnew -create<CR>


	" Unite
	autocmd User MyVimRc nnoremap <silent> <C-k><C-h>        :<C-u>Unite -ignorecase neomru/file<CR>
	autocmd User MyVimRc nnoremap <silent> <C-k><C-f>        :<C-u>Unite -ignorecase outline<CR>
	autocmd User MyVimRc nnoremap <silent> <leader><leader>u :<C-u>call <SID>bufclose_filetype('unite')<CR>


	" excitetranslate-vim
	autocmd User MyVimRc nnoremap <silent> <leader>T :ExciteTranslate<CR>


	" vim-over
	autocmd User MyVimRc nnoremap <silent>       :%s/       :<C-u>OverCommandLine %s/<CR>
	autocmd User MyVimRc nnoremap <silent>       :s/        :<C-u>OverCommandLine s/<CR>
	autocmd User MyVimRc nnoremap <silent><expr> <C-k><C-s> ':OverCommandLine %s/\<' . expand('<cword>') . '\>/<CR>'
	autocmd User MyVimRc nnoremap <silent><expr> <C-k>s     ':OverCommandLine %s/\<' . expand('<cword>') . '\>/' . expand('<cword>') . '<CR>'
	autocmd User MyVimRc vnoremap <silent>       :s/        :<C-u>OverCommandLine '<,'>s/<CR>
	autocmd User MyVimRc cnoremap <silent>       <C-k>:     :<C-u>OverCommandLine<CR>
	"@Marked('this is temporary keymapping, because vim-over do not imported cnoremap maybe')
	" please delete this when fixed it
	autocmd FileType * OverCommandLineNoremap <C-b>      <Left>
	autocmd FileType * OverCommandLineNoremap <C-f>      <Right>
	autocmd FileType * OverCommandLineNoremap <C-a>      <Home>
	autocmd FileType * OverCommandLineNoremap <C-h>      <BS>
	autocmd FileType * OverCommandLineNoremap <C-d>      <Del>
	autocmd FileType * OverCommandLineNoremap <C-e>      <End>
	"autocmd FileType * OverCommandLineNoremap <C-k><C-k> <C-\>e getcmdpos() < 2 ? '' : getcmdline()[:getcmdpos()-2]<CR>
	autocmd FileType * OverCommandLineNoremap <C-l>      <Esc>
	autocmd FileType * OverCommandLineNoremap <C-]>      '<,'>


	" anzu-chan
	autocmd User MyVimRc nmap n      <Plug>(anzu-n-with-echo)zv
	autocmd User MyVimRc nmap N      <Plug>(anzu-N-with-echo)zv
	autocmd User MyVimRc nmap *      <Plug>(anzu-star-with-echo)zv
	autocmd User MyVimRc nmap #      <Plug>(anzu-sharp-with-echo)zv
	autocmd User MyVimRc nmap <C-w>* <C-w>v<Plug>(anzu-star-with-echo)zv
	autocmd User MyVimRc nmap <C-w># <C-w>v<Plug>(anzu-sharp-with-echo)zv


	" incsearch.vim
	autocmd User MyVimRc nmap <expr>      /                 foldclosed('.') > -1 ? 'zv<Plug>(incsearch-forward)'  : '<Plug>(incsearch-forward)'
	autocmd User MyVimRc nmap <silent>    <leader>/         /\m\C
	autocmd User MyVimRc nmap <silent>    <leader><leader>/ /\m\C\<\>[Left][Left]
	autocmd User MyVimRc nmap             g/                /\<<C-r>"\><CR>
	autocmd User MyVimRc nmap <expr>      ?                 foldclosed('.') > -1 ? 'zv<Plug>(incsearch-backward)' : '<Plug>(incsearch-backward)'
	autocmd User MyVimRc nmap <silent>    <leader>?         ?\m\C
	autocmd User MyVimRc nmap <silent>    <leader><leader>? ?\m\C\<\>[Left][Left]
	autocmd User MyVimRc nmap             g?                ?\<<C-r>"\><CR>
	autocmd User MyVimRc vmap <expr>      /                 foldclosed('.') > -1 ? 'zv<Plug>(incsearch-forward)'  : '<Plug>(incsearch-forward)'
	autocmd User MyVimRc vmap <expr>      ?                 foldclosed('.') > -1 ? 'zv<Plug>(incsearch-backward)' : '<Plug>(incsearch-backward)'
	"@Marked('Set event FileType *, because avoid error. please suitable event')
	autocmd FileType * IncSearchNoreMap <C-j> <CR>
	autocmd FileType * IncSearchNoreMap <C-l> <Esc>


	" TaskList.vim
	autocmd User MyVimRc nmap <silent> <leader>t <Plug>TaskListToggle


	" undotree
	autocmd User MyVimRc nnoremap <silent> <leader>u :<C-u>UndotreeToggle<CR>


	" neosnippet.vim
	autocmd User MyVimRc imap <expr> <C-s> neosnippet#expandable() ? '<Plug>(neosnippet_expand)' : '<Plug>(neosnippet_jump)'
	autocmd User MyVimRc smap <expr> <C-s> neosnippet#expandable() ? '<Plug>(neosnippet_expand)' : '<Plug>(neosnippet_jump)'


	" separetaro.vim
	autocmd User MyVimRc nmap <leader>ps <Plug>(separetoro_put_short_under)
	autocmd User MyVimRc nmap <leader>Ps <Plug>(separetoro_put_short_over)
	autocmd User MyVimRc nmap <leader>pl <Plug>(separetoro_put_long_under)
	autocmd User MyVimRc nmap <leader>Pl <Plug>(separetoro_put_long_over)

	" vim-easymotion
	autocmd User MyVimRc nmap <leader>j  <Plug>(easymotion-j)
	autocmd User MyVimRc nmap <leader>k  <Plug>(easymotion-k)
	autocmd User MyVimRc nmap <leader>s  <Plug>(easymotion-s)
	autocmd User MyVimRc nmap <leader>S  <Plug>(easymotion-S)
	autocmd User MyVimRc vmap <leader>j  <Plug>(easymotion-j)
	autocmd User MyVimRc vmap <leader>k  <Plug>(easymotion-k)
	autocmd User MyVimRc vmap <leader>s  <Plug>(easymotion-s)
	autocmd User MyVimRc vmap <leader>S  <Plug>(easymotion-S)
augroup END

" }}}
" Others {{{

augroup KeyMapping
	" normal mode "{{{

	autocmd User MyVimRc nmap <C-j> <CR>

	" † Ex Improved
	autocmd User MyVimRc nnoremap Q  gQ

	autocmd User MyVimRc nnoremap <C-n> gt
	autocmd User MyVimRc nnoremap <C-p> gT
	autocmd User MyVimRc nnoremap <C-m> o<Esc>

	autocmd User MyVimRc nnoremap <silent> m: :<C-u>marks<CR>
	autocmd User MyVimRc nnoremap <silent> q: :<C-u>register<CR>
	autocmd User MyVimRc nnoremap <silent> g: :<C-u>tabs<CR>
	autocmd User MyVimRc nnoremap <silent> z: :<C-u>buffers<CR>
	autocmd User MyVimRc nnoremap <silent> g> :<C-u>messages<CR>
	autocmd User MyVimRc nnoremap <silent> g* :<C-u>execute 'silent! normal! *<C-o>'<CR>
	autocmd User MyVimRc nnoremap <silent> <Space><Space>   :<C-u>call <SID>compress_spaces()<CR>
	autocmd User MyVimRc nnoremap <silent> <Space><S-Space> :<C-u>call <SID>clear_ends_space()<CR>

	autocmd User MyVimRc nnoremap <silent> <leader>b                :<C-u>NewOverridden \| resize 5 \| setl buftype=nofile \| setl filetype=scratch<CR>
	autocmd User MyVimRc nnoremap <silent> <leader>B                :<C-u>NewOverridden \| resize 5<CR>
	autocmd User MyVimRc nnoremap <silent> <leader><leader>b        :<C-u>call <SID>bufclose_filetype('scratch')<CR>
	autocmd User MyVimRc nnoremap <silent> <leader><leader>h        :<C-u>helpclose<CR>
	autocmd User MyVimRc nnoremap <silent> <leader><leader>k        :<C-u>call <SID>cursor_up_to_lid()<CR>
	autocmd User MyVimRc nnoremap <silent> <leader><leader>j        :<C-u>call <SID>cursor_down_to_ground()<CR>
	autocmd User MyVimRc nnoremap <silent> <leader><leader><leader> :<C-u>echohl ErrorMsg \| echo "Don't rush it, keep cool." \| echohl None<CR>

	autocmd User MyVimRc nnoremap <silent> <C-k><C-r> :<C-u>Reload<CR>
	autocmd User MyVimRc nnoremap <silent> <C-k><C-l> :<C-u>nohlsearch<CR>
	autocmd User MyVimRc nnoremap <silent> <C-k><C-j> :<C-u>write<CR>
	autocmd User MyVimRc nnoremap <silent> <C-k>J     :<C-u>wall \| echo 'written all !'<CR>
	autocmd User MyVimRc nnoremap <silent> <C-k>r     :<C-u>let &filetype = &filetype<CR>
	autocmd User MyVimRc nnoremap <silent> <C-k>R     :<C-u>doautocmd User MyVimRc<CR>

	"}}}
	" insert mode "{{{

	autocmd User MyVimRc imap <C-j> <CR>

	autocmd User MyVimRc inoremap <C-l> <Esc>
	autocmd User MyVimRc inoremap <C-k><C-k> <C-o>"_d$
	autocmd User MyVimRc inoremap <C-k><C-y> <Esc>k"zyyjV"zp:let @z = ''<CR>A

	autocmd User MyVimRc inoremap <silent> <C-k><C-j> <Esc>:write<CR>
	autocmd User MyVimRc inoremap <silent> <C-k>J     <Esc>:wall \| echo 'written all !'<CR>

	"}}}
	" command line mode "{{{

	autocmd User MyVimRc cnoremap <C-b>      <Left>
	autocmd User MyVimRc cnoremap <C-f>      <Right>
	autocmd User MyVimRc cnoremap <C-a>      <Home>
	autocmd User MyVimRc cnoremap <C-h>      <BS>
	autocmd User MyVimRc cnoremap <C-d>      <Del>
	autocmd User MyVimRc cnoremap <C-e>      <End>
	autocmd User MyVimRc cnoremap <C-k><C-k> <C-\>e getcmdpos() < 2 ? '' : getcmdline()[ : getcmdpos() - 2]<CR>
	autocmd User MyVimRc cnoremap <C-l>      <Esc>
	autocmd User MyVimRc cnoremap <C-]>      '<,'>
	autocmd User MyVimRc cnoremap <C-k><C-p> <Up>
	autocmd User MyVimRc cnoremap <C-k><C-n> <Down>

	"}}}
	" visual mode "{{{

	autocmd User MyVimRc vnoremap <C-l> <Esc>
	"autocmd User MyVimRc vnoremap <silent> <leader>k :<C-u>call <SID>cursor_up_to_lid()<CR>
	"autocmd User MyVimRc vnoremap <silent> <leader>j :<C-u>call <SID>cursor_down_to_ground()<CR>
	autocmd User MyVimRc vnoremap <silent> i= :Align =<CR>
	autocmd User MyVimRc vnoremap a% V%
	autocmd User MyVimRc vnoremap A% V$%

	" Don't select blank
	autocmd User MyVimRc vnoremap a" 2i"
	autocmd User MyVimRc vnoremap a' 2i'
	autocmd User MyVimRc vnoremap a` 2i`

	" textobj-function
	autocmd User MyVimRc vmap af <Plug>(textobj-function-a)
	autocmd User MyVimRc vmap if <Plug>(textobj-function-i)

	" textobj-indent
	autocmd User MyVimRc vmap ai <Plug>(textobj-indent-a)
	autocmd User MyVimRc vmap ii <Plug>(textobj-indent-i)

	"}}}
	" select mode "{{{

	autocmd User MyVimRc snoremap <C-l> <Esc>

	"}}}
	" operator "{{{

	autocmd User MyVimRc onoremap a% V%
	autocmd User MyVimRc onoremap A% V$%

	" Don't select blank
	autocmd User MyVimRc onoremap a" 2i"
	autocmd User MyVimRc onoremap a' 2i'
	autocmd User MyVimRc onoremap a` 2i`

	" textobj-function
	autocmd User MyVimRc omap af <Plug>(textobj-function-a)
	autocmd User MyVimRc omap if <Plug>(textobj-function-i)

	" textobj-indent
	autocmd User MyVimRc omap ai <Plug>(textobj-indent-a)
	autocmd User MyVimRc omap ii <Plug>(textobj-indent-i)

	"}}}
	" digraph "{{{

	digraph %% 8984
	digraph 8: 9731

	"}}}
augroup END

" }}}

" }}}
" Buffer local keyMaps {{{

augroup PluginPrefs
	autocmd FileType int-* nnoremap <buffer> q          <NOP>
	autocmd FileType int-* nnoremap <buffer> <C-n>      gt
	autocmd FileType int-* nnoremap <buffer> <C-p>      gT
	autocmd FileType int-* nnoremap <buffer> <C-l>      <NOP>

	autocmd FileType int-* nmap     <buffer> <C-]>      <Plug>(vimshell_int_clear)
	autocmd FileType int-* nmap     <buffer> Q          <Plug>(vimshell_int_exit)
	autocmd FileType int-* nmap     <buffer> gj         <Plug>(vimshell_int_next_prompt)
	autocmd FileType int-* nmap     <buffer> gk         <Plug>(vimshell_int_previous_prompt)

	autocmd FileType int-* inoremap <buffer> <C-l>      <Esc>
	autocmd FileType int-* inoremap <buffer> <C-b>      <Left>
	autocmd FileType int-* inoremap <buffer> <C-f>      <Right>
	autocmd FileType int-* inoremap <buffer> <C-e>      <End>
	autocmd FileType int-* inoremap <buffer> <C-d>      <Del>

	autocmd FileType int-* imap     <buffer> <C-n>      <C-o><Plug>(vimshell_int_next_prompt)<End>
	autocmd FileType int-* imap     <buffer> <C-p>      <C-o><Plug>(vimshell_int_previous_prompt)<End>
	autocmd FileType int-* imap     <buffer> <C-]>      <C-o><Plug>(vimshell_int_clear)
	autocmd FileType int-* imap     <buffer> <CR>       <Plug>(vimshell_int_execute_line)
	autocmd FileType int-* imap     <buffer> <C-k><C-p> <Plug>(vimshell_int_history_unite)


	autocmd FileType ref-* nnoremap <silent><buffer> Q :<C-u>quit<CR>
augroup END

" }}}


"-------------------------"
"        File_Type        "
"-------------------------"
"{{{

" If buffer does not has filetype, set filetype 'none'
autocmd ExtensionType VimEnter,BufNew * if &ft ==# '' | setf none | endif

"}}}


"-------------------------"
"    Environment_Pref     "
"-------------------------"
"{{{

if filereadable(s:vimrc_env)
	source ~/.vimrc_env
endif

"}}}


filetype plugin indent on
syntax enable
doautocmd User MyVimRc
let g:vimrc['loaded'] = 1
