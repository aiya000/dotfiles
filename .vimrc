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

"-- Unite outline -> view C-Sharp <summary>~</summary>

" }}}
" Issues {{{

"-- C-o hard use on vimshell

"-- automatic mkdir './C:' when execute NeoBundleInstall in windows kaoriya
"  -- does neobundle think that is repository...?

"-- conceal-javadoc don't functioned ?

"-- shot-f don't functioned in i_<C-o> temporary normal mode

"-- I couldn't auto make vimproc at anywhere

"-- conflicted? vim-ruby and rspec.vim when those was set NeoBundleLazy
"  -- does not loaded syntax of rspec.vim

"-- incsearch.vim(?) throw an exception E874 when searched '<leader>~'
"  -- on windows only ?

"-- happened exception when input '.*' to unite textarea

"}}}
" Todo {{{

"-- Eigo translate to English in this file

"-- read help options.jax

"-- read help windows.txt

"-- read help 'cino'

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
let s:groupname = $GROUP !=# '' ? $GROUP : $USER

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
" file encoding {{{

" Default file encoding
set fileencoding=utf-8 encoding=utf-8

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
	let l:dirs = map(split(glob('~/vimfiles/bundle/*'), '\n'), 'fnamemodify(v:val, ":t")')

	for l:dir in l:dirs
		let l:plugin_dir = s:bundledir . '/' . l:dir
		let l:is_empty   = s:system('ls ' . l:plugin_dir) ==# ''

		if l:is_empty
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

		throw 'neobundle.vim clone failed.'
	endif
endfunction " }}}

if has('vim_starting')
	try
		let &runtimepath = &runtimepath . ',' . s:vim_home . '/bundle/neobundle.vim'

		" Throws Error when nothing neobundle in runtime path
		call neobundle#begin()
	catch
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
NeoBundleLazy    'rbtnn/puyo.vim'
NeoBundleLazy    'mattn/benchvimrc-vim'
NeoBundleLazy    'mattn/yamada-vim'
NeoBundleLazy    'jvoorhis/coq.vim'
NeoBundleLazy    'eagletmt/coqtop-vim'
NeoBundle        'rhysd/vim-grammarous'
NeoBundleLazy    'thinca/vim-themis'
NeoBundle        'tomasr/molokai'
NeoBundle        'aiya000/arot13.vim'
NeoBundle        'aiya000/ahoge-put.vim'
NeoBundleLazy    'kannokanno/previm'
NeoBundle        'kamichidu/vim-vdbc'
NeoBundle        'mattn/vdbi-vim'
NeoBundle        'LeafCage/foldCC'
NeoBundleLazy    'katono/rogue.vim'
NeoBundle        'aiya000/asql.vim'
NeoBundleLazy    'kamichidu/vim-benchmark'
NeoBundle        'kana/vim-submode'
NeoBundle        'mfumi/ref-dicts-en'
NeoBundle        'thinca/vim-painter'
NeoBundle        'osyo-manga/vim-anzu'
NeoBundle        'osyo-manga/vim-over'
NeoBundle        'tyru/restart.vim'
NeoBundle        'vim-jp/vimdoc-ja'
NeoBundleLazy    'rbtnn/game_engine.vim'
NeoBundle        'h1mesuke/vim-alignta'
NeoBundle        'haya14busa/incsearch.vim'
NeoBundle        'thinca/vim-scouter'
NeoBundle        'deris/vim-shot-f'
NeoBundle        'oplatek/Conque-Shell'
NeoBundle        'sgelb/TaskList.vim'
NeoBundle        'tyru/vim-altercmd'
NeoBundle        'mbbill/undotree'
NeoBundle        'Shougo/neomru.vim'
NeoBundle        'aiya000/adrone.vim'
NeoBundleFetch   'Shougo/fakecygpty'
NeoBundle        'nathanaelkane/vim-indent-guides'
NeoBundleLazy    'LeafCage/vimhelpgenerator'
NeoBundleLazy    'thinca/vim-threes'
NeoBundle        'vim-ruby/vim-ruby'
NeoBundle        'Keithbsmiley/rspec.vim'
NeoBundle        'tsukkee/unite-help'
NeoBundle        'altercation/vim-colors-solarized'
NeoBundle        'aiya000/aho-bakaup.vim'
NeoBundleLazy    'yaasita/ore_markdown'
NeoBundle        'chrisbra/vim-diff-enhanced'
NeoBundle        'Shougo/neosnippet.vim'
NeoBundle        'Shougo/neosnippet-snippets'


call neobundle#end()

"}}}
"*** Plugin Depends and Auto Config ***" {{{

if neobundle#tap('vimproc.vim')
	call neobundle#config('vimproc.vim', {
	\	'build' : {
	\		'unix'    : 'make -f make_unix.mak',
	\		'mac'     : 'make -f make_mac.mak',
	\		'cygwin'  : 'make -f make_cygwin.mak',
	\		'windows' : 'make -f make_mingw32.mak'
	\	}
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
	\	'depends' : 'Shougo/vimproc.vim'
	\})
	call neobundle#untap()
endif
if neobundle#tap('gmail.vim')
	call neobundle#config('gmail.vim', {
	\	'depends'  : 'Shougo/vimproc.vim',
	\	'autoload' : {'commands' : 'Gmail'}
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
if neobundle#tap('vimshell-kawaii.vim')
	call neobundle#config('vimshell-kawaii.vim', {
	\	'depends'  : 'Shougo/vimshell.vim'
	\})
	call neobundle#untap()
endif
if neobundle#tap('vimconsole.vim')
	call neobundle#config('vimconsole.vim', {
	\	'autoload' : {'filetypes' : 'vim'}
	\})
	call neobundle#untap()
endif
if neobundle#tap('vim-splash')
	call neobundle#config('vim-splash', {
	\	'autoload' : {'commands' : 'Splash'}
	\})
	call neobundle#untap()
endif
if neobundle#tap('jazzradio.vim')
	call neobundle#config('jazzradio.vim', {
	\	'autoload' : {
	\		'unite_sources' : ['jazzradio'],
	\		'commands'      : [
	\			'JazzradioUpdateChannels',
	\			'JazzradioStop', {
	\				'name'     : 'JazzradioPlay',
	\				'complete' : 'customlist,jazzradio#channel_id_comlete'
	\			}
	\		],
	\		'function_prefix' : 'Jazzradio',
	\		'depends'         : 'Shougo/unite.vim'
	\	}
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
	\	'autoload' : {'commands' : 'Puyo'},
	\	'depends'  : 'rbtnn/game_engine.vim'
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
	\	'autoload' : {'filetypes' : 'coq'},
	\	'depends'  : 'Shougo/vimproc.vim'
	\})
	call neobundle#untap()
endif
if neobundle#tap('vim-grammarous')
	call neobundle#config('vim-grammarous', {
	\	'disabled' : !executable('java')
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
	\	'autoload' : {'filetypes' : 'markdown'}
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
if neobundle#tap('fakecygpty')
	call neobundle#config('fakecygpty', {
	\	'build' : {
	\		'windows' : expand('gcc fakecygpty.c -o ~/bin/fakecygpty.exe')
	\	}
	\})
	call neobundle#untap()
endif
if neobundle#tap('vimhelpgenerator')
	call neobundle#config('vimhelpgenerator', {
	\	'autoload' : {'commands' : [
	\		'VimHelpGenerator',
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
if neobundle#tap('ore_markdown')
	"@Bugs('Do not functioned on windows with this config')
	call neobundle#config('ore_markdown', {
	\	'buld' : {
	\		'unix'    : 'bundle install --gemfile ./bin/Gemfile',
	\		'windows' : 'bundle install --gemfile .\bin\Gemfile',
	\		'mac'     : 'bundle install --gemfile ./bin/Gemfile'
	\	},
	\	'autoload' : {'commands' : 'OreMarkdown'}
	\})
	call neobundle#untap()
endif

" }}}


"------------------------"
"*** Plugin_Configure ***"
"------------------------"
"--- netrw ---" {{{

let g:netrw_preview = 1

" }}}
"--- matchit.vim ---" {{{

"" uooooooooooooo... oh, my triple operator !!!!!!!!!!!
"" Why if set you, happend an error when doing match it...
"autocmd PluginPrefs User * let b:match_words = &matchpairs . ',?::'

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
\	},
\	'vimspec' : {
\		'command' : 'themis',
\		'cmdopt'  : '--runtimepath ".."',
\		'exec'    : '%c %o %s:p | tr -d "\r"'
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

" }}}
"--- vimproc.vim ---"{{{

" If you use windows kaoriya vim and does not have mingw
" you use kaoriya vimproc ...maybe
if s:is_kaoriya && s:is_windows && !s:has_mingw
	set runtimepath-=~/vimfiles/bundle/vimproc.vim/
endif

" }}}
"--- TweetVim ---"{{{

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

" This variable defined by my command
" Connect to bash's hereis
let g:vimshell_hereis_file = expand('~/.bashrc_places')

"}}}
"--- vimshell-kawaii.vim ---"{{{

let g:vimshell_kawaii_smiley = 1

"}}}
"--- excitetraslate-vim ---"{{{

" Don't yank result to @" register
let g:excitetranslate_options = ["buffer"]

"}}}
"--- w3m.vim ---"{{{

let g:w3m#external_browser = 'firefox'

"let g:w3m#homepage = 'http://www.bing.com/'
let g:w3m#homepage = 'http://www.google.co.jp/'

"}}}
"--- vimconsole.vim ---"{{{

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
"--- vim-over ---"{{{

"@Imcomplete('do not functioned')
if neobundle#tap('vim-over')
	function! neobundle#hooks.on_source(bundle)
		autocmd KeyMapping User * OverCommandLineNoremap <C-l> <Esc>
	endfunction
	call neobundle#untap()
endif

"}}}
"--- vim-submode ---"{{{

let g:submode_timeout = 0

if neobundle#tap('vim-submode')
	augroup PluginPrefs
		" Window Resizer
		autocmd User * call submode#enter_with('window_resize', 'n', '', '<C-s>w')
		autocmd User * call submode#map('window_resize', 'n', '', 'j', '<C-w>+')
		autocmd User * call submode#map('window_resize', 'n', '', 'k', '<C-w>-')
		autocmd User * call submode#map('window_resize', 'n', '', 'h', '<C-w><')
		autocmd User * call submode#map('window_resize', 'n', '', 'l', '<C-w>>')
		autocmd User * call submode#map('window_resize', 'n', '', '=', '<C-w>=')
		autocmd User * call submode#map('window_resize', 'n', '', '_', '<C-w>_')

		" Fold Mover
		autocmd User * call submode#enter_with('fold_move', 'n', '', '<C-s>z')
		autocmd User * call submode#map('fold_move', 'n', 'e', 'j', "foldlevel('.') > 0 ? 'zczjzozz'   : 'zjzozz'")
		autocmd User * call submode#map('fold_move', 'n', 'e', 'k', "foldlevel('.') > 0 ? 'zczkzo[zzz' : 'zkzo[zzz'")
		autocmd User * call submode#map('fold_move', 'n', '',  'h', '[z')
		autocmd User * call submode#map('fold_move', 'n', '',  'l', ']z')

		" Buffer Changer
		autocmd User * call submode#enter_with('buffer_change', 'n', '', '<C-s>b')
		autocmd User * call submode#map('buffer_change', 'n', 's', 'n', ':bnext<CR>')
		autocmd User * call submode#map('buffer_change', 'n', 's', 'p', ':bprevious<CR>')

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
		autocmd User * call submode#enter_with('tab_move', 'n', '', '<C-s>t')
		autocmd User * call submode#map('tab_move', 'n', 's', 'n', ':call LoopableTabMoveNext()<CR>')
		autocmd User * call submode#map('tab_move', 'n', 's', 'p', ':call LoopableTabMovePrev()<CR>')

		" WinTab Mover
		" Current buffer move to next tab "{{{
		command! BufTabMovePrev execute 'normal! mZ:hide<CR>gT:vsp<CR>`Z'
		command! BufTabMoveNext execute 'normal! mZ' . (winnr('$') <= 1 ? ':hide<CR>' : ':hide<CR>gt') . ':vsp<CR>`Z'
		"}}}
		autocmd User * call submode#enter_with('wintab_move', 'n', '', '<C-s>N', ':BufTabMoveNext<CR>')
		autocmd User * call submode#enter_with('wintab_move', 'n', '', '<C-s>P', ':BufTabMovePrev<CR>')
		autocmd User * call submode#map('wintab_move', 'n', '', 'N', ':BufTabMoveNext<CR>')
		autocmd User * call submode#map('wintab_move', 'n', '', 'P', ':BufTabMovePrev<CR>')
		autocmd User * call submode#map('wintab_move', 'n', '', 'H', '<C-w>H')
		autocmd User * call submode#map('wintab_move', 'n', '', 'J', '<C-w>J')
		autocmd User * call submode#map('wintab_move', 'n', '', 'K', '<C-w>K')
		autocmd User * call submode#map('wintab_move', 'n', '', 'L', '<C-w>L')
	augroup END
endif

"}}}
"--- vim-ref ---" {{{

let g:ref_use_vimproc = 1

" }}}
"--- ref-dicts-en ---" {{{
"@See('http://d.hatena.ne.jp/akishin999/20131024/1382569289')

let g:ref_source_webdict_sites = {
\	'weblio' : {
\		'url' : 'http://ejje.weblio.jp/content/%s'
\	}
\}

let g:ref_source_webdict_sites['default'] = 'weblio'

function! s:weblio_filter(output) "{{{
	let l:lines  = split(a:output, "\n")
	"@Incomplete('do not filtered')
	let l:lines1 = map(l:lines, 'substitute(v:val, "\v(発音記号|音声を聞く|ダウンロード再生)\n", "", "g")')
	return join(l:lines1[60 : ], "\n")
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
"--- incsearch.vim ---"{{{

"@Imcomplete('do not functioned')
if neobundle#tap('incsearch.vim')
	function! neobundle#hooks.on_source(bundle)
		augroup KeyMapping
			autocmd User * IncSearchNoreMap <C-j> <CR>
			autocmd User * IncSearchNoreMap <C-l> <Esc>
		augroup END
	endfunction
	call neobundle#untap()
endif

"}}}
"--- Conque-Shell ---"{{{

let g:ConqueTerm_CloseOnEnd     = 1
let g:ConqueTerm_SessionSupport = 1
let g:ConqueTerm_ReadUnfocused  = 1
let g:ConqueTerm_Color          = 1
let g:ConqueTerm_InsertOnEnter  = 0
let g:ConqueTerm_StartMessages  = 1

"}}}
"--- TaskList.vim ---"{{{

" TaskList search these
let g:tlTokenList = ["FIXME", "TODO", "XXX", "NOTE"]

" Open window at bottom
let g:tlWindowPosition = 1

" Restore opened position when closed TaskList
let g:tlRememberPosition = 1

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
"--- For Debug ---"{{{

" Local my plugins
let s:makes = ['arot13.vim',
\              'ahoge-put.vim',
\              'asql.vim',
\              'adrone.vim',
\              'aho-bakaup.vim']

let s:makes_dir = '~/Repository/'


" If valid local plugin, disable bundled same plugin
for s:plug in s:makes
	let s:plug_dir = s:makes_dir . s:plug

	if isdirectory(expand(s:plug_dir))
		let &runtimepath .= ',' . s:plug_dir
		execute ':NeoBundleDisable' s:plug
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

	let l:mod = len(filter(copy(l:bufnrs), "getbufvar(v:val, '&modified')")) ? '+' : ''
	let l:sp = (l:no . l:mod) ==# '' ? '' : ' '

	let l:curbufnr = bufnrs[tabpagewinnr(a:n) - 1]
	let l:fname = pathshorten(bufname(l:curbufnr))
	if l:fname ==# ''
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

"@Experiment('commented out')
" Manually generate my help tags
"if isdirectory(expand('~/.vim/doc'))
"	helptags ~/.vim/doc
"endif

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
autocmd FileEvent BufReadPost * call s:visit_past_position()


" If you using wim command prompt, listchars using safe chars
autocmd FileEvent VimEnter,WinEnter,BufWinEnter,BufRead,EncodingChanged *
	\	if &encoding ==# 'utf-8' && !s:is_doswin
	\|		let &listchars = 'tab:»_,trail:_,extends:»,precedes:«,nbsp:%,eol:↲'
	\|	else
	\|		let &listchars = 'tab:>_,trail:_,extends:>,precedes:<,nbsp:%'
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
command! -bang NewOverridden new<bang> | setf none
AlterCommand new NewOverridden

command! -bang VnewOverridden vnew<bang> | setf none
AlterCommand vnew VnewOverridden

command! -bang EnewOverridden enew<bang> | setf none
AlterCommand enew EnewOverridden

command! -bang TabnewOverridden tabnew<bang> | setf none
AlterCommand tabnew TabnewOverridden

" }}}
" Usefull {{{

" Grep and Open current buffer
command! -nargs=1 GrepNow vimgrep <args> % | cwindow


" Save a Temporary Directory
" {{{

autocmd FileEvent BufNew * let b:tdir_dir = get(b:, 'tdir_dir', 'Not set tdir')

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

" }}}


" Revese Lines
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
command! -range=% ReverseLine :<line1>, <line2> call s:reverse_line()


" Rename current buffer file
function! s:rename_to(to_file) "{{{
	let l:this_file    = fnameescape(expand('%:t'))
	let l:to_file      = fnameescape(a:to_file)
	let l:editing_file = &modified

	if l:editing_file
		echoerr 'Please :write this file'
		return
	endif

	let l:failed = rename(l:this_file, l:to_file)
	if l:failed
		echoerr printf('Rename %s to %s is failed', l:this_file, a:to_file)
		return
	endif

	let l:bufnr = bufnr('%')
	execute ':edit' l:to_file
	execute ':' . l:bufnr . 'bdelete'

	echo printf('Renamed %s to %s', l:this_file, l:to_file)
endfunction "}}}
command! -nargs=1 Rename call <SID>rename_to(<q-args>)

"}}}
" Life Helper {{{

" Vim Utils {{{

command! VimConfig e $MYVIMRC

command! VimConfigTab tabnew $MYVIMRC

command! Reload so $MYVIMRC
	\|	if has('gui_running')
	\|		so $MYGVIMRC
	\|	endif

command! ForceSave w !sudo tee > /dev/null %

command! CdBufDir cd %:p:h

command! ColorPreview Unite colorscheme -auto-preview

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

	"@Incomplete('wait sync here')
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

	"@Incomplete('wait here')
	"execute ':TweetVimSwitchAccount ' g:vimrc.private['twitter']['curr_ac']
endfunction "}}}
command! TweetPublic        call TweetPublicFunc()


command!  Bitly TweetVimBitly
cnoreabbr tvs   TweetVimSwitchAccount

" }}}


" To Service Name
cnoreabbr Lingr J6uil
command!  Lingr NOP


" Beautifull Life
command!  JazzUpdate JazzradioUpdateChannels
command!  JazzList   Unite jazzradio
cnoreabbr JazzPlay   JazzradioPlay
command!  JazzPlay   NOP
command!  JazzStop   JazzradioStop


" Translates Languages
cnoreabbr Translate ExciteTranslate
cnoreabbr Weblio    Ref webdict weblio

command!  Translate NOP
command!  Weblio    NOP


" Markdown help on online
command! MarkdownHelpOnline W3mTab http://qiita.com/Qiita/items/c686397e4a0f4f11683d#3-4

" }}}
" Development {{{

" Open Develop Buffer {{{

" vimconsole.vim
cnoreabbr Log      VimConsoleLog
cnoreabbr LogClear VimConsoleClear
cnoreabbr LogOpen  VimConsoleOpen

command!  Log      NOP
command!  LogClear NOP
command!  LogOpen  NOP


" GHCi
cnoreabbr RunGhc   !runghc %
cnoreabbr Ghci     VimShellInteractive ghci
cnoreabbr Sghci    VimShellInteractive --split='sp' ghci
cnoreabbr Vghci    VimShellInteractive --split='vsp' ghci
cnoreabbr GhciTab  VimShellInteractive --split='tabnew' ghci
cnoreabbr Hoogle   Ref hoogle

command!  RunGhc   NOP
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
" Read something {{{

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

command! HighlightListTab call s:highlight_list_tab()

" }}}

" }}}

" }}}


"-------------------------"
"       Key_Mapping       "
"-------------------------"
" Disables {{{

augroup KeyMapping
	" I can use some mapping to hoge<C-c>
	autocmd User * nnoremap <C-c>      <NOP>
	autocmd User * nnoremap <C-c><C-c> <C-c>

	autocmd User * nnoremap <Up>    <NOP>
	autocmd User * nnoremap <Down>  <NOP>
	autocmd User * nnoremap <Left>  <NOP>
	autocmd User * nnoremap <Right> <NOP>

	autocmd User * inoremap <Up>    <NOP>
	autocmd User * inoremap <Down>  <NOP>
	autocmd User * inoremap <Left>  <NOP>
	autocmd User * inoremap <Right> <NOP>

	autocmd User * cnoremap <Left>  <NOP>
	autocmd User * cnoremap <Right> <NOP>

	autocmd User * cnoremap [Left] <Left>
augroup END

" }}}
" Bashnize Command Mode {{{

augroup KeyMapping
	autocmd User * nmap     <C-j> <CR>
	autocmd User * imap     <C-j> <CR>

	autocmd User * cnoremap <C-b>      <Left>
	autocmd User * cnoremap <C-f>      <Right>
	autocmd User * cnoremap <C-a>      <Home>
	autocmd User * cnoremap <C-h>      <BS>
	autocmd User * cnoremap <C-d>      <Del>
	autocmd User * cnoremap <C-e>      <End>
	autocmd User * cnoremap <C-k><C-k> <C-\>e getcmdpos() < 2 ? '' : getcmdline()[:getcmdpos()-2]<CR>
augroup END

" }}}
" Global KeyMaps {{{

" Prepare temporary functions {{{

" Compress continuous space
function! s:compress_spaces() "{{{
	let l:recent_pattern = @/

	try
		substitute/\s\s\+/ /g
		normal! ==
	catch
	finally
		let @/ = l:recent_pattern
	endtry

	nohlsearch
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


" Easily putting short separator for many filetypes
" function! s:put_short_separator(put_upper) {{{

autocmd FileEvent FileType,WinEnter,BufWinEnter * let s:long_separator =
\	&ft =~# '\v(vim|vimspec)'    ? '"#-=- -=- -=- -=- -=- -=- -=- -=- -=-#"'
\:	&ft =~# '\v(java|cs|cpp|c)'  ? '/* ---===---===---===---===---===---===--- */'
\:	&ft ==# 'haskell'            ? '-- - - - - - - - - - - - - - - - --'
\:	&ft ==# 'coq'                ? '(* - - - - - - - - - - - - - - - *)'
\:	&ft ==# 'mysql'              ? '-- - - - - - - - - - - - - - - - --'
\:	&ft =~# '\v(markdown|eruby)' ? '<!-- - - - - - - - - - - - - - - - -->'
\:	&ft =~# '\v(ruby|sh)'        ? '#- - - - - - - - - - - - - - - - -#'
\:	&ft =~# '\v(text|none)'      ? '- - - - - - - - - - - - - - - - - - - -'
\                                : 'long_separator_undefined'


function! s:put_long_separator(put_upper)
	execute 'normal!' (a:put_upper ? 'O' : 'o')
	execute 'normal! 0D'
	execute 'normal! i' s:long_separator
	execute 'normal! =='
endfunction

"}}}


" Easily putting long separator for many filetypes
" function! s:put_short_separator(put_upper) {{{

autocmd FileEvent FileType,WinEnter,BufWinEnter * let s:short_sparator =
\	&ft =~# '\v(vim|vimspec)'    ? '"#--- --- ---#"'
\:	&ft =~# '\v(java|cs|cpp|c)'  ? '/* -=-=-=-=-=-=-=-=- */'
\:	&ft ==# 'haskell'            ? '-- - - - - - --'
\:	&ft ==# 'coq'                ? '(* - - - - - *)'
\:	&ft ==# 'mysql'              ? '-- - - - - - --'
\:	&ft =~# '\v(markdown|eruby)' ? '<!-- - - - - - -->'
\:	&ft =~# '\v(ruby|sh)'        ? '#- - - - - - -#'
\:	&ft =~# '\v(text|none)'      ? '- - - - - - - - - -'
\                                : 'short_separator_undefined'


function! s:put_short_separator(put_upper)
	execute 'normal!' (a:put_upper ? 'O' : 'o')
	execute 'normal! 0D'
	execute 'normal! i' s:short_sparator
	execute 'normal! =='
endfunction

" }}}


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

		" doautocmd for normally keymappings
		let &filetype = &filetype
	else
		nnoremap n gt
		nnoremap p gT
		nnoremap f <C-f>
		nnoremap b <C-b>
		nnoremap o <C-o>
		nnoremap i <C-i>
		nnoremap u <C-w><C-w>
	endif

	let s:onehand_enabled = !s:onehand_enabled
	echo (s:onehand_enabled ? '' : 'no') . 'onehand'
endfunction

"}}}


" If you has nofile buffer, close it.
"@Marked('maybe this was not does purported function')
function! s:bufclose_filetype(ft) "{{{
	for l:w in range(1, winnr('$'))
		let l:buf_ft = getwinvar(l:w, '&ft')

		if l:buf_ft ==# a:ft
			execute ':' . l:w . 'wincmd w'
			execute ':quit'

			break
		endif
	endfor
endfunction "}}}

" }}}
" Override mapping {{{

augroup KeyMapping
	" † Rebirth Of The Neo Ex
	autocmd User * nnoremap Q gQ
	autocmd User * nnoremap <C-n> gt
	autocmd User * nnoremap <C-p> gT

	autocmd User * inoremap <C-l> <Esc>
	autocmd User * vnoremap <C-l> <Esc>
	autocmd User * cnoremap <C-l> <Esc>
augroup END

" }}}
" Appends " {{{

augroup KeyMapping
	autocmd User * nnoremap <C-m> o<Esc>

	autocmd User * nnoremap <silent> m: :<C-u>marks<CR>
	autocmd User * nnoremap <silent> q: :<C-u>register<CR>
	autocmd User * nnoremap <silent> z: :<C-u>tabs<CR>
	autocmd User * nnoremap <silent> g: :<C-u>buffers<CR>
	autocmd User * nnoremap <silent> g> :<C-u>messages<CR>

	autocmd User * nnoremap <silent> <leader>b         :<C-u>NewOverridden<CR>:resize 5<CR>:setl buftype=nofile<CR>
	autocmd User * nnoremap <silent> <leader>B         :<C-u>NewOverridden<CR>:resize 5<CR>
	autocmd User * nnoremap <silent> <leader>ps        :<C-u>call <SID>put_short_separator(0)<CR>
	autocmd User * nnoremap <silent> <leader>Ps        :<C-u>call <SID>put_short_separator(1)<CR>
	autocmd User * nnoremap <silent> <leader>pl        :<C-u>call <SID>put_long_separator(0)<CR>
	autocmd User * nnoremap <silent> <leader>Pl        :<C-u>call <SID>put_long_separator(1)<CR>
	autocmd User * nnoremap <silent> <leader>pd        :<C-u>execute 'normal! a' . strftime('%c')<CR>
	autocmd User * nnoremap <silent> <leader><leader>h :<C-u>helpclose<CR>
	autocmd User * nnoremap <silent> <Space><Space>    :<C-u>call <SID>compress_spaces()<CR>
	autocmd User * nnoremap <silent> <leader>k         :<C-u>call <SID>cursor_up_to_lid()<CR>
	autocmd User * nnoremap <silent> <leader>j         :<C-u>call <SID>cursor_down_to_ground()<CR>

	autocmd User * nnoremap <silent> <C-k><C-r> :<C-u>Reload<CR>
	autocmd User * nnoremap <silent> <C-k><C-l> :<C-u>nohlsearch<CR>
	autocmd User * nnoremap <silent> <C-k><C-j> :<C-u>write<CR>
	autocmd User * nnoremap <silent> <C-k>J     :<C-u>wall<CR>
	autocmd User * nnoremap <silent> <C-k>R     :<C-u>let &filetype = &filetype<CR>
	autocmd User * nnoremap <silent> <C-k>r     :<C-u>doautocmd User<CR>
	autocmd User * nnoremap <silent> <C-k>l     :<C-u>source %<CR>


	autocmd User * inoremap <silent> <C-k><C-j> <Esc>:write<CR>
	autocmd User * inoremap <silent> <C-k>J     <Esc>:wall<CR>

	autocmd User * inoremap <C-k><C-k> <C-o>"_d$
	autocmd User * inoremap <C-k><C-y> <Esc>k"zyyjV"zp:let @z = ''<CR>A


	"autocmd User * vnoremap <silent> [k :<C-u>call <SID>cursor_up_to_lid()<CR>
	"autocmd User * vnoremap <silent> ]k :<C-u>call <SID>cursor_up_to_lid()<CR>
	"autocmd User * vnoremap <silent> [j :<C-u>call <SID>cursor_down_to_ground()<CR>
	"autocmd User * vnoremap <silent> ]j :<C-u>call <SID>cursor_down_to_ground()<CR>


	autocmd User * cnoremap <C-k><C-p> <Up>
	autocmd User * cnoremap <C-k><C-n> <Down>
	autocmd User * cnoremap <C-]>      '<,'>
augroup END

" }}}
" Foldings {{{

augroup KeyMapping
	autocmd User * nnoremap <expr> h foldclosed('.') > -1 ? 'zo' : 'h'
	autocmd User * nnoremap <expr> l foldclosed('.') > -1 ? 'zo' : 'l'

	autocmd User * nnoremap zj zjzo
	autocmd User * nnoremap zk zkzo
	autocmd User * nnoremap z< V$%zf
augroup END

" }}}
" Windows and Buffers {{{

augroup KeyMapping
	autocmd User * nnoremap <Space>h <C-w>h
	autocmd User * nnoremap <Space>j <C-w>j
	autocmd User * nnoremap <Space>k <C-w>k
	autocmd User * nnoremap <Space>l <C-w>l

	autocmd User * nnoremap <silent> <C-w>t :<C-u>TabnewOverridden<CR>
	autocmd User * nnoremap <silent> <C-w>T :<C-u>tabclose<CR>
	autocmd User * nnoremap <silent> <C-w>c :<C-u>bdelete<CR>
	autocmd User * nnoremap <silent> <C-w>C :<C-u>bdelete!<CR>
	autocmd User * nnoremap <silent> <C-w>N :<C-u>EnewOverridden!<CR>
	autocmd User * nnoremap <silent> <C-w>Q :<C-u>quitall<CR>
	autocmd User * nnoremap <silent> <C-w>" :<C-u>resize 5<CR>

	autocmd User * nnoremap <silent><expr> <C-w>bt 'mZ:tabnew<CR>`Zzz'       . (foldlevel('.') > 0 ? 'zo' : '')
	autocmd User * nnoremap <silent><expr> <C-w>bT 'mZ<C-w>c:tabnew<CR>`Zzz' . (foldlevel('.') > 0 ? 'zo' : '')
augroup END

" }}}
" Toggle options {{{

augroup KeyMapping
	autocmd User * nnoremap <silent><expr> <C-h><C-d> (&diff ? ':diffoff' : ':diffthis') . '\|set diff?<CR>'
	autocmd User * nnoremap <silent><expr> <C-h><C-v> ':setl virtualedit=' . (&virtualedit ==# '' ? 'all' : '') . ' virtualedit?<CR>'

	autocmd User * nnoremap <silent> <C-h><C-w> :<C-u>setl wrap!           wrap?          <CR>
	autocmd User * nnoremap <silent> <C-h><C-c> :<C-u>setl cursorline!     cursorline?    <CR>
	autocmd User * nnoremap <silent> <C-h><C-e> :<C-u>setl expandtab!      expandtab?     <CR>
	autocmd User * nnoremap <silent> <C-h><C-r> :<C-u>setl relativenumber! relativenumber?<CR>
	autocmd User * nnoremap <silent> <C-h><C-l> :<C-u>setl list!           list?          <CR>
	autocmd User * nnoremap <silent> <C-h><C-n> :<C-u>setl number!         number?        <CR>
	autocmd User * nnoremap <silent> <C-h><C-s> :<C-u>setl wrapscan!       wrapscan?      <CR>
	autocmd User * nnoremap <silent> <C-h>jk    :<C-u>call <SID>toggle_onehand_mode()<CR>

	autocmd User * inoremap <silent> <C-k><C-e> <C-o>:setl expandtab! expandtab?<CR>
augroup END

" }}}
" for Plugins {{{

augroup KeyMapping
	" netrw
	autocmd User * nnoremap <silent> <leader>e         :<C-u>Vexplore<CR>
	autocmd User * nnoremap <silent> <leader><leader>e :<C-u>Sexplore<CR>
	autocmd User * nnoremap <silent> <leader>E         :<C-u>Explore<CR>
	autocmd User * nnoremap <silent> <leader><leader>E :<C-u>Texplore<CR>


	" open-browser.vim
	autocmd User * nmap <leader>w <Plug>(openbrowser-open)


	" vim-quickrun
	autocmd User * nnoremap <silent> <leader>R         :<C-u>QuickRun -runner shell<CR>
	autocmd User * nnoremap <silent> <leader><leader>r :<C-u>call <SID>bufclose_filetype('quickrun')<CR>


	" vimshell
	autocmd User * nnoremap <silent> <leader>v         :<C-u>VimShell -split-command=vsp -toggle<CR>
	autocmd User * nnoremap <silent> <leader><leader>v :<C-u>VimShell -split-command=sp  -toggle<CR>
	autocmd User * nnoremap <silent> <leader>V         :<C-u>VimShellBufferDir -create<CR>
	autocmd User * nnoremap <silent> <leader><leader>V :<C-u>VimShell -split-command=tabnew -create<CR>


	" Unite
	autocmd User * nnoremap <silent> <leader>uf        :<C-u>Unite -ignorecase -start-insert outline:foldings<CR>
	autocmd User * nnoremap <silent> <leader>um        :<C-u>Unite -ignorecase neomru/file<CR>
	autocmd User * nnoremap <silent> <leader><leader>u :<C-u>call <SID>bufclose_filetype('unite')<CR>


	" excitetranslate-vim
	autocmd User * nnoremap <silent> <leader>t :<C-u>ExciteTranslate<CR>


	" vim-over
	autocmd User * nnoremap <silent>       :%s/       :<C-u>OverCommandLine<CR>%s/
	autocmd User * nnoremap <silent>       :s/        :<C-u>OverCommandLine<CR>s/
	autocmd User * nnoremap <silent><expr> <C-k><C-s> ':OverCommandLine<CR>%s/\<' . expand('<cword>') . '\>/'
	autocmd User * nnoremap <silent><expr> <C-k>S     ':OverCommandLine<CR>%s/\<' . expand('<cword>') . '\>/' . expand('<cword>')
	autocmd User * vnoremap <silent>       :s/        :<C-u>OverCommandLine<CR>'<,'>s/


	" anzu-chan
	autocmd User * nmap n      <Plug>(anzu-n-with-echo)zv
	autocmd User * nmap N      <Plug>(anzu-N-with-echo)zv
	autocmd User * nmap *      <Plug>(anzu-star-with-echo)zv
	autocmd User * nmap #      <Plug>(anzu-sharp-with-echo)zv
	autocmd User * nmap <C-w>* <C-w>v<Plug>(anzu-star-with-echo)zv
	autocmd User * nmap <C-w># <C-w>v<Plug>(anzu-sharp-with-echo)zv


	" incsearch.vim
	autocmd User * nmap <expr>      /                 foldclosed('.') > -1 ? 'zv<Plug>(incsearch-forward)'  : '<Plug>(incsearch-forward)'
	autocmd User * nmap <silent>    <leader>/         /\m\C
	autocmd User * nmap <silent>    <leader><leader>/ /\m\C\<\>[Left][Left]
	autocmd User * nmap             g/                /\<<C-r>"\><CR>
	autocmd User * nmap <expr>      ?                 foldclosed('.') > -1 ? 'zv<Plug>(incsearch-backward)' : '<Plug>(incsearch-backward)'
	autocmd User * nmap <silent>    <leader>?         ?\m\C
	autocmd User * nmap <silent>    <leader><leader>? ?\m\C\<\>[Left][Left]
	autocmd User * nmap             g?                ?\<<C-r>"\><CR>
	autocmd User * vmap <expr>      /                 foldclosed('.') > -1 ? 'zv<Plug>(incsearch-forward)'  : '<Plug>(incsearch-forward)'
	autocmd User * vmap <expr>      ?                 foldclosed('.') > -1 ? 'zv<Plug>(incsearch-backward)' : '<Plug>(incsearch-backward)'


	" TaskList.vim
	autocmd User * nmap <leader>T <Plug>TaskListToggle


	" undotree
	autocmd User * nnoremap <leader>U :<C-u>UndotreeToggle<CR>


	" neosnippet.vim
	autocmd User * imap <expr> <C-s> neosnippet#expandable() ? '<Plug>(neosnippet_expand)' : '<Plug>(neosnippet_jump)'
	autocmd User * smap <expr> <C-s> neosnippet#expandable() ? '<Plug>(neosnippet_expand)' : '<Plug>(neosnippet_jump)'
augroup END

" }}}

" }}}
" Buffer Local KeyMaps {{{

augroup PluginPrefs
	autocmd User int-* nnoremap <buffer> q          <NOP>
	autocmd User int-* nnoremap <buffer> <C-n>      gt
	autocmd User int-* nnoremap <buffer> <C-p>      gT
	autocmd User int-* nnoremap <buffer> <C-l>      <NOP>

	autocmd User int-* nmap     <buffer> <C-]>      <Plug>(vimshell_int_clear)
	autocmd User int-* nmap     <buffer> Q          <Plug>(vimshell_int_exit)
	autocmd User int-* nmap     <buffer> gj         <Plug>(vimshell_int_next_prompt)
	autocmd User int-* nmap     <buffer> gk         <Plug>(vimshell_int_previous_prompt)

	autocmd User int-* inoremap <buffer> <C-l>      <Esc>
	autocmd User int-* inoremap <buffer> <C-b>      <Left>
	autocmd User int-* inoremap <buffer> <C-f>      <Right>
	autocmd User int-* inoremap <buffer> <C-e>      <End>
	autocmd User int-* inoremap <buffer> <C-d>      <Del>

	autocmd User int-* imap     <buffer> <C-n>      <C-o><Plug>(vimshell_int_next_prompt)<End>
	autocmd User int-* imap     <buffer> <C-p>      <C-o><Plug>(vimshell_int_previous_prompt)<End>
	autocmd User int-* imap     <buffer> <C-]>      <C-o><Plug>(vimshell_int_clear)
	autocmd User int-* imap     <buffer> <CR>       <Plug>(vimshell_int_execute_line)
	autocmd User int-* imap     <buffer> <C-k><C-p> <Plug>(vimshell_int_history_unite)


	autocmd User ref-* nnoremap <silent><buffer> Q :<C-u>quit<CR>
augroup END

" }}}


"-------------------------"
"        File_Type        "
"-------------------------"
"{{{

" If buffer does not has filetype, set filetype 'none'
autocmd ExtensionType VimEnter,BufNew * if &ft ==# '' | setf none | endif


"@Incomplete('do not functioned')
" netrw
autocmd FileEvent User netrw highlight default link CursorLine Visual

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
doautocmd User
let g:vimrc['loaded'] = 1
