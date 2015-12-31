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
"  This file supported       "
"    - *NIX (exclude MacOSX) "
"    - Cygwin                "
"    - Windows Kaoriya GVim  "
"----------------------------"
"     Eigo - Ingulisshu      "
"----------------------------"
" Ideas {{{

"-- Unite outline ? -> view C-Sharp <summary>~</summary> with method name

" }}}
" Issues {{{

"-- automatic mkdir './C:' when execute NeoBundleInstall in windows kaoriya
"  -- does neobundle think that is repository...?

"-- shot-f doesn't functioned in i_<C-o> temporary normal mode

"-- conflicted? vim-ruby and rspec.vim when those was set NeoBundleLazy
"  -- does not loaded syntax of rspec.vim

"-- submode wintab-move over 1 previous tab

" }}}
" Todo {{{

"-- read help options.jax

"-- read help windows.txt

"-- read help 'cino'

"-- reference to help 'ftplugin' L2159

"-- read help syntax.txt

"-- read help clear-undo.txt

"-- read help usr_41

" }}}


"----------------------------------------
" {- Message -} "
" @Bugs         => It has some bug
" @Incomplete   => It is incompleted implementation
" @Unchecked    => I implemented without check
" @Unsupported  => I don't support some environment
" @Deprecated   => Deprecated
" @Experiment   => That is experimental implementation
" @Marked       => I have eye on it
" @See          => Please see it
" @Code         => The sample code for how to use
"-------------------


"---------------------"
"      Parameter      "
"---------------------"
" {{{

" Open in preference to an entity
let $MYVIMRC = filereadable(expand('~/.dotfiles/.vimrc'))
\	? expand('~/.dotfiles/.vimrc')
\	: $MYVIMRC

let $MYGVIMRC = filereadable(expand('~/.dotfiles/.gvimrc'))
\	? expand('~/.dotfiles/.gvimrc')
\	: $MYGVIMRC

let g:vimrc = get(g:, 'vimrc', {
\	'loaded'   : 0,
\	'vim_home' : expand('~/.vim')
\})

let s:vimrc_env = expand('~/.vimrc_env')

" Global values for local plugins
let g:vimrc['is_windows'] = get(g:vimrc, 'is_windows', has('win32')    )
let g:vimrc['is_cygwin']  = get(g:vimrc, 'is_cygwin',  has('win32unix'))
let g:vimrc['is_kaoriya'] = get(g:vimrc, 'is_kaoriya', has('kaoriya')  )
let g:vimrc['is_unix']    = get(g:vimrc, 'is_unix',    has('unix')     )

let g:vimrc['has_cygwin'] = executable('cygstart')
let g:vimrc['has_mingw']  = 0  "NOTE: ('dummy')

let s:backupdir = expand('~/.backup/vim_backup')
let s:directory = s:backupdir . '/swp'
let s:undodir   = s:backupdir . '/undo'
let s:viewdir   = s:backupdir . '/view'

" }}}


"-------------------------"
"       Initialize        "
"-------------------------"
" Set encodings {{{

" Set default file encoding
if !g:vimrc['loaded']
	set fileencoding=utf-8 encoding=utf-8
endif

" Specify encoding for this file
scriptencoding utf-8

" }}}
" Declare autocmd groups {{{

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

augroup UserEvent
	autocmd!
augroup END

" }}}
" Build environment for Kaoriya Vim {{{

if g:vimrc['is_kaoriya'] && g:vimrc['is_windows']
	" Set environments
	let $HOME               = $VIM
	let $PATH               = $HOME . '/bin;' . $PATH
	let g:vimrc['vim_home'] = substitute($VIM, '\', '/', 'g') . '/vimfiles'
	let &runtimepath        = &runtimepath . ',' . g:vimrc['vim_home']

	" Use cygwin's commands
	if g:vimrc['has_cygwin']
		let $PATH = '/cygwin/bin;/cygwin/usr/bin;/cygwin/usr/sbin;' . $PATH
	endif

	" Make base directories
	if !isdirectory(g:vimrc['vim_home'])
		call mkdir(g:vimrc['vim_home'])
	endif

	" Use kaoriya's vimproc
	let s:switch_dir       = $VIM . '/switches/enabled'
	let s:suppress_vimproc = s:switch_dir . '/disable-vimproc.vim'

	" If you has mingw, use neobundle's vimproc
	if !g:vimrc['has_mingw'] && filereadable(s:suppress_vimproc)
		call delete(s:suppress_vimproc)
	elseif g:vimrc['has_mingw'] && !filereadable(s:suppress_vimproc)
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

	" Unset kaoriya default preference
	set noignorecase nosmartcase

	" Disable plugins/kaoriya/plugin/{cmdex,scrnmode}.vim
	let g:plugin_cmdex_disable    = 1
	let g:plugin_scrnmode_disable = 1

	" You must open the vimrc by the utf-8
	autocmd FileEvent BufRead $MYVIMRC setl enc=utf8 fenc=utf8
endif

" }}}
" Startup NeoBundle {{{

let s:bundledir = g:vimrc['vim_home'] . '/bundle'

if !isdirectory(s:bundledir)
	call mkdir(s:bundledir)
endif

if has('vim_starting')
	let &runtimepath = &runtimepath . ',' . (g:vimrc['vim_home'] . '/bundle/neobundle.vim')
endif

try
	call neobundle#begin()
catch /E117/  " neobundle.vim not found
	try
		call vimrc#fetch_neobundle(s:bundledir)
		call neobundle#begin()
		echo 'NeoBundle install completed.'
		echo 'Please execute :NeoBundleInstall,'
		echo 'and restart Vim.'
	catch /FALIED/
		call vimrc#echo_error('cloning or starting neobundle.vim failed.')
		call vimrc#echo_error('>> Error build vim environment <<')
	endtry
endtry

unlet s:bundledir

" }}}
" Check backup directories {{{

if !isdirectory(s:backupdir)
	call mkdir(s:backupdir, 'p', 0700)
	call s:system(printf('chown -R %s:%s %s', $USER, $GROUP, s:backupdir))
endif

if !isdirectory(s:directory)
	call mkdir(s:directory, 'p', 0700)
	call s:system(printf('chown -R %s:%s %s', $USER, $GROUP, s:directory))
endif

if !isdirectory(s:undodir)
	call mkdir(s:undodir, 'p', 0700)
	call s:system(printf('chown -R %s:%s %s', $USER, $GROUP, s:undodir))
endif

" }}}
" Enable embedded plugins {{{

if !exists('loaded_matchit')
	" Load matchit.vim
	runtime macros/matchit.vim
	" Get help of matchit.vim
	let s:matchit_doc_from = expand('$VIMRUNTIME/macros/matchit.txt')
	let s:matchit_doc_to   = g:vimrc['vim_home'] . '/doc/matchit.txt'
	if !filereadable(s:matchit_doc_to)
		call writefile(readfile(s:matchit_doc_from), s:matchit_doc_to)
	endif
	unlet s:matchit_doc_to s:matchit_doc_from
endif

" }}}


"-------------------------"
"     Plugin_Manage       "
"-------------------------"
"*** Plugin List *** {{{

NeoBundleFetch 'Shougo/neobundle.vim'
NeoBundleLazy  'basyura/twibill.vim'
NeoBundle      'tyru/open-browser.vim'
NeoBundleLazy  'basyura/bitly.vim'
NeoBundle      'Shougo/unite.vim'
NeoBundle      'Shougo/vimproc.vim'
NeoBundleLazy  'basyura/TweetVim'
NeoBundleLazy  'mattn/webapi-vim'
NeoBundle      'Shougo/vimshell.vim'
NeoBundleLazy  'thinca/vim-quickrun'
NeoBundleLazy  'basyura/J6uil.vim'
NeoBundleLazy  'yuratomo/w3m.vim'
NeoBundle      'supermomonga/vimshell-kawaii.vim'
NeoBundleLazy  'supermomonga/jazzradio.vim'
NeoBundleLazy  'mattn/favstar-vim'
NeoBundleLazy  'ujihisa/unite-colorscheme'
NeoBundleLazy  'Shougo/vinarise.vim'
NeoBundleLazy  'mattn/gist-vim'
NeoBundle      'thinca/vim-ref'
NeoBundle      'ujihisa/ref-hoogle'
NeoBundleLazy  'vim-jp/vital.vim'
NeoBundle      'Shougo/unite-outline'
NeoBundleLazy  'mattn/benchvimrc-vim'
NeoBundleLazy  'jvoorhis/coq.vim'
NeoBundleLazy  'eagletmt/coqtop-vim'
NeoBundleLazy  'thinca/vim-themis'
NeoBundle      'tomasr/molokai'
NeoBundleLazy  'kannokanno/previm'
NeoBundle      'LeafCage/foldCC'
NeoBundle      'kana/vim-submode'
NeoBundle      'mfumi/ref-dicts-en'
NeoBundle      'osyo-manga/vim-anzu'
NeoBundle      'osyo-manga/vim-over'
NeoBundleLazy  'tyru/restart.vim'
NeoBundle      'vim-jp/vimdoc-ja'
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
NeoBundleLazy  'vim-ruby/vim-ruby'
NeoBundleLazy  'Keithbsmiley/rspec.vim'
NeoBundle      'altercation/vim-colors-solarized'
NeoBundle      'aiya000/aho-bakaup.vim'
NeoBundle      'Shougo/neosnippet.vim'
NeoBundle      'Shougo/neosnippet-snippets'
NeoBundle      'aiya000/separetaro.vim'
NeoBundleLazy  'kurocode25/mdforvim'
NeoBundleLazy  'rbtnn/vimconsole.vim'
NeoBundleLazy  'fatih/vim-go'
NeoBundle      'tpope/vim-surround'
NeoBundle      'kana/vim-textobj-user'
NeoBundle      'kana/vim-textobj-indent'
NeoBundle      'Shougo/neocomplete.vim'
NeoBundle      'Shougo/deoplete.nvim'
NeoBundleLazy  'soramugi/auto-ctags.vim'
NeoBundleLazy  'tsukkee/unite-tag'
NeoBundle      'aiya000/vimshell-command-dehanai.vim'
NeoBundle      'osyo-manga/vim-textobj-from_regexp'
NeoBundleLazy  'leafgarland/typescript-vim'
NeoBundle      'tpope/vim-repeat'
NeoBundleLazy  'lambdalisue/vim-pager'
NeoBundleLazy  'lambdalisue/vim-manpager'
NeoBundle      'thinca/vim-visualstar'
NeoBundle      'tpope/vim-fugitive'
NeoBundleLazy  'rhysd/try-colorscheme.vim'
NeoBundle      'jonathanfilip/vim-lucius'
NeoBundleLazy  'aiya000/unite-syntax'
NeoBundleLazy  'Shougo/unite-help'
NeoBundleLazy  'osyo-manga/unite-filetype'
NeoBundleLazy  'cohama/agit.vim'
NeoBundleLazy  'Shougo/unite-session'
NeoBundleLazy  'whatyouhide/vim-textobj-xmlattr'
NeoBundleLazy  'yomi322/neco-tweetvim'
NeoBundleLazy  'yomi322/unite-tweetvim'
NeoBundleLazy  'itchyny/vim-haskell-indent'
NeoBundle      'aiya000/submode-window_move.vim'
NeoBundleLazy  'ujihisa/repl.vim'
NeoBundleLazy  'mattn/emmet-vim'
NeoBundle      'kana/vim-operator-user'
NeoBundle      'haya14busa/vim-operator-flashy'

" }}}
"*** Plugin Depends and Auto Config *** {{{

if neobundle#tap('vimproc.vim')
	call neobundle#config('vimproc.vim', {
	\	'build' : {
	\		'linux'  : 'make',
	\		'cygwin' : 'make'
	\	},
	\	'disabled' : g:vimrc['is_kaoriya'] && g:vimrc['is_windows'] && !g:vimrc['has_mingw']
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
	\	'on_cmd' : 'TweetVim'
	\})
	call neobundle#untap()
endif
if neobundle#tap('vim-quickrun')
	call neobundle#config('vim-quickrun', {
	\	'depends'  : 'Shougo/vimproc.vim',
	\	'on_cmd' : {
	\		'name'     : 'QuickRun',
	\		'complete' : 'filetype'
	\	},
	\	'on_map' : [
	\		'<Plug>(quickrun)',
	\		'<Plug>(quickrun-op)'
	\	]
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
	\	'on_cmd' : 'J6uil'
	\})
	call neobundle#untap()
endif
if neobundle#tap('w3m.vim')
	call neobundle#config('w3m.vim', {
	\	'on_cmd' : [
	\		'W3m',
	\		'W3mHistory',
	\		'W3mHistoryClear',
	\		'W3mLocal',
	\		'W3mSplit',
	\		'W3mTab',
	\		'W3mVSplit'
	\	]
	\})
	call neobundle#untap()
endif
"@Bugs('not fuond jazzradio#channel_id_comlete')
if neobundle#tap('jazzradio.vim')
	call neobundle#config('jazzradio.vim', {
	\	'depends'  : 'Shougo/unite.vim',
	\	'on_unite' : 'jazzradio',
	\	'on_cmd'   : [
	\		'JazzradioUpdateChannels',
	\		'JazzradioStop', {
	\			'name'     : 'JazzradioPlay',
	\			'complete' : 'customlist,jazzradio#channel_id_comlete'
	\		}
	\	]
	\})
	call neobundle#untap()
endif
if neobundle#tap('favstart-vim')
	call neobundle#config('favstart-vim', {
	\	'on_cmd' : 'FavStar'
	\})
	call neobundle#untap()
endif
if neobundle#tap('vinarise.vim')
	call neobundle#config('vinarise.vim', {
	\	'on_cmd' : [
	\		'Vinarise',
	\		'VinariseDump'
	\	]
	\})
	call neobundle#untap()
endif
if neobundle#tap('gist-vim')
	call neobundle#config('gist-vim', {
	\	'depends' : 'mattn/webapi-vim',
	\	'on_cmd'  : 'Gist'
	\})
	call neobundle#untap()
endif
if neobundle#tap('ref-hoogle')
	call neobundle#config('ref-hoogle', {
	\	'depends'  : 'thinca/vim-ref'
	\})
	call neobundle#untap()
endif
if neobundle#tap('benchvimrc-vim')
	call neobundle#config('benchvimrc-vim', {
	\	'on_cmd' : 'BenchVimrc'
	\})
	call neobundle#untap()
endif
if neobundle#tap('coq.vim')
	call neobundle#config('coq.vim', {
	\	'on_ft' : 'coq'
	\})
	call neobundle#untap()
endif
if neobundle#tap('coqtop-vim')
	call neobundle#config('coqtop-vim', {
	\	'depends' : 'Shougo/vimproc.vim',
	\	'on_ft'   : 'coq'
	\})
	call neobundle#untap()
endif
if neobundle#tap('vim-themis')
	call neobundle#config('vim-themis', {
	\	'on_ft' : [
	\		'vim',
	\		'vimspec'
	\	]
	\})
	call neobundle#untap()
endif
if neobundle#tap('previm')
	call neobundle#config('previm', {
	\	'on_ft'  : 'markdown',
	\	'on_cmd' : 'PrevimOpen'
	\})
	call neobundle#untap()
endif
if neobundle#tap('ref-dicts-en')
	call neobundle#config('ref-dicts-en', {
	\	'depends' : 'thinca/vim-ref'
	\})
	call neobundle#untap()
endif
if neobundle#tap('restart.vim')
	call neobundle#config('restart.vim', {
	\	'gui'    : 1,
	\	'on_cmd' : 'Restart'
	\})
	call neobundle#untap()
endif
if neobundle#tap('vim-scouter')
	call neobundle#config('vim-scouter', {
	\	'on_cmd' : {
	\		'name'     : 'Scouter',
	\		'complete' : 'file'
	\	}
	\})
	call neobundle#untap()
endif
if neobundle#tap('TaskList.vim')
	call neobundle#config('TaskList.vim', {
	\	'on_cmd' : [
	\		'TaskList',
	\		'TaskListToggle'
	\	],
	\	'on_map' : '<Plug>TaskListToggle'
	\})
	call neobundle#untap()
endif
if neobundle#tap('undotree')
	call neobundle#config('undotree', {
	\	'on_cmd' : [
	\		'UndotreeToggle',
	\		'UndotreeFocus',
	\		'UndotreeShow',
	\		'UndotreeHide'
	\	]
	\})
	call neobundle#untap()
endif
if neobundle#tap('adrone.vim')
	call neobundle#config('adrone.vim', {
	\	'on_cmd' : [
	\		'AdroneHome',
	\		'AdroneSay',
	\		'AdroneVersion'
	\	]
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
	\	'on_cmd' : [
	\		'VimHelpGenerator',
	\		'VimHelpGeneratorVirtual',
	\		'HelpIntoMarkdown'
	\	]
	\})
	call neobundle#untap()
endif
if neobundle#tap('vim-ruby')
	"@Bugs('rspec.vim do not highlight syntax before loading vim-ruby')
	"@Marked(' -> was fixed by commit a516 ?')
	call neobundle#config('vim-ruby', {
	\	'on_ft' : 'ruby'
	\})
	call neobundle#untap()
endif
if neobundle#tap('rspec.vim')
	call neobundle#config('rspec.vim', {
	\	'on_ft' : 'ruby'
	\})
	call neobundle#untap()
endif
if neobundle#tap('mdforvim')
	call neobundle#config('mdforvim', {
	\	'on_cmd' : [
	\		'MdConvert',
	\		'MdPreview',
	\		'MdStopPreview', {
	\			'name'     : 'MdSaveAs',
	\			'complete' : 'file'
	\		}
	\	]
	\})
	call neobundle#untap()
endif
if neobundle#tap('vimconsole.vim')
	call neobundle#config('vimconsole.vim', {
	\	'on_cmd' : [
	\		'VimConsoleLog',
	\		'VimConsoleOpen',
	\		'VimConsoleClose',
	\		'VimConsoleToggle',
	\		'VimConsoleClear',
	\		'VimConsoleRedraw'
	\	]
	\})
	call neobundle#untap()
endif
if neobundle#tap('vim-go')
	call neobundle#config('vim-go', {
	\	'on_ft' : 'go'
	\})
	call neobundle#untap()
endif
if neobundle#tap('vim-textobj-indent')
	call neobundle#config('vim-textobj-indent', {
	\	'depends' : 'vim-textobj-user'
	\})
	call neobundle#untap()
endif
if neobundle#tap('neocomplete.vim')
	call neobundle#config('neocomplete.vim', {
	\	'disabled' : has('nvim')
	\})
	call neobundle#untap()
endif
if neobundle#tap('deoplete.nvim')
	call neobundle#config('deoplete.nvim', {
	\	'disabled' : !has('nvim')
	\})
	call neobundle#untap()
endif
if neobundle#tap('auto-ctags.vim')
	call neobundle#config('auto-ctags.vim', {
	\	'on_cmd'            : 'Ctags',
	\	'external_commands' : 'ctags'
	\})
	call neobundle#untap()
endif
if neobundle#tap('unite-tag')
	call neobundle#config('unite-tag', {
	\	'depends' : 'Shougo/unite.vim'
	\})
	call neobundle#untap()
endif
if neobundle#tap('vim-textobj-from_regexp')
	call neobundle#config('vim-textobj-from_regexp', {
	\	'depends' : 'vim-textobj-user'
	\})
	call neobundle#untap()
endif
if neobundle#tap('typescript-vim')
	call neobundle#config('typescript-vim', {
	\	'on_ft' : 'typescript'
	\})
	call neobundle#untap()
endif
if neobundle#tap('vim-pager')
	call neobundle#config('vim-pager', {
	\	'on_cmd' : 'PAGER'
	\})
	call neobundle#untap()
endif
if neobundle#tap('vim-manpager')
	call neobundle#config('vim-manpager', {
	\	'on_cmd' : [
	\		'Man',
	\		'MANPAGER'
	\	]
	\})
	call neobundle#untap()
endif
if neobundle#tap('vim-fugitive')
	call neobundle#config('vim-fugitive', {
	\	'augroup' : 'fugitive'
	\})
	call neobundle#untap()
endif
if neobundle#tap('try-colorscheme.vim')
	call neobundle#config('try-colorscheme.vim', {
	\	'on_cmd' : 'TryColorscheme'
	\})
	call neobundle#untap()
endif
if neobundle#tap('unite-syntax')
	call neobundle#config('unite-syntax', {
	\	'depends'  : 'Shougo/unite.vim'
	\})
	call neobundle#untap()
endif
if neobundle#tap('unite-help')
	call neobundle#config('unite-help', {
	\	'depends' : 'Shougo/unite.vim',
	\})
	call neobundle#untap()
endif
if neobundle#tap('unite-filetype')
	call neobundle#config('unite-filetype', {
	\	'depends' : 'Shougo/unite.vim'
	\})
	call neobundle#untap()
endif
if neobundle#tap('agit.vim')
	call neobundle#config('agit.vim', {
	\	'on_cmd' : [
	\		'Agit',
	\		'AgitFile',
	\		'AgitGit',
	\		'AgitDiff'
	\	]
	\})
	call neobundle#untap()
endif
if neobundle#tap('unite-session')
	call neobundle#config('unite-session', {
	\	'depends'  : 'Shougo/unite.vim',
	\	'on_cmd' : {
	\		'name'     : ['UniteSessionSave', 'UniteSessionLoad'],
	\		'complete' : 'customlist,unite#sources#session#_complete'
	\	}
	\})
	call neobundle#untap()
endif
if neobundle#tap('vim-textobj-xmlattr')
	call neobundle#config('vim-textobj-xmlattr', {
	\	'depends' : 'vim-textobj-user',
	\	'on_ft'   : [
	\		'html',
	\		'xml',
	\		'xaml',
	\		'fxml'
	\	]
	\})
	call neobundle#untap()
endif
if neobundle#tap('neco-tweetvim')
	call neobundle#config('neco-tweetvim', {
	\	'on_source' : 'TweetVim'
	\})
	call neobundle#untap()
endif
if neobundle#tap('unite-tweetvim')
	call neobundle#config('unite-tweetvim', {
	\	'on_source' : 'TweetVim'
	\})
	call neobundle#untap()
endif
if neobundle#tap('vim-haskell-indent')
	call neobundle#config('vim-haskell-indent', {
	\	'on_ft' : 'haskell'
	\})
	call neobundle#untap()
endif
if neobundle#tap('repl.vim')
	call neobundle#config('repl.vim', {
	\	'depends' : [
	\		'Shougo/vimshell.vim',
	\		'Shougo/vimproc.vim'
	\	],
	\	'on_cmd' : 'Repl',
	\	'on_map' : '<Plug>(repl-run)'
	\})
	call neobundle#untap()
endif
if neobundle#tap('emmet-vim')
	call neobundle#config('emmet-vim', {
	\	'on_cmd' : 'EmmetInstall',
	\	'on_map' : get(g:, 'user_emmet_leader_key', '\\')
	\})
	call neobundle#untap()
endif
if neobundle#tap('vim-operator-flashy')
	call neobundle#config('vim-operator-flashy', {
	\	'depends' : 'vim-operator-user'
	\})
	call neobundle#untap()
endif

call neobundle#end()

" }}}


"------------------------"
"*** Plugin_Configure ***"
"------------------------"
"--- netrw --- {{{

" Enable netrw previewing
let g:netrw_preview = 1

" Set directory for .netwhist and .netrwbook
let g:netrw_home = g:vimrc['vim_home']

" Set default options for opening netrw
let g:netrw_bufsettings = 'relativenumber readonly nomodifiable nomodified nowrap nobuflisted'

" }}}
"--- unite.vim --- {{{

"@Code(':Unite javasrc')
" If you want to use this, you must extract JDK's src.zip here
let s:java_src = printf('%s/resource/Java/src', g:vimrc['vim_home'])
if filereadable(s:java_src)
	let g:unite_source_alias_aliases.javasrc = {
	\	'source' : 'file_rec',
	\	'args'   : s:java_src
	\}
endif
unlet s:java_src

" }}}
"--- vim-quickrun --- {{{

" Define myself, for lazy load
let g:quickrun_no_default_key_mappings = 0

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
\	},
\	'html': {
\		'outputter' : 'null',
\		'exec'      : '%c %s:p'
\	},
\	'tex' : {
\		'command' : 'ptex2pdf',
\		'cmdopt'  : '-l',
\		'exec'    : '%c %o %s:r'
\	}
\}

" Set by each environment
if g:vimrc['is_unix'] && !g:vimrc['is_cygwin']
	call vimrc#plugins#append_config_quickrun_unix()
elseif g:vimrc['is_windows']
	call vimrc#plugins#append_config_quickrun_windows()
elseif g:vimrc['is_cygwin']
	call vimrc#plugins#append_config_quickrun_cygwin()
endif

" }}}
"--- TweetVim --- {{{

" Do smooth posting tweet
let g:tweetvim_async_post = 1

" }}}
"--- vimshell.vim --- {{{

let g:vimshell_no_save_history_commands = {
\	'history': 1,
\	'ls'     : 1,
\	'clear'  : 1
\}
let g:vimshell_enable_transient_user_prompt = 1
let g:vimshell_max_command_history          = 10000
let g:vimshell_scrollback_limit             = 10000
let g:vimshell_split_command                = 'split'

" Use current directory as vimshell prompt
let g:vimshell_prompt_expr    = 'escape(fnamemodify(getcwd(), ":~") . "%", "\\[]()?! ") . " "'
let g:vimshell_prompt_pattern = '^\%(\f\|\\.\)\+% '

"@See('autoload/vimshell/commands/{hereis,edit_places,places,reload_places}.vim')
let g:vimshell_hereis_alias_prefix = 'p_'

" }}}
"--- vimshell-kawaii.vim --- {{{

" vimshell is kawaii
let g:vimshell_kawaii_smiley = 1

" }}}
"--- w3m.vim --- {{{

let g:w3m#external_browser = 'firefox'

let g:w3m#homepage = 'http://www.google.co.jp/'

" }}}
"--- foldCC --- {{{

let g:foldCCtext_maxchars = 120

" }}}
"--- vim-submode --- {{{

let g:submode_timeout = 0

if neobundle#tap('vim-submode')
	augroup PluginPrefs
		" Window Resizer
		autocmd User MyVimRc call submode#enter_with('window_resize', 'n', '', '<C-s>w')
		autocmd User MyVimRc call submode#map('window_resize', 'n', '', 'j', '<C-w>+')
		autocmd User MyVimRc call submode#map('window_resize', 'n', '', 'k', '<C-w>-')
		autocmd User MyVimRc call submode#map('window_resize', 'n', '', 'h', '<C-w><')
		autocmd User MyVimRc call submode#map('window_resize', 'n', '', 'l', '<C-w>>')

		" Fold Mover
		autocmd User MyVimRc call submode#enter_with('fold_move', 'n', '', '<C-s>z')
		autocmd User MyVimRc call submode#map('fold_move', 'n', 'e', 'j', 'foldlevel(".") > 0 ? "zczjzozz"   : "zjzozz"')
		autocmd User MyVimRc call submode#map('fold_move', 'n', 'e', 'k', 'foldlevel(".") > 0 ? "zczkzo[zzz" : "zkzo[zzz"')
		autocmd User MyVimRc call submode#map('fold_move', 'n', '',  'h', '[z')
		autocmd User MyVimRc call submode#map('fold_move', 'n', '',  'l', ']z')

		" Buffer Changer
		autocmd User MyVimRc call submode#enter_with('buffer_change', 'n', '', '<C-s>b')
		autocmd User MyVimRc call submode#map('buffer_change', 'n', 's', 'n', ':bnext<CR>')
		autocmd User MyVimRc call submode#map('buffer_change', 'n', 's', 'p', ':bprevious<CR>')

		" Linewise Mover
		autocmd User MyVimRc call submode#enter_with('linewise_move', 'n', '', 'gk', 'gk')
		autocmd User MyVimRc call submode#enter_with('linewise_move', 'n', '', 'gj', 'gj')
		autocmd User MyVimRc call submode#enter_with('linewise_move', 'n', '', 'g0', 'g0')
		autocmd User MyVimRc call submode#enter_with('linewise_move', 'n', '', 'g^', 'g^')
		autocmd User MyVimRc call submode#enter_with('linewise_move', 'n', '', 'g$', 'g$')
		autocmd User MyVimRc call submode#map('linewise_move', 'n', '', 'k',  'gk')
		autocmd User MyVimRc call submode#map('linewise_move', 'n', '', 'j',  'gj')
		autocmd User MyVimRc call submode#map('linewise_move', 'n', '', '0',  'g0')
		autocmd User MyVimRc call submode#map('linewise_move', 'n', '', '^',  'g^')
		autocmd User MyVimRc call submode#map('linewise_move', 'n', '', '$',  'g$')
		autocmd User MyVimRc call submode#map('linewise_move', 'n', '', '_',  'g^')
		autocmd User MyVimRc call submode#map('linewise_move', 'n', '', 'g_', 'g$')
		autocmd User MyVimRc call submode#map('linewise_move', 'n', '', 'h',  'h')
		autocmd User MyVimRc call submode#map('linewise_move', 'n', '', 'l',  'l')
	augroup END
endif

" }}}
"--- ref-dicts-en --- {{{
"@See('http://d.hatena.ne.jp/akishin999/20131024/1382569289')

let g:ref_source_webdict_sites = {
\	'weblio' : {
\		'url' : 'http://ejje.weblio.jp/content/%s'
\	}
\}

let g:ref_source_webdict_sites['default'] = 'weblio'

let g:ref_source_webdict_sites['weblio'].filter = function('vimrc#plugins#weblio_filter')

" }}}
"--- restart.vim --- {{{

let g:restart_sessionoptions = 'blank,curdir,folds,help,localoptions,tabpages'

" }}}
"--- vimdoc-ja --- {{{

" vimdoc-ja is secondary order
set helplang=en,ja

" }}}
"--- TaskList.vim --- {{{

" TaskList search these
let g:tlTokenList = ['NOTE', 'TODO', 'FIXME', 'XXX']

" Open window at bottom
let g:tlWindowPosition = 1

" Restore opened position when closed TaskList
let g:tlRememberPosition = 1

" }}}
"--- adrone.vim --- {{{

let g:adrone_home_default_keymappings = 0

" }}}
"--- vim-indent-guides --- {{{

let g:indent_guides_default_mapping = 0
let g:indent_guides_guide_size      = 1
let g:indent_guides_auto_colors     = 0

" Define guide colors
augroup HighlightPref
	autocmd VimEnter,ColorScheme * highlight default link IndentGuidesOdd  PmenuSel
	autocmd VimEnter,ColorScheme * highlight default link IndentGuidesEven Pmenu
augroup END

" Unify indent-guides available
"@See('nnoremap <C-h><C-i>')
let g:vimrc#keys#indent_guides_enable = get(g:, 'vimrc#keys#indent_guides_enable', 1)
augroup FileEvent
	autocmd WinEnter,BufWinEnter * IndentGuidesDisable
	autocmd WinEnter,BufWinEnter *.{xml,html,css,scss,erb,xaml,fxml}
	\	if g:vimrc#keys#indent_guides_enable
	\|		IndentGuidesEnable
	\|	endif
augroup END

" }}}
"--- vim-colors-solarized --- {{{

let g:solarized_contrast = 'high'

" }}}
"--- aho-bakaup.vim --- {{{

" Devolute to bakaup
set nobackup

" The file was backed up automatically when you written that file
let g:bakaup_backup_dir  = s:backupdir
let g:bakaup_auto_backup = 1

" }}}
"--- neosnippet.vim --- {{{

" for :NeoSnippetEdit
let g:neosnippet#snippets_directory = g:vimrc['vim_home'] . '/neosnippets'

" Disable default select-mode mapping
let g:neosnippet#disable_select_select_mappings = 1

" }}}
"--- separetaro.vim --- {{{

let g:separetaro_short_separator_of = {
\	'vim'        : '"----------"',
\	'java'       : '/* -=-=-=-=-=-=-=-=- */',
\	'javascript' : '/* -=-=-=-=-=-=-=-=- */',
\	'typescript' : '/* -=-=-=-=-=-=-=-=- */',
\	'css'        : '/* -=-=-=-=-=-=-=-=- */',
\	'php'        : '/* -=-=-=-=-=-=-=-=- */',
\	'go'         : '/* -=-=-=-=-=-=-=-=- */',
\	'html'       : '<!-- - - - -->',
\	'markdown'   : '- - -',
\	'fxml'       : '<!-- - - - -->',
\	'xaml'       : '<!-- - - - -->'
\}

let g:separetaro_long_separator_of = {
\	'vim'        : '"-------------------"',
\	'java'       : '/* --- --- --- _ --- --- --- */',
\	'javascript' : '/* --- --- --- _ --- --- --- */',
\	'typescript' : '/* --- --- --- _ --- --- --- */',
\	'css'        : '/* --- --- --- _ --- --- --- */',
\	'php'        : '/* --- --- --- _ --- --- --- */',
\	'go'         : '/* --- --- --- _ --- --- --- */',
\	'html'       : '<!-- - - - _ - - - -->',
\	'markdown'   : '- - - - -',
\	'fxml'       : '<!-- - - - _ - - - -->',
\	'xaml'       : '<!-- - - - _ - - - -->'
\}

" }}}
"--- vimconsole.vim --- {{{

let g:vimconsole#auto_redraw             = 1
let g:vimconsole#no_default_key_mappings = 1

" }}}
"--- vim-go --- {{{

" Avoid a bug on cygwin environment
if g:vimrc['is_cygwin']
	let g:go_fmt_autosave        = 0
	let g:go_def_mapping_enabled = 0
endif

" }}}
"--- vim-textobj-indent --- {{{

let g:textobj_indent_no_default_key_mappings = 1

" }}}
"--- neocomplete.vim --- {{{

" Start neocomplete automatically
let g:neocomplete#enable_at_startup = 1

" Disable on
let g:neocomplete#sources = {
\	'int-ghci'  : [],
\	'int-stack' : []
\}

" }}}
"--- auto-ctags.vim --- {{{

" Specific the ctags generated directory ( Must sync 'tags' )
let g:auto_ctags_directory_list = [
\	'.git',             '.tags',
\	'../.git',          '../.tags',
\	'../../.git',       '../../.tags',
\	'../../../.git',    '../../../.tags',
\	'../../../../.git', '../../.../../.tags'
\]

" Avoid hyper gravity
let g:auto_ctags_tags_args = '--tag-relative --recurse --sort=yes'
\                          . ' --exclude=node_modules'
\                          . ' --exclude=bower_components'

" }}}
"--- unite-tag --- {{{

" Fully showing name
let g:unite_source_tag_max_name_length  = 100
let g:unite_source_tag_max_fname_length = 100

" }}}
"--- vim-visualstar --- {{{

" Do zzzv after execute visualstar
let g:visualstar_extra_commands = 'zzzv'

" }}}
"--- submode-window_move.vim --- {{{

" Register mode starting keymapping
let g:submode_window_move = {}
let g:submode_window_move['start_tab_move']                   = '<C-s>t'
let g:submode_window_move['start_window_move_with_move_next'] = '<C-s>N'
let g:submode_window_move['start_window_move_with_move_prev'] = '<C-s>P'

" }}}
"--- repl.vim --- {{{

" Use this repl
let g:repl_filetype_repl = {
\	'haskel' : {
\		'repl' : 'stack exec -- ghci',
\		'opt'  : ''
\	},
\	'python' : {
\		'repl' : 'python2',
\		'opt'  : '-i'
\	}
\}

let g:repl_no_default_keymappings = 1
let g:repl_split_command          = 'vertical split'

" }}}
"--- emmet-vim --- {{{

" Set prefix key
let g:user_emmet_leader_key = '\\'

" Enable these filetype
autocmd UserEvent FileType html,xml,fxml EmmetInstall


" }}}
"--- For Develop --- {{{

" Local my plugins
let s:repos = [ 'adrone.vim'
\             , 'aho-bakaup.vim'
\             , 'auto-ctags.vim'
\             , 'submode-window_move.vim'
\             , 'repl.vim'
\             ]
let s:repos_dir = '~/Repository/'

" If valid local plugin, disable bundled same plugin
for s:plug in s:repos
	let s:plug_dir = s:repos_dir . s:plug
	if isdirectory(expand(s:plug_dir))
		execute 'set runtimepath+=' . s:plug_dir
		silent! execute 'NeoBundleDisable' s:plug
	end
endfor
unlet s:plug_dir s:plug s:repos_dir s:repos

" }}}
"--- For Private --- {{{

" Load private configure
if filereadable(expand('~/.vimrc_private'))
	source ~/.vimrc_private
endif

" }}}


"-------------------------"
"      View_Setting       "
"-------------------------"
" {{{

" Set basic preferences
set number relativenumber nowrap hlsearch list scrolloff=16
set listchars=tab:»_,trail:_,extends:»,precedes:«,nbsp:%,eol:↲

" Status bar was always displayed
set laststatus=2

" Set status bar format
let s:statusline_left  = '[Fenc=%{&fileencoding}]'
\                      . '[Enc=%{&encoding}]'
\                      . '%{vimrc#set#tag_load_status()}'
let s:statusline_right = '%1*%F(%n)%*'
\                      . '%2*%m%*'
\                      . '%3*%r%*'
\                      . '%4*[FT=%y]%*'
\                      . '[%03v]'
let &statusline        = s:statusline_left . '%=' . s:statusline_right
unlet s:statusline_left s:statusline_right

" ☆ Fix 2byte code viewin
" (Not support gnome-terminal)
set ambiwidth=double

" Define powered up syntax highlights
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
	"autocmd ColorScheme * highlight CursorLine   cterm=underline ctermfg=Cyan

	" StatusLine specified highlight
	autocmd ColorScheme * highlight User1 cterm=standout ctermfg=Black  ctermbg=White
	autocmd ColorScheme * highlight User2 cterm=standout ctermfg=Yellow     ctermbg=Black
	autocmd ColorScheme * highlight User3 cterm=standout ctermfg=DarkYellow ctermbg=Black
	autocmd ColorScheme * highlight User4 cterm=standout ctermfg=Gray      ctermbg=Black
augroup END

augroup HighlightPref
	autocmd ColorScheme       * highlight RcEmSpace ctermbg=LightBlue
	autocmd VimEnter,WinEnter * call matchadd('RcEmSpace', '　')
	" Highlight VCS conflict markers
	autocmd ColorScheme * call matchadd('Error', '^\(<\|=\|>\)\{7\}\([^=].\+\)\?$')
augroup END


augroup HighlightPref
	autocmd InsertEnter * highlight StatusLine ctermfg=Black ctermbg=Cyan
	autocmd InsertLeave * highlight StatusLine ctermfg=Cyan  ctermbg=Black
augroup END

" }}}

" Set for color scheme only once
if !g:vimrc['loaded']
	set background=dark
	colorscheme desert
endif

" Wrapped text was appended indent on the window
set breakindent linebreak

" View more info on <C-g>
set noruler

" Always show tabline
set showtabline=2

" Sugoi view tabline
set tabline=%!vimrc#set#with_delimitter_tab_line()

" Turn off highlight
nohlsearch

" Show two line
set cmdheight=2

" }}}


"-------------------------"
"     Action_Setting      "
"-------------------------"
" {{{

" Backspace can delete carriage returns
set backspace=indent,eol,start

" Don't insert carriage return automatically, and set tab styles
set textwidth=0 tabstop=4 shiftwidth=4

" Set c type auto indent
set autoindent cindent

" Searcher don't jump to top or bottom
set nowrapscan

" Set fold options
set foldmethod=marker
set foldtext=FoldCCtext()
set foldcolumn=1
let &fillchars = 'vert:|,fold: '
set foldopen=search,jump,mark,percent,insert,tag,undo
set foldclose=all

" Collection swap file to here
let &directory = s:directory

" Save view position when execute ':mkview'
let &viewdir = s:viewdir

" Hold undo archive when file closed
set undofile
let &undodir = s:undodir

" Bell sound is instead of screen flash.
set visualbell

"@Bugs("This option has not functioned (?)")
" Disable auto commentalize new line
set formatoptions-=ro

" Ignore case on NormalMode searching and InsertMode completion
set ignorecase noinfercase

" No timeout key maps
set notimeout

" Do not set file name order priority on c-mode completion
set suffixes=

" Don't put two space on join (normal J)
set nojoinspaces

" Control IME by myself
set iminsert=0

" Open .tex as LaTex
let g:tex_flavor = 'latex'

" }}}


"-------------------------"
"     Inner_Setting       "
"-------------------------"
" {{{

" Set order of judging file encode
set fileencodings=ucs-bom,utf-8,sjis,euc-jp,cp932,iso-2022-jp-3,iso-2022-jp,eucjp-ms,euc-jisx0213,ucs-bom,latin1,default

" Leaving a history and it limit is a 500 pieces
set history=500

" Display command complement
set wildmenu

" Path delimiter is slash
set shellslash

" Add correspond pairs
set matchpairs+=<:>

" Reference tags of ctags
let s:pre_tags = map(g:auto_ctags_directory_list, 'v:val . "/tags"')
let &tags      = join(s:pre_tags + ['./tags', '~/tags'], ',')
unlet s:pre_tags

" Netrw wake up default dir
set browsedir=buffer

" Set spell lang
set spelllang=en_US,cjk

" Set reference path, using by :find, gf and more
set path=.,,./**

" Auto generate my help tags
if isdirectory(g:vimrc['vim_home'] . '/doc')
	execute 'helptags' (g:vimrc['vim_home'] . '/doc')
endif

" }}}


"-------------------------"
"      Event_Method       "
"-------------------------"
" {{{

augroup FileEvent
	" Auto set cursor position in the file
	autocmd BufReadPost * call vimrc#set#visit_past_position()

	" Auto load filetype dictionary
	autocmd FileType *
	\	if filereadable(printf('%s/dict/filetype/%s.dict', g:vimrc['vim_home'], &filetype))
	\|		execute 'setl dict+=' . printf('%s/dict/filetype/%s.dict', g:vimrc['vim_home'], &filetype)
	\|	endif
augroup END

"" Foooooo!!!!!!! I hope get this omoshiro event!!
"autocmd UserEvent UserGettingBored * echo 'oooooiiiii!!!!!'

" RelativeNumber is used current window only
augroup UserEvent
	autocmd BufEnter,WinEnter * if &number | setl relativenumber | end
	autocmd BufLeave,Winleave * setl norelativenumber
augroup END

" Hide relativenumber when OverCommandLine entered
augroup UserEvent
	autocmd User OverCmdLineEnter setl norelativenumber
	autocmd User OverCmdLineLeave if &number | setl relativenumber | end
augroup END

" }}}


"-------------------------"
"      Command_Util       "
"-------------------------"
" Alternate {{{

call altercmd#load()

" buffer open commands with filetype 'none'
command! -bar -bang NewOverridden new<bang> | setf none
AlterCommand new NewOverridden

command! -bar -bang -complete=file -nargs=? EditOverridden e<bang> <args> | if empty(&ft) | setf none | endif
AlterCommand e[dit] EditOverridden

command! -bar -bang -complete=file -nargs=? VnewOverridden vnew<bang> <args> | if empty(&ft) | setf none | endif
AlterCommand vne[w] VnewOverridden

command! -bar -bang -complete=file -nargs=? EnewOverridden enew<bang> <args> | if empty(&ft) | setf none | endif
AlterCommand ene[w] EnewOverridden

command! -bar -bang -complete=file -nargs=? TabnewOverridden tabnew<bang> <args> | if empty(&ft) | setf none | endif
AlterCommand tabnew TabnewOverridden

" }}}
" Util {{{

" Grep current buffer, and open quickfix window
command! -bar -nargs=1 GrepInThis vimgrep <args> % | cwindow

" Reverse ranged lines
command! -range=% ReverseLine :<line1>, <line2>call vimrc#cmd#reverse_line()

" Rename the file of current buffer
command! -bar -nargs=1 -complete=file Rename call vimrc#cmd#rename_to(<q-args>)

"@Bugs(':RedirToVar @" highlight  " happend exception')
" Substitute result to a variable easily
command! -bar -nargs=1 -bang -complete=command RedirToVar call vimrc#cmd#redir_to_var(<bang>0, <q-args>)

" Count selected line num
command! -bar -range Count :echomsg (<line2> - <line1> + 1)

" }}}
" Helper {{{

" Prepare functions & commands {{{

" Define cnoreabbr with cmd completion
command! -nargs=+ CmdCnoreabbr call vimrc#cmd#cmd_cnoreabbr(<f-args>)

" }}}
" Vim Utils {{{

command! -bar VimConfig    e $MYVIMRC
command! -bar VimConfigTab tabnew $MYVIMRC

command! -bar Reload so $MYVIMRC
\|	if has('gui_running') && filereadable($MYGVIMRC)
\|		so $MYGVIMRC
\|	endif

cnoreabbr    w!! w !sudo tee % > /dev/null
CmdCnoreabbr CdBufDir     cd %:p:h
CmdCnoreabbr ColorPreview Unite colorscheme -auto-preview

command! -bar -nargs=? -complete=filetype FtpluginEditAfter
\	execute ':edit' printf('%s/after/ftplugin/%s.vim', g:vimrc['vim_home'], (empty(<q-args>) ? &filetype : <q-args>))

command! -bar -nargs=? -complete=filetype FtDictionaryEdit
\	execute ':edit' printf('%s/dict/filetype/%s.dict', g:vimrc['vim_home'], (empty(<q-args>) ? &filetype : <q-args>))

command! -bar -nargs=? -complete=filetype SyntaxEdit
\	execute ':edit' printf('%s/syntax/%s.vim', g:vimrc['vim_home'], (empty(<q-args>) ? &filetype : <q-args>))

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
command! -bar TwitterPrivate     call vimrc#plugins#twitter_private()
command! -bar TwitterPrivateTab  tabnew | TwitterPrivate
command! -bar TweetPrivate       call vimrc#plugins#tweet_private()

"-- Public Account --"
command! -bar TwitterPublic      call vimrc#plugins#twitter_public()
command! -bar TwitterPublicTab   tabnew | TwitterPublic
command! -bar TweetPublic        call vimrc#plugins#tweet_public()

CmdCnoreabbr Bitly         TweetVimBitly
CmdCnoreabbr SwitchAccount TweetVimSwitchAccount
CmdCnoreabbr UserTimeline  TweetVimUserTimeline
CmdCnoreabbr TweetVimNote  TabnewOverridden ~/.tmp/tweetvim_note.md \| set syntax=tweetvim_say

" }}}

" To Service Name
CmdCnoreabbr Lingr J6uil
CmdCnoreabbr LingrTab TabnewOverridden \| J6uil

" Translates Languages
CmdCnoreabbr Weblio    Ref webdict weblio
cnoreabbr    weblio    Ref webdict weblio
CmdCnoreabbr WeblioTab TabnewOverridden \| Ref webdict weblio

" NeoBundle install with showing log
CmdCnoreabbr NeoBundleInstallL NeoBundleInstall \| NeoBundleLog
CmdCnoreabbr NeoBundleUpdateL  NeoBundleUpdate \| NeoBundleUpdatesLog

" Remove prefix
CmdCnoreabbr SessionSave UniteSessionSave
CmdCnoreabbr SessionLoad UniteSessionLoad

" Save session and specify session name automatically
command! SessionSaveInGitBranch call vimrc#cmd#git_branch_session_save()

" Aliases
CmdCnoreabbr JazzradioList Unite jazzradio
CmdCnoreabbr ManTab        TabnewOverridden \| Man

" }}}
" Development {{{

" vimconsole.vim
CmdCnoreabbr Log      VimConsoleLog
CmdCnoreabbr LogClear VimConsoleClear
CmdCnoreabbr LogOpen  VimConsoleOpen

" GHCi
let s:ghci_command = executable('stack') ? 'stack ghci' : 'ghci'
execute 'CmdCnoreabbr Ghci'    'VimShellInteractive'                    s:ghci_command
execute 'CmdCnoreabbr Sghci'   'VimShellInteractive' '--split="sp"'     s:ghci_command
execute 'CmdCnoreabbr Vghci'   'VimShellInteractive' '--split="vsp"'    s:ghci_command
execute 'CmdCnoreabbr GhciTab' 'VimShellInteractive' '--split="tabnew"' s:ghci_command
CmdCnoreabbr Hoogle  Ref hoogle
unlet s:ghci_command

" js
CmdCnoreabbr Js    VimShellInteractive js
CmdCnoreabbr Sjs   VimShellInteractive --split='sp' js
CmdCnoreabbr Vjs   VimShellInteractive --split='vsp' js
CmdCnoreabbr JsTab VimShellInteractive --split='tabnew' js

" irb
CmdCnoreabbr Irb    VimShellInteractive irb
CmdCnoreabbr Sirb   VimShellInteractive --split='sp' irb
CmdCnoreabbr Virb   VimShellInteractive --split='vsp' irb
CmdCnoreabbr IrbTab VimShellInteractive --split='tabnew' irb

" }}}


"-------------------------"
"       Key_Mapping       "
"-------------------------"
" Disable {{{

augroup KeyMapping
	" Enable some hoge<C-c> mappings
	autocmd User MyVimRc nnoremap <C-c>      <NOP>
	autocmd User MyVimRc nnoremap <C-c><C-c> <C-c>

	autocmd User MyVimRc nnoremap <Up>    <NOP>
	autocmd User MyVimRc nnoremap <Down>  <NOP>
	autocmd User MyVimRc nnoremap <Left>  <NOP>
	autocmd User MyVimRc nnoremap <Right> <NOP>

	" Cancel <C-w>foo
	autocmd User MyVimRc nnoremap <C-w><C-l> <NOP>

	autocmd User MyVimRc inoremap <Up>    <NOP>
	autocmd User MyVimRc inoremap <Down>  <NOP>
	autocmd User MyVimRc inoremap <Left>  <NOP>
	autocmd User MyVimRc inoremap <Right> <NOP>

	autocmd User MyVimRc cnoremap <Left>  <NOP>
	autocmd User MyVimRc cnoremap <Right> <NOP>

	autocmd User MyVimRc cnoremap [Left] <Left>
augroup END

" }}}
" Global {{{

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

    autocmd User MyVimRc vnoremap zo zogv
    autocmd User MyVimRc vnoremap zO zOgv
augroup END

" }}}
" Windows and Buffers {{{

augroup KeyMapping
	autocmd User MyVimRc nnoremap <Space>h <C-w>h
	autocmd User MyVimRc nnoremap <Space>j <C-w>j
	autocmd User MyVimRc nnoremap <Space>k <C-w>k
	autocmd User MyVimRc nnoremap <Space>l <C-w>l

	autocmd User MyVimRc nnoremap <C-w><C-r> <C-w>r<C-w>p

	autocmd User MyVimRc nnoremap <silent> <C-w>T :<C-u>tabclose<CR>
	autocmd User MyVimRc nnoremap <silent> <C-w>c :<C-u>bdelete<CR>
	autocmd User MyVimRc nnoremap <silent> <C-w>C :<C-u>bdelete!<CR>
	autocmd User MyVimRc nnoremap <silent> <C-w>N :<C-u>EnewOverridden!<CR>
	autocmd User MyVimRc nnoremap <silent> <C-w>Q :<C-u>quitall<CR>
	autocmd User MyVimRc nnoremap <silent> <C-w>" :<C-u>resize 5<CR>
	autocmd User MyVimRc nnoremap <silent> <C-w>\ :<C-u>resize 0<CR>
	autocmd User MyVimRc nnoremap <silent> <C-w>~ :<C-u>vertical resize 0<CR>

	autocmd User MyVimRc nnoremap <silent><expr> <C-w>t  ('mZ:tabnew<CR>`Zzz'          . (foldlevel('.') > 0 ? 'zo' : ''))
	autocmd User MyVimRc nnoremap <silent><expr> g<C-w>t ('mZ:hide<CR>:tabnew<CR>`Zzz' . (foldlevel('.') > 0 ? 'zo' : ''))
augroup END

" }}}
" Options Toggling  {{{

augroup KeyMapping
	" All
	autocmd User MyVimRc nnoremap <silent> <C-h>E     :<C-u>call vimrc#keys#motionless_bufdo('set expandtab')<CR>:echo 'set expandtab all buffer!'<CR>
	autocmd User MyVimRc nnoremap <silent> <C-h><C-e> :<C-u>call vimrc#keys#motionless_bufdo('set expandtab!')<CR>:set expandtab?<CR>
	autocmd User MyVimRc inoremap <silent> <C-k><C-e> <C-o>:call vimrc#keys#motionless_bufdo('set expandtab!')<CR><C-o>:set expandtab?<CR>

	" Local
	autocmd User MyVimRc nnoremap <silent>       <C-h><C-f> :<C-u>call vimrc#keys#toggle_foldmethod()<CR>
	autocmd User MyVimRc nnoremap <silent>       <C-h><C-d> :<C-u>call vimrc#keys#toggle_diff()<CR>
	autocmd User MyVimRc nnoremap <silent><expr> <C-h><C-v> ':setl virtualedit=' . (&virtualedit ==# '' ? 'all' : '') . ' virtualedit?<CR>'

	autocmd User MyVimRc nnoremap <silent> <C-h><C-w> :<C-u>setl wrap!           wrap?          <CR>
	autocmd User MyVimRc nnoremap <silent> <C-h><C-c> :<C-u>setl cursorline!     cursorline?    <CR>
	autocmd User MyVimRc nnoremap <silent> <C-h><C-r> :<C-u>setl relativenumber! relativenumber?<CR>
	autocmd User MyVimRc nnoremap <silent> <C-h><C-l> :<C-u>setl list!           list?          <CR>
	autocmd User MyVimRc nnoremap <silent> <C-h><C-n> :<C-u>setl number!         number?        <CR>
	autocmd User MyVimRc nnoremap <silent> <C-h><C-s> :<C-u>setl wrapscan!       wrapscan?      <CR>

	autocmd User MyVimRc inoremap <silent> <C-k><C-w> <C-o>:setl wrap!      wrap?<CR>

	autocmd User MyVimRc vnoremap <silent><expr> <C-h><C-v> '<Esc>:setl virtualedit=' . (&virtualedit ==# '' ? 'all' : '') . ' virtualedit?<CR>gv'
augroup END

" }}}
" Plugins {{{

augroup KeyMapping
	" netrw
	autocmd User MyVimRc nnoremap <silent> <leader>e         :<C-u>call vimrc#keys#toggle_netrw_vexplorer()<CR>
	autocmd User MyVimRc nnoremap <silent> <leader><leader>e :<C-u>Sexplore<CR>
	autocmd User MyVimRc nnoremap <silent> <leader>E         :<C-u>Explore<CR>
	autocmd User MyVimRc nnoremap <silent> <leader><leader>E :<C-u>Texplore<CR>

	" open-browser.vim
	autocmd User MyVimRc nmap <leader>w <Plug>(openbrowser-open)
	autocmd User MyVimRc vmap <leader>w <Plug>(openbrowser-open)

	" vim-quickrun
	autocmd User MyVimRc nmap              <leader>r         <Plug>(quickrun)
	autocmd User MyVimRc nnoremap <silent> <leader>R         :<C-u>QuickRun -runner shell<CR>
	autocmd User MyVimRc nnoremap <silent> <leader><leader>r :<C-u>call vimrc#keys#bufclose_filetype('quickrun')<CR>
	autocmd User MyVimRc vmap              <leader>r         <Plug>(quickrun)
	autocmd User MyVimRc vnoremap <silent> <leader>R         :QuickRun -runner shell<CR>

	" vimshell
	autocmd User MyVimRc nnoremap <silent> <leader>v         :<C-u>VimShell -split-command=vsp -toggle<CR>
	autocmd User MyVimRc nnoremap <silent> <leader><leader>v :<C-u>VimShell -split-command=sp  -toggle<CR>
	autocmd User MyVimRc nnoremap <silent> <leader>V         :<C-u>VimShellBufferDir -create<CR>
	autocmd User MyVimRc nnoremap <silent> <leader><leader>V :<C-u>VimShell -split-command=tabnew -create<CR>

	" Unite
	autocmd User MyVimRc nnoremap          <leader>U         :<C-u>Unite<Space>
	autocmd User MyVimRc nnoremap <silent> <C-k>h            :<C-u>Unite -ignorecase file_rec<CR>
	autocmd User MyVimRc nnoremap <silent> <C-k><C-h>        :<C-u>Unite -ignorecase neomru/file<CR>
	autocmd User MyVimRc nnoremap <silent> <C-k><C-f>        :<C-u>Unite -ignorecase outline<CR>
	autocmd User MyVimRc nnoremap <silent> <C-k>f            :<C-u>Unite -ignorecase -start-insert filetype<CR>
	autocmd User MyVimRc nnoremap <silent> <C-k><C-g>        :<C-u>Unite -ignorecase -start-insert syntax<CR>
	autocmd User MyVimRc nnoremap <silent> <C-k><C-i>        :<C-u>Unite -ignorecase -start-insert line<CR>
	autocmd User MyVimRc nnoremap <silent> <leader><leader>u :<C-u>UniteClose<CR>

	" ref-dicts-en
	autocmd User MyVimRc nnoremap <silent> <leader>K :<C-u>Ref webdict weblio <C-r>=expand('<cword>')<CR><CR>

	" vim-over
	autocmd User MyVimRc nnoremap <silent>       :%s/       :<C-u>OverCommandLine %s/<CR>
	autocmd User MyVimRc nnoremap <silent>       :s/        :<C-u>OverCommandLine s/<CR>
	autocmd User MyVimRc nnoremap <silent><expr> <C-k><C-s> ':OverCommandLine %s/\<' . expand('<cword>') . '\>/<CR>'
	autocmd User MyVimRc nnoremap <silent><expr> <C-k>s     ':OverCommandLine %s/\<' . expand('<cword>') . '\>/' . expand('<cword>') . '<CR>'
	autocmd User MyVimRc vnoremap <silent>       :s/        :<C-u>OverCommandLine '<,'>s/<CR>
	autocmd User MyVimRc cnoremap <silent>       <C-k>:     <Home>OverCommandLine <CR>
	"@Marked('this is temporary keymapping, because vim-over do not imported cnoremap maybe')
	autocmd FileType * OverCommandLineNoremap <C-b> <Left>
	autocmd FileType * OverCommandLineNoremap <C-f> <Right>
	"autocmd FileType * OverCommandLineNoremap <C-k><C-k> <C-\>e getcmdpos() < 2 ? '' : getcmdline()[:getcmdpos()-2]<CR>

	" anzu-chan
	autocmd User MyVimRc nmap n      <Plug>(anzu-n-with-echo)zv
	autocmd User MyVimRc nmap N      <Plug>(anzu-N-with-echo)zv
	autocmd User MyVimRc nmap *      <Plug>(anzu-star-with-echo)zv
	autocmd User MyVimRc nmap #      <Plug>(anzu-sharp-with-echo)zv
	autocmd User MyVimRc nmap <C-w>* <C-w>v<Plug>(anzu-star-with-echo)zv
	autocmd User MyVimRc nmap <C-w># <C-w>v<Plug>(anzu-sharp-with-echo)zv
	autocmd User MyVimRc nmap <leader><leader>* g*/<Up><Home>\m\C<CR>
	autocmd User MyVimRc nmap <leader><leader># g#?<Up><Home>\m\C<CR>

	" incsearch.vim
	autocmd User MyVimRc nmap <expr>      /                 foldclosed('.') > -1 ? 'zv<Plug>(incsearch-forward)'  : '<Plug>(incsearch-forward)'
	autocmd User MyVimRc nmap <silent>    <leader>/         /\m\C
	autocmd User MyVimRc nmap <silent>    <leader><leader>/ /\m\C\<\>[Left][Left]
	autocmd User MyVimRc nmap             q/                /\<<C-r>"\><CR>
	autocmd User MyVimRc nmap             g/                <Plug>(incsearch-stay)
	autocmd User MyVimRc nmap <expr>      ?                 foldclosed('.') > -1 ? 'zv<Plug>(incsearch-backward)' : '<Plug>(incsearch-backward)'
	autocmd User MyVimRc nmap <silent>    <leader>?         ?\m\C
	autocmd User MyVimRc nmap <silent>    <leader><leader>? ?\m\C\<\>[Left][Left]
	autocmd User MyVimRc nmap             q?                ?\<<C-r>"\><CR>
	autocmd User MyVimRc vmap <expr>      /                 foldclosed('.') > -1 ? 'zv<Plug>(incsearch-forward)'  : '<Plug>(incsearch-forward)'
	autocmd User MyVimRc vmap <expr>      ?                 foldclosed('.') > -1 ? 'zv<Plug>(incsearch-backward)' : '<Plug>(incsearch-backward)'
	"@Marked('Set event FileType *, because avoid error. please suitable event')
	autocmd FileType * IncSearchNoreMap <C-j> <CR>
	autocmd FileType * IncSearchNoreMap <C-l> <Esc>

	" TaskList.vim
	autocmd User MyVimRc nmap <silent> <leader>t <Plug>TaskListToggle

	" undotree
	autocmd User MyVimRc nnoremap <silent> <leader>u :<C-u>UndotreeToggle<CR>

	" vim-indent-guides
	autocmd User MyVimRc nnoremap <silent> <C-h><C-i> :<C-u>call vimrc#keys#toggle_indent_guides()<CR>

	" neosnippet.vim
	autocmd User MyVimRc imap <expr> <C-s> neosnippet#expandable() ? '<Plug>(neosnippet_expand)' : '<Plug>(neosnippet_jump)'
	autocmd User MyVimRc smap <expr> <C-s> neosnippet#expandable() ? '<Plug>(neosnippet_expand)' : '<Plug>(neosnippet_jump)'

	" separetaro.vim
	autocmd User MyVimRc nmap <leader>ps <Plug>(separetoro_put_short_under)
	autocmd User MyVimRc nmap <leader>pS <Plug>(separetoro_put_short_over)
	autocmd User MyVimRc nmap <leader>pl <Plug>(separetoro_put_long_under)
	autocmd User MyVimRc nmap <leader>pL <Plug>(separetoro_put_long_over)

	" neocomplete.vim
	autocmd User MyVimRc inoremap <silent> <C-k><C-i> <C-o>:NeoCompleteToggle<CR>
	autocmd User MyVimRc inoremap <expr>   <CR>  neocomplete#close_popup()  . '<CR>'
	autocmd User MyVimRc inoremap <expr>   <Tab> neocomplete#close_popup()  . '<Tab>'
	autocmd User MyVimRc inoremap <expr>   <C-y> neocomplete#cancel_popup() . '<C-y>'
	autocmd User MyVimRc inoremap <expr>   <C-e> neocomplete#cancel_popup() . '<C-e>'

	" vim-visualstar
	autocmd User MyVimRc vmap g* <Plug>(visualstar-*)Nzz

	" repl.vim
	autocmd User MyVimRc nmap <leader>o <Plug>(repl-run)

	" vim-operator-flashy
	autocmd User MyVimRc map  y <Plug>(operator-flashy)
	autocmd User MyVimRc nmap Y <Plug>(operator-flashy)$
augroup END

" }}}
" Others {{{

augroup KeyMapping
	" normal mode {{{

	autocmd User MyVimRc nmap <C-j> <CR>

	autocmd User MyVimRc nnoremap Q      gQ
	autocmd User MyVimRc nnoremap zs     zszh
	autocmd User MyVimRc nnoremap <C-n>  gt
	autocmd User MyVimRc nnoremap <C-p>  gT
	autocmd User MyVimRc nnoremap <C-m>  o<Esc>
	autocmd User MyVimRc nnoremap <C-]>  g<C-]>
	autocmd User MyVimRc nnoremap g<C-]> <C-]>

	autocmd User MyVimRc nnoremap <silent> m: :<C-u>marks<CR>
	autocmd User MyVimRc nnoremap <silent> q: :<C-u>register<CR>
	autocmd User MyVimRc nnoremap <silent> g: :<C-u>buffers<CR>
	autocmd User MyVimRc nnoremap <silent> z: :<C-u>tabs<CR>
	autocmd User MyVimRc nnoremap <silent> g> :<C-u>messages<CR>
	autocmd User MyVimRc nnoremap <silent> g* :<C-u>execute 'silent! normal! *<C-o>'<CR>

	autocmd User MyVimRc nnoremap <silent> <leader>b                :<C-u>NewOverridden \| resize 5 \| setl buftype=nofile \| setl filetype=scratch<CR>
	autocmd User MyVimRc nnoremap <silent> <leader>B                :<C-u>NewOverridden \| resize 5<CR>
	autocmd User MyVimRc nnoremap <silent> <leader>k                :<C-u>call vimrc#keys#cursor_up_to_lid()<CR>
	autocmd User MyVimRc nnoremap <silent> <leader>j                :<C-u>call vimrc#keys#cursor_down_to_ground()<CR>
	autocmd User MyVimRc nnoremap <silent> <leader><leader>b        :<C-u>call vimrc#keys#bufclose_filetype('scratch')<CR>
	autocmd User MyVimRc nnoremap <silent> <leader><leader>h        :<C-u>helpclose<CR>
	autocmd User MyVimRc nnoremap <silent> <leader><leader>q        :<C-u>call vimrc#keys#bufclose_filetype('qf')<CR>
	autocmd User MyVimRc nnoremap <silent> <leader><leader><leader> :<C-u>echohl ErrorMsg \| echo "Don't rush it, keep cool." \| echohl None<CR>

	autocmd User MyVimRc nnoremap <silent> <C-k><C-r>     :<C-u>Reload<CR>
	autocmd User MyVimRc nnoremap <silent> <C-k>r         :<C-u>let &filetype=&filetype<CR>
	autocmd User MyVimRc nnoremap <silent> <C-k><C-l>     :<C-u>nohlsearch<CR>
	autocmd User MyVimRc nnoremap <silent> <C-k><C-j>     :<C-u>write<CR>
	autocmd User MyVimRc nnoremap <silent> <C-k>J         :<C-u>wall \| echo 'written all !'<CR>
	autocmd User MyVimRc nnoremap <silent> <C-k><Space>   :<C-u>call vimrc#keys#clear_ends_space()<CR>
	autocmd User MyVimRc nnoremap <silent> <Space><Space> :<C-u>call vimrc#keys#compress_spaces()<CR>

	" }}}
	" insert mode {{{

	autocmd User MyVimRc imap <C-j> <CR>

	autocmd User MyVimRc inoremap <C-l> <Esc>
	autocmd User MyVimRc inoremap <C-k><C-k> <C-o>"_d$
	autocmd User MyVimRc inoremap <C-k><C-y> <Esc>k"zyyjV"zp:let @z = ''<CR>A

	autocmd User MyVimRc inoremap <silent> <C-k><C-j> <Esc>:write<CR>
	autocmd User MyVimRc inoremap <silent> <C-k>J     <Esc>:wall \| echo 'written all !'<CR>

	" }}}
	" command-line mode {{{

	autocmd User MyVimRc cmap     <C-]>      \<\>[Left][Left]
	autocmd User MyVimRc cnoremap <C-b>      <Left>
	autocmd User MyVimRc cnoremap <C-f>      <Right>
	autocmd User MyVimRc cnoremap <C-a>      <Home>
	autocmd User MyVimRc cnoremap <C-h>      <BS>
	autocmd User MyVimRc cnoremap <C-d>      <Del>
	autocmd User MyVimRc cnoremap <C-e>      <End>
	autocmd User MyVimRc cnoremap <C-k><C-k> <C-\>e getcmdpos() < 2 ? '' : getcmdline()[ : getcmdpos() - 2]<CR>
	autocmd User MyVimRc cnoremap <C-l>      <C-c>
	autocmd User MyVimRc cnoremap <C-g>      '<,'>
	autocmd User MyVimRc cnoremap <C-o>      <Up>
	autocmd User MyVimRc cnoremap <C-y>      <Down>

	" }}}
	" visual mode {{{

	" textobj-function
	autocmd User MyVimRc vmap af <Plug>(textobj-function-a)
	autocmd User MyVimRc vmap if <Plug>(textobj-function-i)

	" textobj-indent
	autocmd User MyVimRc vmap ai <Plug>(textobj-indent-a)
	autocmd User MyVimRc vmap ii <Plug>(textobj-indent-i)

	" textobj-from_regexp
	" Select alphabet glob
	autocmd User MyVimRc vmap <expr> a_ textobj#from_regexp#mapexpr('[^A-Za-z0-9][A-Za-z0-9]\+[^A-Za-z0-9]')
	autocmd User MyVimRc vmap <expr> i_ textobj#from_regexp#mapexpr('[A-Za-z0-9]\+')
	" Select line ignore newline code (and ignore head spaces)
	autocmd User MyVimRc vmap <expr> al textobj#from_regexp#mapexpr('^.*$')
	autocmd User MyVimRc vmap <expr> il textobj#from_regexp#mapexpr('^\s*\zs.*\ze.*$')

	autocmd User MyVimRc vnoremap <C-l> <Esc>
	autocmd User MyVimRc vnoremap <silent> <leader>k :<C-u>call <SID>cursor_up_to_lid()<CR>
	autocmd User MyVimRc vnoremap <silent> <leader>j :<C-u>call <SID>cursor_down_to_ground()<CR>
	autocmd User MyVimRc vnoremap <silent> i=        :Alignta =/1<CR>
	autocmd User MyVimRc vnoremap <silent> i:        :Alignta :/1<CR>

	" Don't select blank
	autocmd User MyVimRc vnoremap a" 2i"
	autocmd User MyVimRc vnoremap a' 2i'
	autocmd User MyVimRc vnoremap a` 2i`

	" }}}
	" select mode {{{

	autocmd User MyVimRc snoremap <C-l> <Esc>

	" }}}
	" operator {{{

	" textobj-function
	autocmd User MyVimRc omap af <Plug>(textobj-function-a)
	autocmd User MyVimRc omap if <Plug>(textobj-function-i)

	" textobj-indent
	autocmd User MyVimRc omap ai <Plug>(textobj-indent-a)
	autocmd User MyVimRc omap ii <Plug>(textobj-indent-i)

	" textobj-from_regexp
	" Select alphabet glob
	autocmd User MyVimRc omap <expr> a_ textobj#from_regexp#mapexpr('[^A-Za-z0-9][A-Za-z0-9]\+[^A-Za-z0-9]')
	autocmd User MyVimRc omap <expr> i_ textobj#from_regexp#mapexpr('[A-Za-z0-9]\+')
	" Select line ignore newline code (and ignore head spaces)
	autocmd User MyVimRc omap <expr> al textobj#from_regexp#mapexpr('^.*$')
	autocmd User MyVimRc omap <expr> il textobj#from_regexp#mapexpr('^\s*\zs.*\ze.*$')

	" Don't select blank
	autocmd User MyVimRc onoremap a" 2i"
	autocmd User MyVimRc onoremap a' 2i'
	autocmd User MyVimRc onoremap a` 2i`

	" }}}
	" digraph {{{

	digraph %% 8984
	digraph 8: 9731

	" }}}
augroup END

" }}}

" }}}
" Buffer local {{{

augroup PluginPrefs
	autocmd FileType int-* nnoremap <buffer> q          <NOP>
	autocmd FileType int-* nnoremap <buffer> <C-n>      gt
	autocmd FileType int-* nnoremap <buffer> <C-p>      gT
	autocmd FileType int-* nnoremap <buffer> <C-l>      <NOP>

	autocmd FileType int-* nmap     <buffer> <C-]>      <Plug>(vimshell_int_clear)
	autocmd FileType int-* nmap     <buffer> Q          <Plug>(vimshell_int_exit)
	autocmd FileType int-* nmap     <buffer> gJ         <Plug>(vimshell_int_next_prompt)
	autocmd FileType int-* nmap     <buffer> gK         <Plug>(vimshell_int_previous_prompt)

	autocmd FileType int-* inoremap <buffer> <C-l>      <Esc>
	autocmd FileType int-* inoremap <buffer> <C-b>      <Left>
	autocmd FileType int-* inoremap <buffer> <C-f>      <Right>
	autocmd FileType int-* inoremap <buffer> <C-e>      <End>
	autocmd FileType int-* inoremap <buffer> <C-d>      <Del>
	autocmd FileType int-* inoremap <buffer> <C-n>      <Tab>

	autocmd FileType int-* imap     <buffer> <C-p>      <Plug>(vimshell_int_history_unite)
	autocmd FileType int-* imap     <buffer> <C-]>      <C-o><Plug>(vimshell_int_clear)
	autocmd FileType int-* imap     <buffer> <CR>       <Plug>(vimshell_int_execute_line)
	autocmd FileType int-* imap     <buffer> <C-k><C-p> <Plug>(vimshell_int_history_unite)

	autocmd FileType ref-* nnoremap <silent><buffer> Q :<C-u>quit<CR>
augroup END

" }}}


"-------------------------"
"        File_Type        "
"-------------------------"
" {{{

" If buffer does not has filetype, set filetype 'none'
autocmd ExtensionType VimEnter,BufNew * if empty(&ft) | setf none | endif

"@Marked('if fixed bug of auto indent, remove this')
augroup UserEvent
	autocmd FileType int-* set indentkeys-=:
augroup END

" }}}


"-------------------------"
"    Environment_Pref     "
"-------------------------"
" {{{

if filereadable(s:vimrc_env)
	execute 'source' s:vimrc_env
endif

" }}}


filetype plugin indent on
syntax enable
doautocmd User MyVimRc
let g:vimrc['loaded'] = 1
