"----------------------------"
"  This file supports        "
"    - *NIX (exclude MacOSX) "
"    - Cygwin                "
"    - Windows Kaoriya GVim  "
"    - NeoVim                "
"----------------------------"

"-------------------
" {- Message -} "
" @Bugs         => It has some bug
" @Incomplete   => It is incompleted implementation
" @Unchecked    => I implemented without check
" @Unsupported  => I don't support some environment
" @Deprecated   => Deprecated
" @Experiment   => That is experimental implementation
" @See          => Please see it
" @Code         => The sample code for how to use
"-------------------


"------"
" Memo "
"------"
" {{{
"
" my gina.vim keymappings are existent in .vim/autoload/vimrc/dein/hook_source.vim
" LanguageClient-neovim is for not only neovim, both vim and neovim
"
" }}}


"----------------------"
" Define global values "
"----------------------"
" {{{

" Open in preference to an entity
let $MYVIMRC = filereadable(expand('~/.dotfiles/.vimrc'))
\    ? expand('~/.dotfiles/.vimrc')
\    : $MYVIMRC

let $MYGVIMRC = filereadable(expand('~/.dotfiles/.gvimrc'))
\    ? expand('~/.dotfiles/.gvimrc')
\    : $MYGVIMRC

let g:vimrc = get(g:, 'vimrc', {
\    'loaded'   : 0,
\    'vim_home' : expand('~/.vim')
\})

" Global values for local plugins
let g:vimrc['is_windows'] = get(g:vimrc, 'is_windows', has('win32'))
let g:vimrc['is_cygwin']  = get(g:vimrc, 'is_cygwin', has('win32unix'))
let g:vimrc['is_kaoriya'] = get(g:vimrc, 'is_kaoriya', has('kaoriya'))
let g:vimrc['is_unix']    = get(g:vimrc, 'is_unix', has('unix'))
let g:vimrc['is_wsl']     = get(g:vimrc, 'is_wsl', executable('uname') && (system('uname -a') =~# 'Microsoft'))

let g:vimrc['has_cygwin'] = executable('cygstart')
let g:vimrc['has_mingw']  = 0  "NOTE: ('dummy')

let s:backupdir  = expand('~/.backup/vim_backup')
let s:directory  = s:backupdir . '/swp'
let s:undodir    = s:backupdir . '/undo'
let s:viewdir    = s:backupdir . '/view'
let s:sessiondir = s:backupdir . '/session'

" }}}


"---------"
" Startup "
"---------"
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
    "TODO: tmp for windows 10 env (over symlink)
    if !filereadable(g:vimrc['vim_home'] . '/init.vim')  "!isdirectory(g:vimrc['vim_home'])
        call mkdir(g:vimrc['vim_home'])
    endif

    " Enable kaoriya plugins
    let s:kaoriya_switch_dir = $VIM . '/switches/enabled/'
    for s:kaoriya_plugin_flag in map(['utf-8.vim', 'vimdoc-ja.vim', 'vimproc.vim'], 's:kaoriya_switch_dir . v:val')
        if !filereadable(s:kaoriya_plugin_flag)
            call writefile([], s:kaoriya_plugin_flag)
        endif
    endfor
    unlet s:kaoriya_switch_dir s:kaoriya_plugin_flag

    " Unset kaoriya default preference
    set noignorecase nosmartcase

    " Disable plugins/kaoriya/plugin/{cmdex,scrnmode}.vim
    let g:plugin_cmdex_disable    = 1
    let g:plugin_scrnmode_disable = 1

    " You must open the vimrc by the utf-8
    autocmd FileEvent BufRead $MYVIMRC setl enc=utf8 fenc=utf8
endif

" }}}
" Prepare dein.vim {{{

" Start dein.vim
let s:dein_dirname = g:vimrc['vim_home'] . '/bundle/repos/github.com/Shougo/dein.vim'
let &runtimepath   = &runtimepath . ',' . s:dein_dirname

try
    call dein#begin(expand('~/.vim/bundle'))
catch /E117/  " If dein.vim is not found
    try
        call vimrc#fetch_dein(s:dein_dirname)
        call dein#begin(expand('~/.vim/bundle'))
        echo 'dein.vim installation was completed.'
        echo 'Please execute :call dein#install(),'
        echo 'and restart your vim.'
    catch /FALIED/
        call vimrc#echo_error('cloning or starting dein.vim failed.')
        call vimrc#echo_error('>> Error build vim environment <<')
    endtry
endtry

" Copy the dein.vim document
let s:dein_doc_from = s:dein_dirname . '/doc/dein.txt'
let s:dein_doc_to   = g:vimrc['vim_home'] . '/doc/dein.txt'
if filereadable(s:dein_doc_from) && !filereadable(s:dein_doc_to)
    " ~/.vim/doc will be :helptags automatically
    call writefile(readfile(s:dein_doc_from), s:dein_doc_to)
endif
unlet s:dein_doc_from s:dein_doc_to

unlet s:dein_dirname

" }}}
" Check backup directories {{{

if !isdirectory(s:directory)
    call mkdir(s:directory, 'p', 0700)
    call system(printf('chown -R %s:%s %s', $USER, $GROUP, s:directory))
endif

if !isdirectory(s:undodir)
    call mkdir(s:undodir, '', 0700)
    call system(printf('chown -R %s:%s %s', $USER, $GROUP, s:undodir))
endif

if !isdirectory(s:sessiondir)
    call mkdir(s:sessiondir, '', 0700)
    call system(printf('chown -R %s:%s %s', $USER, $GROUP, s:sessiondir))
endif

" }}}
" Enable embedded plugins {{{

if !exists('loaded_matchit')
    " Load matchit.vim
    runtime macros/matchit.vim
    " Get help of matchit.vim
    let s:matchit_doc_from = expand('$VIMRUNTIME/macros/matchit.txt')
    let s:matchit_doc_to   = g:vimrc['vim_home'] . '/doc/matchit.txt'
    if filereadable(s:matchit_doc_from) && !filereadable(s:matchit_doc_to)
        call writefile(readfile(s:matchit_doc_from), s:matchit_doc_to)
    endif
    unlet s:matchit_doc_to s:matchit_doc_from
endif

" }}}


"--------------"
" Load plugins "
"--------------"
" {{{

call dein#load_toml('~/.vim/dein.toml',      {'lazy': 0})
call dein#load_toml('~/.vim/dein_lazy.toml', {'lazy': 1})
call dein#add('Shougo/dein.vim', {'rtp': ''})

" }}}


"--------------------"
" Load local scripts "
"--------------------"
" {{{
"NOTE: This section must be put at between dein#begin() and dein#end()

if filereadable(expand('~/.vimrc_private'))
    source ~/.vimrc_private
endif

if filereadable(expand('~/.vimrc_env'))
    source ~/.vimrc_env
endif

" }}}


"---------------------"
" Configurate plugins "
"---------------------"
" --- netrw --- {{{

" Enable netrw previewing
let g:netrw_preview = 1

" Set directory for .netwhist and .netrwbook
let g:netrw_home = g:vimrc['vim_home']

" Set default options for opening netrw
let g:netrw_bufsettings = 'relativenumber readonly nomodifiable nomodified nowrap nobuflisted'

" }}}
" --- vim-quickrun --- {{{

let g:quickrun_no_default_key_mappings = 0

" For *nix environments
let g:quickrun_config = {
\   '_': {
\       'split': '',
\       'runner': 'system',
\       'runner/vimproc/updatetime': 10,
\       'hook/time/enable': 1,
\       'outputter': 'error',
\       'outputter/error/error': 'quickfix',
\       'outputter/error/success': 'buffer',
\   },
\   'cpp': {
\       'cmdopt': '-std=c++14'
\   },
\   'java': {
\       'cmdopt': '-encoding UTF-8 -source 1.8'
\   },
\   'cs': {
\       'command': 'mcs'
\   },
\   'vimspec': {
\       'command' : 'themis',
\       'cmdopt'  : '--runtimepath ".."',
\       'exec'    : '%c %o %s:p | tr -d "\r"',
\       'tempfile':  printf('%s/{tempname()}.vimspec', $TMP)
\   },
\   'html': {
\       'command': 'xdg-open',
\       'outputter': 'null',
\       'exec'     : '%c %s:p'
\   },
\   'tex': {
\       'command': 'ptex2pdf',
\       'cmdopt' : '-l',
\       'exec'   : '%c %o %s:r'
\   },
\   'clojure': {
\       'command': 'lein',
\       'cmdopt' : 'exec'
\   },
\   'swift': {
\       'command': 'swift'
\   },
\   'scala': {
\       'cmdopt': '-feature'
\   },
\   'brainfuck': {
\       'command': 'brainfuck'
\   },
\   'nico': {
\       'command': 'nicorun'
\   },
\   'haskell': {
\       'cmdopt': '--ghc-arg=-fprint-explicit-kinds',
\       'command': 'stack',
\       'exec': '%c exec runghc -- %o %s',
\       'runner': 'vimproc',
\   },
\   'lhaskell': {
\       'command': 'stack exec runghc',
\       'exec': ['grep "^>.*$" %s | sed -r "s/^>//g" > %s:p:r.hs', '%c %o %s:p:r.hs'],
\       'tempfile': '%{tempname()}.lhs',
\       'hook/sweep/files': '%S:p:r.hs',
\   },
\   'stack_test': {
\       'command': 'stack',
\       'cmdopt': 'test',
\       'exec': '%c %o',
\       'runner': 'vimproc',
\       'outputter': 'quickfix',
\   },
\   'stack_build': {
\       'type': 'stack_test',
\       'cmdopt': 'build',
\   },
\   'eta': {
\       'runner': 'vimproc',
\   },
\   'etlas_build': {
\       'command': 'etlas',
\       'cmdopt': 'build',
\       'exec': '%c %o',
\       'runner': 'vimproc',
\       'outputter': 'quickfix',
\   },
\   'elm': {
\       'runner': 'vimproc',
\       'command': 'elm-make',
\       'cmdopt': '--warn',
\       'exec': ['%c %s %o --output /tmp/vim-quickrun-elm.html', 'xdg-open /tmp/vim-quickrun-elm.html'],
\       'tempfile': '%{tempname()}.elm',
\   },
\   'idris': {
\       'cmdopt': '-p base -p prelude -p effects -p contrib',
\   },
\}

" Append config of each environment
if g:vimrc['is_windows']
    call vimrc#plugins#append_config_quickrun_windows()
elseif g:vimrc['is_cygwin']
    call vimrc#plugins#append_config_quickrun_cygwin()
endif

" }}}
" --- TweetVim --- {{{

" Tweet smoothly
let g:tweetvim_async_post = 1
let g:twibill_use_job     = 1

" Avoid unite-tweetvim lazy loading error
let g:tweetvim_config_dir = expand('~/.tweetvim')

" }}}
" --- vimshell.vim --- {{{

let g:vimshell_no_save_history_commands = {
\    'history': 1,
\    'ls'     : 1,
\    'clear'  : 1
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
" --- vimshell-kawaii.vim --- {{{

" Her face is very useful
let g:vimshell_kawaii_smiley = 1

" }}}
" --- foldCC --- {{{

let g:foldCCtext_maxchars = 120

" }}}
" --- vim-submode --- {{{

let g:submode_timeout = 0

function! s:vim_submode_on_source()
    augroup PluginPrefs
        " Window Resizer
        autocmd User MyVimRc call submode#enter_with('window_resize', 'n', '', '<C-s>w')
        autocmd User MyVimRc call submode#map('window_resize', 'n', '', 'j', '3<C-w>+')
        autocmd User MyVimRc call submode#map('window_resize', 'n', '', 'k', '3<C-w>-')
        autocmd User MyVimRc call submode#map('window_resize', 'n', '', 'h', '3<C-w><')
        autocmd User MyVimRc call submode#map('window_resize', 'n', '', 'l', '3<C-w>>')

        " Buffer Changer
        autocmd User MyVimRc call submode#enter_with('buffer_change', 'n', '', '<C-s>b')
        autocmd User MyVimRc call submode#map('buffer_change', 'n', 's', 'n', ':bnext<CR>')
        autocmd User MyVimRc call submode#map('buffer_change', 'n', 's', 'p', ':bprevious<CR>')

        " Continuous Buffer Deleter
        autocmd User MyVimRc call submode#enter_with('cont_bdelete', 'n', '', '<C-s><C-w>c', ':bdelete<CR>')
        autocmd User MyVimRc call submode#map('cont_bdelete', 'n', 's', 'c', ':bdelete<CR>')
    augroup END
endfunction
call dein#set_hook('vim-submode', 'hook_source', function('s:vim_submode_on_source'))

" }}}
" --- vimdoc-ja --- {{{

" vimdoc-ja is secondary order
set helplang=en,ja

" }}}
" --- TaskList.vim --- {{{

" TaskList finds these line
let g:tlTokenList = ['TODO', 'FIXME', 'XXX']

" Show the window at bottom
let g:tlWindowPosition = 1

" Save current position when TaskList is closed
let g:tlRememberPosition = 1

" }}}
" --- vim-indent-guides --- {{{

let g:indent_guides_default_mapping = 0
let g:indent_guides_guide_size      = 1
let g:indent_guides_auto_colors     = 0

" Define colors
augroup HighlightPref
    autocmd VimEnter,ColorScheme * highlight default link IndentGuidesOdd  PmenuSel
    autocmd VimEnter,ColorScheme * highlight default link IndentGuidesEven Pmenu
augroup END

" Define indent-guides state at global
"@See('nnoremap <C-h><C-i>')
"FIXME: Disable if the buffer is &noexpandtab
let g:vimrc#keys#indent_guides_enable = get(g:, 'vimrc#keys#indent_guides_enable', 1)
augroup FileEvent
    autocmd WinEnter,BufWinEnter * IndentGuidesDisable
    autocmd WinEnter,BufWinEnter *.{xml,html,css,scss,erb,xaml,fxml,scala,kt,gradle,vim,cs,csproj,ts,js,json,tsx,java,vimspec,sh,zsh,c,h,cpp,hpp}
    \   if g:vimrc#keys#indent_guides_enable
    \|      IndentGuidesEnable
    \|  endif
augroup END

" }}}
" --- vim-colors-solarized --- {{{

" Use dark color
let g:solarized_contrast = 'high'

" }}}
" --- aho-bakaup.vim --- {{{

let g:bakaup_backup_dir  = s:backupdir
let g:bakaup_auto_backup = 1

" }}}
" --- neosnippet.vim --- {{{

let g:neosnippet#snippets_directory = g:vimrc['vim_home'] . '/neosnippets'
let g:neosnippet#disable_select_select_mappings = 1

" }}}
" --- vimconsole.vim --- {{{

let g:vimconsole#auto_redraw             = 1
let g:vimconsole#no_default_key_mappings = 1

" }}}
" --- vim-textobj-indent --- {{{

let g:textobj_indent_no_default_key_mappings = 1

" }}}
" --- neocomplete.vim --- {{{

let g:neocomplete#enable_at_startup = 1

" neocomplete is disabled in
let g:neocomplete#sources = {
\    'int-ghci'  : [],
\    'int-stack' : []
\}

" }}}
" --- unite-tag --- {{{

" Fully showing name
let g:unite_source_tag_max_name_length  = 100
let g:unite_source_tag_max_fname_length = 100

" }}}
" --- deoplete.nvim --- {{{

let g:deoplete#enable_at_startup = 0

" }}}
" --- vim-visualstar --- {{{

" Do zzzv after execute visualstar
let g:visualstar_extra_commands = 'zzzv'

" }}}
" --- submode-window_move.vim --- {{{

" Register mode starting keymapping
let g:submode_window_move = {}
let g:submode_window_move['start_tab_move']                   = '<C-s><C-g>'
let g:submode_window_move['start_window_move_with_move_next'] = '<C-s>N'
let g:submode_window_move['start_window_move_with_move_prev'] = '<C-s>P'

" }}}
" --- repl.vim --- {{{

" Use this repl
let g:repl_filetype_repl = {
\    'haskell' : {
\        'repl' : 'stack ghci',
\        'opt'  : ''
\    }
\}

" Set myself
let g:repl_no_default_keymappings = 1
" Open by vertical split
let g:repl_split_command = 'vertical split'

" }}}
" --- vim-gista --- {{{

" Don't ask description for :Gista post
let g:gista#command#post#interactive_description = 0
let g:gista#command#post#allow_empty_description = 1

augroup PluginPrefs
    autocmd User GistaPost call vimrc#autocmd#yank_gista_posted_url()
augroup END

"}}}
" --- aref-web.vim --- {{{

let g:aref_web_source = {
\    'weblio': {
\        'url': 'https://ejje.weblio.jp/content/%s'
\    },
\    'stackage': {
\        'url': 'https://www.stackage.org/lts-10.9/hoogle?q=%s&page=1'
\    },
\    'hoogle': {
\        'url': 'https://www.haskell.org/hoogle/?hoogle=%s'
\    },
\    'shellcheck': {
\        'url': 'https://github.com/koalaman/shellcheck/wiki/%s'
\    },
\}

let g:aref_web_split_vertically = v:true

"let g:ref_source_webdict_sites['weblio'].filter = function('vimrc#plugins#weblio_filter')

"}}}
" --- autofmt ---" {{{

set formatexpr=autofmt#japanese#formatexpr()
let autofmt_allow_over_tw=1

"}}}
" --- github-complete.vim --- {{{

" Use as emoji completer
let g:github_complete_enable_issue_completion = 0
let g:github_complete_enable_user_completion  = 0
let g:github_complete_enable_repo_completion  = 0
let g:github_complete_include_issue_title     = 0

let g:github_complete_enable_neocomplete        = 1
let g:github_complete_emoji_japanese_workaround = 1

" }}}
" --- vim-textobj-between --- {{{

let g:textobj_between_no_default_key_mappings = 1

" }}}
" --- ale --- {{{

let g:ale_linters = {
\    'haskell': ['hlint', 'stack ghc'],
\    'html': ['htmlhint', 'tidy'],
\    'css': ['csslint', 'stylelint'],
\}

" Turn off by default
ALEDisable

" }}}
" --- elm-vim --- {{{

let g:elm_setup_keybindings  = 0
let g:elm_make_output_file   = '/tmp/elm-vim-output.html'
let g:elm_make_show_warnings = 1
let g:elm_browser_command    = 'xdg-open'

" }}}
" --- vim-fakeclip --- {{{

let fakeclip_provide_clipboard_key_mappings = g:vimrc['is_wsl']

" }}}
" --- denite.nvim --- {{{

call denite#custom#map('insert', '<C-l>', '<Esc>')
call denite#custom#map('insert', '<C-j>', '<CR>')
call denite#custom#map('insert', '<C-n>', '<denite:move_to_next_line>')
call denite#custom#map('insert', '<C-p>', '<denite:move_to_previous_line>')
call denite#custom#map('insert', '<C-a>', '<denite:move_caret_to_head>')
call denite#custom#map('insert', '<C-e>', '<denite:move_caret_to_tail>')
call denite#custom#map('insert', '<C-f>', '<denite:move_caret_to_right>')
call denite#custom#map('insert', '<C-b>', '<denite:move_caret_to_left>')
call denite#custom#map('insert', '<C-x>', '<denite:do_action:delete>')

call denite#custom#source('buffer', 'matchers', ['matcher_substring'])
call denite#custom#source('file_mru', 'matchers', ['matcher_substring'])
call denite#custom#source('tag', 'matchers', ['matcher_substring'])
call denite#custom#source('file_rec', 'matchers', ['matcher_substring'])

augroup PluginPrefs
    autocmd BufEnter,BufWinEnter *
        \   call denite#custom#var('outline', 'command', ['ctags'])
        \|  call denite#custom#var('outline', 'options', [])
    autocmd BufEnter,BufWinEnter *.hs
        \   call denite#custom#var('outline', 'command', ['hasktags'])
        \|  call denite#custom#var('outline', 'options', ['--ignore-close-implementation', '--ctags', '-x'])
    autocmd BufEnter,BufWinEnter *.md,*.markdown
        \   call denite#custom#var('outline', 'command', ['markdown2ctags.py'])
        \|  call denite#custom#var('outline', 'options', ['--sort=yes'])
        \|  call denite#custom#var('outline', 'file_opt', '-f')
augroup END

" }}}
" --- idris-vim --- {{{

let g:idris_vim_enable_keymappings_by_default = v:false

" }}}
" --- vim-textobj-clang --- {{{

let g:textobj_clang_more_mappings = 1

" }}}
" --- LanguageClient-neovim --- " {{{

let g:LanguageClient_serverCommands = {
    \ 'haskell': ['hie', '--lsp'],
\}

" }}}

call dein#end()


"-----------------"
" Set vim options "
"-----------------"
" {{{

set number relativenumber nowrap hlsearch list scrolloff=16 incsearch
set textwidth=0 tabstop=4 shiftwidth=4 expandtab
set listchars=tab:»_,trail:_,extends:»,precedes:«,nbsp:%,eol:↲
set breakindent linebreak autoindent cindent nojoinspaces
set previewheight=40
set laststatus=2 wildmenu noruler cmdheight=2 history=500
set tabline=%!vimrc#set#tabline_as_statusline()
set backspace=indent,eol,start
set nowrapscan visualbell notimeout
set fileencodings=ucs-bom,utf-8,sjis,euc-jp,cp932,iso-2022-jp-3,iso-2022-jp,eucjp-ms,euc-jisx0213,ucs-bom,latin1,default
set path=.,,./** shellslash matchpairs+=<:>
set browsedir=buffer spelllang=en_US,cjk suffixes=
set hidden

if !has('nvim')
    set termkey=<C-z>
endif

" See ':h hl-User1..9' for what is the pattern of '%n*string%*' (n is a naturalnumer),
" and below augroup 'HighlightPref'
let &statusline = '%1*[%F(%n)]%*'
    \           . '%2*[FT=%y]%*'
    \           . '[%03v]'
    \           . '[Fenc=%{&fileencoding}]'
    \           . '[Enc=%{&encoding}]'
    \           . '%3*%m%*'
    \           . '%4*%r%*'

" ☆
set ambiwidth=double

" Define my highlight colors
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
    autocmd ColorScheme * highlight LineNr                       ctermfg=Gray
    "autocmd ColorScheme * highlight CursorLine   cterm=underline ctermfg=Cyan

    " StatusLine specified highlight
    autocmd ColorScheme * highlight User1 cterm=standout ctermfg=Green  ctermbg=Black
    autocmd ColorScheme * highlight User2 cterm=standout ctermfg=Yellow ctermbg=Black
    autocmd ColorScheme * highlight User3 cterm=standout ctermfg=White  ctermbg=Black
    autocmd ColorScheme * highlight User4 cterm=standout ctermfg=Gray   ctermbg=Black
    autocmd ColorScheme * highlight User5 cterm=standout ctermfg=Black  ctermbg=White
    autocmd ColorScheme * highlight User6 cterm=standout ctermfg=Black  ctermbg=White
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

" Set the colorscheme, but it is set only once
if !g:vimrc['loaded']
    set background=dark
    colorscheme elflord
endif

" Tabline is always shown
set showtabline=2

" Set the fold options
set foldmethod=marker
set foldtext=FoldCCtext()
set foldcolumn=1
let &fillchars = 'vert:|,fold: '
set foldopen=search,jump,mark,percent,insert,tag,undo
set foldclose=all

" The backup options
let &directory = s:directory
let &viewdir = s:viewdir
set undofile
let &undodir = s:undodir

" Use aho-bakaup.vim's the backup functions
set nobackup

"@Bugs("This option doesn't work finely (?)")
" Disable auto commentalize new line
set formatoptions-=ro

" Always I use the ignorecase
set ignorecase noinfercase

" I control the IME state by myself
set iminsert=0

" Open .tex as LaTex
let g:tex_flavor = 'latex'

" Reference tags of ctags
let &tags = &tags . ',' . join([
    \ 'tags',
    \ '.git/tags',
    \ '../tags',
    \ '../../tags',
    \ '../../../tags',
    \ '../../../../tags',
    \ '../.git/tags',
    \ '../../.git/tags',
    \ '../../../.git/tags',
    \ '../../../../.git/tags'
\], ',')

let lhs_markup = 'none'

" }}}


"---------------------"
" Set augroup details "
"---------------------"
" {{{

augroup FileEvent
    " Auto set cursor position in the file
    autocmd BufReadPost * call vimrc#autocmd#visit_past_position()

    " Auto load filetype dictionary
    autocmd FileType *
    \    if filereadable(printf('%s/dict/filetype/%s.dict', g:vimrc['vim_home'], &filetype))
    \|       execute 'setl dict+=' . printf('%s/dict/filetype/%s.dict', g:vimrc['vim_home'], &filetype)
    \|   endif
augroup END

" :P
"autocmd UserEvent UserGettingBored * echo 'sugar'

augroup UserEvent
    " RelativeNumber is used current window only
    autocmd BufEnter,WinEnter * if &number | setl relativenumber | end
    autocmd BufLeave,Winleave * setl norelativenumber

    autocmd InsertEnter * call vimrc#autocmd#enable_input_completion()
augroup END

" Hide relativenumber when OverCommandLine entered
augroup UserEvent
    autocmd User OverCmdLineEnter setl norelativenumber
    autocmd User OverCmdLineLeave if &number | setl relativenumber | end
augroup END

" Set the 'none' filetype to the empty filetype
augroup ExtensionType
    autocmd VimEnter,BufNew * if empty(&ft) | setf none | endif
augroup END

"NOTE: Remove this after the auto indent bug is fixed
augroup UserEvent
    autocmd FileType int-* set indentkeys-=:
augroup END

" }}}


"-----------------"
" Manage commands "
"-----------------"
" Prepare {{{

call altercmd#load()

" Define cnoreabbr with cmd completion
command! -nargs=+ CmdCnoreabbr call vimrc#cmd#cmd_cnoreabbr(<f-args>)
command! -bar -nargs=1 UnCmdCnoreabbr call vimrc#cmd#un_cmd_cnoreabbr(<q-args>)

" }}}
" AlterCommand {{{

" See .vim/plugin/vimrc.vim
AlterCommand new NewOverridden
AlterCommand e[dit] EditOverridden
AlterCommand vne[w] VnewOverridden
AlterCommand ene[w] EnewOverridden
AlterCommand tabnew TabnewOverridden

" Haskell
AlterCommand etr !etlas run


" }}}
" CmdCnoreabbr {{{

CmdCnoreabbr CdBufDir cd %:p:h

" aref-web.vim
CmdCnoreabbr Hoogle     Aref hoogle
CmdCnoreabbr ShellCheck Aref shellcheck
CmdCnoreabbr Stackage   Aref stackage
CmdCnoreabbr Weblio     Aref weblio

" vim-open-googletranslate
CmdCnoreabbr Google OpenGoogleTranslate

" TweetVim
CmdCnoreabbr SwitchAccount TweetVimSwitchAccount
CmdCnoreabbr UserTimeline  TweetVimUserTimeline

" Others
CmdCnoreabbr Lingr J6uil
CmdCnoreabbr LingrTab TabnewOverridden \| J6uil
CmdCnoreabbr Gist Gista post --stay

" }}}


"--------------------"
" Define keymappings "
"--------------------"
" Virtual {{{

augroup KeyMapping
    autocmd User MyVimRc cnoremap [Left] <Left>
augroup END

" }}}
" Disabling {{{

augroup KeyMapping
    " Enable some hoge<C-c> mappings
    autocmd User MyVimRc nnoremap <C-c>      <NOP>
    autocmd User MyVimRc nnoremap <C-c><C-c> <C-c>

    autocmd User MyVimRc nnoremap <C-w><C-l> <NOP>
    autocmd User MyVimRc nnoremap <C-w><C-o> <NOP>
augroup END

" }}}
" Folding {{{

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

    " Window key <C-w> to gh (save filger epoint)
    "" Disable default
    autocmd User MyVimRc nnoremap gh     <NOP>
    autocmd User MyVimRc nnoremap <C-w>q <NOP>
    autocmd User MyVimRc nnoremap <C-w>c <NOP>
    autocmd User MyVimRc nnoremap <C-w>r <NOP>
    autocmd User MyVimRc nnoremap <C-w>_ <NOP>
    autocmd User MyVimRc nnoremap <C-w>\ <NOP>
    autocmd User MyVimRc nnoremap <C-w>= <NOP>
    autocmd User MyVimRc nnoremap <C-w>o <NOP>
    autocmd User MyVimRc nnoremap <C-w>H <NOP>
    autocmd User MyVimRc nnoremap <C-w>J <NOP>
    autocmd User MyVimRc nnoremap <C-w>K <NOP>
    autocmd User MyVimRc nnoremap <C-w>L <NOP>
    autocmd User MyVimRc nnoremap <C-w>s <NOP>
    autocmd User MyVimRc nnoremap <C-w>v <NOP>
    autocmd User MyVimRc nnoremap ghR    <C-w>r
    " Enable custom
    autocmd User MyVimRc nnoremap <silent> ghq   :<C-u>call vimrc#keys#hide_or_quit()<CR>
    autocmd User MyVimRc nnoremap <silent> ghQ   :<C-u>quitall<CR>
    autocmd User MyVimRc nnoremap <silent> ghc   :<C-u>bdelete<CR>
    autocmd User MyVimRc nnoremap <silent> ghC   :<C-u>bdelete!<CR>
    autocmd User MyVimRc nnoremap <silent> gho   :<C-u>only<CR>
    autocmd User MyVimRc nnoremap <silent> gh_   :<C-u>resize<CR>
    autocmd User MyVimRc nnoremap <silent> gh"   :<C-u>resize 5<CR>
    autocmd User MyVimRc nnoremap          gh\|  <C-w>\|
    autocmd User MyVimRc nnoremap <silent> gh\   :<C-u>vertical resize 1<CR>
    autocmd User MyVimRc nnoremap          gh=   <C-w>=
    autocmd User MyVimRc nmap              gh+   gh_gh\|
    autocmd User MyVimRc nnoremap          ghH   <C-w>H
    autocmd User MyVimRc nnoremap          ghJ   <C-w>J
    autocmd User MyVimRc nnoremap          ghK   <C-w>K
    autocmd User MyVimRc nnoremap          ghL   <C-w>L
    autocmd User MyVimRc nnoremap          ghs   :<C-u>split<CR>
    autocmd User MyVimRc nnoremap          ghv   :<C-u>vsplit<CR>
    autocmd User MyVimRc nnoremap <silent><expr> gH  ('mZ:tabnew<CR>`Zzz'          . (foldlevel('.') > 0 ? 'zo' : ''))
    autocmd User MyVimRc nnoremap <silent><expr> ghh ('mZ:hide<CR>:tabnew<CR>`Zzz' . (foldlevel('.') > 0 ? 'zo' : ''))

    " Open :terminal
    autocmd User MyVimRc nnoremap <silent> <leader>v         :<C-u>call vimrc#open_terminal_as('term-shell', 'vertical', &shell)<CR>
    autocmd User MyVimRc nnoremap <silent> <leader><leader>v :<C-u>call vimrc#open_terminal_as('term-shell', 'horizontal', &shell)<CR>
    autocmd User MyVimRc nnoremap <silent> <leader>V         :<C-u>call vimrc#open_terminal_as('term-shell', 'stay', &shell)<CR>
    autocmd User MyVimRc nnoremap <silent> <leader><leader>V :<C-u>call vimrc#open_terminal_as('term-shell', 'tabnew', &shell)<CR>
    autocmd User MyVimRc nnoremap <silent> <C-[><C-v> :<C-u>call vimrc#keys#toggle_shell_mode()<CR>
augroup END

" }}}
" Toggling Option {{{

augroup KeyMapping
    " Local
    autocmd User MyVimRc nnoremap <silent>       <C-h><C-d> :<C-u>call vimrc#keys#toggle_diff()<CR>
    autocmd User MyVimRc nnoremap <silent><expr> <C-h><C-v> ':setl virtualedit=' . (&virtualedit ==# '' ? 'all' : '') . ' virtualedit?<CR>'
    autocmd User MyVimRc nnoremap <silent><expr> zm         ':setl foldmethod=' . (&foldmethod ==# 'marker' ? 'syntax' : 'marker') . ' foldmethod?<CR>'

    autocmd User MyVimRc nnoremap <silent> <C-h><C-w> :<C-u>setl wrap!           wrap?          <CR>
    autocmd User MyVimRc nnoremap <silent> <C-h><C-c> :<C-u>setl cursorline!     cursorline?    <CR>
    autocmd User MyVimRc nnoremap <silent> <C-h><C-r> :<C-u>setl relativenumber! relativenumber?<CR>
    autocmd User MyVimRc nnoremap <silent> <C-h><C-l> :<C-u>setl list!           list?          <CR>
    autocmd User MyVimRc nnoremap <silent> <C-h><C-n> :<C-u>setl number!         number?        <CR>
    autocmd User MyVimRc nnoremap <silent> <C-h><C-s> :<C-u>setl wrapscan!       wrapscan?      <CR>

    autocmd User MyVimRc inoremap <silent> <C-k><C-w> <C-o>:setl wrap! wrap?<CR>
    autocmd User MyVimRc inoremap <silent> <C-k><C-e> <C-o>:setl expandtab! expandtab?<CR>
augroup END

" }}}
" With plugins {{{

augroup KeyMapping
    " netrw
    autocmd User MyVimRc nnoremap <silent> <leader>e         :<C-u>call vimrc#keys#toggle_netrw_vexplorer()<CR>
    autocmd User MyVimRc nnoremap <silent> <leader><leader>e :<C-u>call vimrc#keys#netrw_wrapper('horizontal')<CR>
    autocmd User MyVimRc nnoremap <silent> <leader>E         :<C-u>call vimrc#keys#netrw_wrapper('stay')<CR>
    autocmd User MyVimRc nnoremap <silent> <leader><leader>E :<C-u>call vimrc#keys#netrw_wrapper('tabnew')<CR>

    " open-browser.vim
    autocmd User MyVimRc nmap <leader>w <Plug>(openbrowser-open)
    autocmd User MyVimRc vmap <leader>w <Plug>(openbrowser-open)

    " vim-quickrun
    autocmd User MyVimRc nmap              <leader>r <Plug>(quickrun)
    autocmd User MyVimRc nnoremap <silent> <leader>R :<C-u>QuickRun -runner shell<CR>
    autocmd User MyVimRc vmap              <leader>r <Plug>(quickrun)
    autocmd User MyVimRc vnoremap <silent> <leader>R :QuickRun -runner shell<CR>

    " denite.nvim
    autocmd User MyVimRc nnoremap          <leader>u  :<C-u>Denite<Space>
    autocmd User MyVimRc nnoremap <silent> <C-k><C-h> :<C-u>tcd <C-r>=isdirectory(expand('%:p:h')) ? expand('%:p:h') : execute(':pwd')[1:]<CR><CR>:Denite file_rec<CR>
    autocmd User MyVimRc nnoremap <silent> <C-k><C-t> :<C-u>Denite tag<CR>
    autocmd User MyVimRc nnoremap <silent> <C-k><C-f> :<C-u>Denite outline<CR>

    " aref-web.vim
    autocmd User MyVimRc nnoremap <leader>K :<C-u>Aref weblio <C-r>=expand('<cword>')<CR><CR>
    autocmd User MyVimRc nnoremap <leader>S :<C-u>Aref stackage <C-r>=expand('<cword>')<CR><CR>
    autocmd User MyVimRc vnoremap <leader>K "zy:<C-u>Aref weblio <C-r>z<CR>
    autocmd User MyVimRc vnoremap <leader>S "zy:<C-u>Aref stackage <C-r>z<CR>

    " vim-over
    autocmd User MyVimRc nnoremap <silent>       :%s/       :<C-u>OverCommandLine %s/<CR>
    autocmd User MyVimRc nnoremap <silent>       :s/        :<C-u>OverCommandLine s/<CR>
    autocmd User MyVimRc nnoremap <silent><expr> <C-k><C-s> ':OverCommandLine %s/\m\C\<' . expand('<cword>') . '\>/<CR>'
    autocmd User MyVimRc nnoremap <silent><expr> <C-k>s     ':OverCommandLine %s/\m\C\<' . expand('<cword>') . '\>/' . expand('<cword>') . '<CR>'
    autocmd User MyVimRc vnoremap <silent>       :s/        :<C-u>OverCommandLine '<,'>s/<CR>
    autocmd User MyVimRc cnoremap <silent>       <C-k>:     <Home>OverCommandLine <CR>
    "NOTE: this is temporary keymapping, because vim-over do not imported cnoremap maybe
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

    " TaskList.vim
    autocmd User MyVimRc nmap <silent> <leader>t <Plug>TaskListToggle

    " undotree
    autocmd User MyVimRc nnoremap <silent> <leader>U :<C-u>UndotreeToggle<CR>

    " vim-indent-guides
    autocmd User MyVimRc nnoremap <silent> <C-h><C-i> :<C-u>call vimrc#keys#toggle_indent_guides()<CR>

    " neosnippet.vim
    autocmd User MyVimRc imap <C-s> <Plug>(neosnippet_jump_or_expand)
    autocmd User MyVimRc smap <C-s> <Plug>(neosnippet_jump_or_expand)

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

    " textobj-indent
    autocmd User MyVimRc omap ai <Plug>(textobj-indent-a)
    autocmd User MyVimRc omap ii <Plug>(textobj-indent-i)
    autocmd User MyVimRc vmap ai <Plug>(textobj-indent-a)
    autocmd User MyVimRc vmap ii <Plug>(textobj-indent-i)
    autocmd User MyVimRc nmap <silent> <leader><leader>s vii:sort<CR>

    " textobj-from_regexp
    " Select line ignore newline code (and ignore head spaces)
    autocmd User MyVimRc omap <expr> al textobj#from_regexp#mapexpr('^.*$')
    autocmd User MyVimRc omap <expr> il textobj#from_regexp#mapexpr('^\s*\zs.*\ze.*$')

    " textobj-from_regexp
    " Select alphabet glob
    autocmd User MyVimRc vmap <expr> a_ textobj#from_regexp#mapexpr('[^A-Za-z0-9][A-Za-z0-9]\+[^A-Za-z0-9]')
    autocmd User MyVimRc vmap <expr> i_ textobj#from_regexp#mapexpr('[A-Za-z0-9]\+')
    autocmd User MyVimRc omap <expr> a_ textobj#from_regexp#mapexpr('[^A-Za-z0-9][A-Za-z0-9]\+[^A-Za-z0-9]')
    autocmd User MyVimRc omap <expr> i_ textobj#from_regexp#mapexpr('[A-Za-z0-9]\+')
    " Select line ignore newline code (and ignore head spaces)
    autocmd User MyVimRc vmap <expr> al textobj#from_regexp#mapexpr('^.*$')
    autocmd User MyVimRc vmap <expr> il textobj#from_regexp#mapexpr('^\s*\zs.*\ze.*$')

    " vim-textobj-between
    autocmd User MyVimRc vmap a* <Plug>(textobj-between-a)
    autocmd User MyVimRc vmap i* <Plug>(textobj-between-i)
    autocmd User MyVimRc omap a* <Plug>(textobj-between-a)
    autocmd User MyVimRc omap i* <Plug>(textobj-between-i)

    " vim-open-googletranslate
    autocmd User MyVimRc vnoremap <silent> <leader>k "zy:OpenGoogleTranslate <C-r>z<CR>

    " ale
    autocmd User MyVimRc nnoremap <silent> <C-k><C-a> :<C-u>ALEToggle<CR>
    autocmd User MyVimRc nnoremap <silent> g{ :<C-u>ALEPrevious<CR>
    autocmd User MyVimRc nnoremap <silent> g} :<C-u>ALENext<CR>

    " incsearch.vim
    autocmd User MyVimRc nmap g/ <Plug>(incsearch-stay)
    autocmd User MyVimRc IncSearchNoreMap <C-j> <CR>
    autocmd User MyVimRc IncSearchNoreMap <C-b> <Left>
    autocmd User MyVimRc IncSearchNoreMap <C-f> <Right>
    autocmd User MyVimRc IncSearchNoreMap <C-a> <Home>
    autocmd User MyVimRc IncSearchNoreMap <C-h> <BS>
    autocmd User MyVimRc IncSearchNoreMap <C-d> <Del>
    autocmd User MyVimRc IncSearchNoreMap <C-e> <End>
    autocmd User MyVimRc IncSearchNoreMap <C-l> <C-c>
    autocmd User MyVimRc IncSearchNoreMap <C-o> <Up>
    autocmd User MyVimRc IncSearchNoreMap <C-y> <Down>

    " gina.vim (and .vim/plugin/vimrc.vim)
    autocmd User MyVimRc nnoremap <silent> <leader>gs :<C-u>GStatus<CR>
    autocmd User MyVimRc nnoremap <silent> <leader>gS :<C-u>GitShowViewer<CR>
    autocmd User MyVimRc nnoremap <silent> <leader>gc :<C-u>GCommit<CR>
    autocmd User MyVimRc nnoremap <silent> <leader>gC :<C-u>GCAM<CR>
    autocmd User MyVimRc nnoremap <silent> <leader>ga :<C-u>GAP<CR>
    autocmd User MyVimRc nnoremap <silent> <leader>gl :<C-u>GLog<CR>
    autocmd User MyVimRc nnoremap <silent> <leader>gL :<C-u>GLP<CR>
    autocmd User MyVimRc nnoremap <silent> <leader>gd :<C-u>GDiff<CR>
    autocmd User MyVimRc nnoremap <silent> <leader>gb :<C-u>GBA<CR>
    autocmd User MyVimRc nnoremap <silent> <leader>gt :<C-u>GTree<CR>
    autocmd User MyVimRc nnoremap <silent> <leader>gT :<C-u>GTreeAll<CR>

    " LanguageClient-neovim
    autocmd User MyVimRc nnoremap <silent> <C-x><C-d><C-g> :<C-u>LspDocumentDiagnostics<CR>
    autocmd User MyVimRc nnoremap <silent> <C-x><C-d><C-d> :<C-u>call LanguageClient_textDocument_definition()<CR>
    autocmd User MyVimRc nnoremap <silent> <C-x><C-d><C-f> :<C-u>call LanguageClient_textDocument_formatting()<CR>
    autocmd User MyVimRc nnoremap <silent> <C-x><C-d><C-r> :<C-u>call LanguageClient_textDocument_rangeFormatting()<CR>
    autocmd User MyVimRc nnoremap <silent> <C-x><C-d><C-s> :<C-u>call LanguageClient_textDocument_documentSymbol()<CR>
    autocmd User MyVimRc nnoremap <silent> <C-x><C-h> :<C-u>call LanguageClient_textDocument_hover()<CR>
    autocmd User MyVimRc nnoremap <silent> <C-x><C-i> :<C-u>LspImplementation<CR>
    autocmd User MyVimRc nnoremap <silent> <C-x><C-g> :<C-u>call LanguageClient_textDocument_references()<CR>
    autocmd User MyVimRc nnoremap <silent> <C-x><C-r> :<C-u>call LanguageClient_textDocument_rename()<CR>
    autocmd User MyVimRc nnoremap <silent> <C-x><C-w> :<C-u>call LanguageClient_workspace_symbol()<CR>

    " vim-textobj-clang
    " You are not i
    autocmd User MyVimRc vmap <silent> a;m i;m
    autocmd User MyVimRc omap <silent> a;m i;m
    autocmd User MyVimRc vmap <silent> a;f i;f
    autocmd User MyVimRc omap <silent> a;f i;f
augroup END

" }}}
" Basics {{{

augroup KeyMapping
    " normal mode {{{

    autocmd User MyVimRc nmap <C-j> <CR>
    autocmd User MyVimRc nmap gd    *ggn

    autocmd User MyVimRc nnoremap H      :<C-u>Denite line<CR>
    autocmd User MyVimRc nnoremap M      :<C-u>Denite file_mru<CR>
    autocmd User MyVimRc nnoremap L      :<C-u>Denite buffer<CR>
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
    autocmd User MyVimRc nnoremap <silent> !  :!<CR>

    autocmd User MyVimRc nnoremap <leader>/         /\m\C
    autocmd User MyVimRc nnoremap <leader><leader>/ /\m\C\<\><Left><Left>
    autocmd User MyVimRc nnoremap <leader>?         ?\m\C
    autocmd User MyVimRc nnoremap <leader><leader>? ?\m\C\<\><Left><Left>

    autocmd User MyVimRc nnoremap <silent> <leader>b         :<C-u>NewOverridden \| resize 5 \| setl buftype=nofile \| setl filetype=markdown<CR>
    autocmd User MyVimRc nnoremap <silent> <leader>B         :<C-u>sp ~/.backup/vim-memo.md<CR>
    autocmd User MyVimRc nnoremap <silent> <leader><leader>q :<C-u>call vimrc#keys#bufclose_filetype(<C-r>=string(g:vimrc.auto_closing_filetypes)<CR>)<CR>
    autocmd User MyVimRc let g:vimrc.auto_closing_filetypes = [
        \ 'aref_web',
        \ 'diff',
        \ 'gina-branch',
        \ 'gina-log',
        \ 'gina-status',
        \ 'gitdiffviewer',
        \ 'gitlogviewer',
        \ 'gitreflogviewer',
        \ 'gitshowviewer',
        \ 'help',
        \ 'man',
        \ 'netrw',
        \ 'qf',
        \ 'quickrun',
        \ 'scratch',
        \ 'tasklist',
    \]

    autocmd User MyVimRc nnoremap <silent> <C-k><C-r>     :<C-u>Reload<CR>
    autocmd User MyVimRc nnoremap <silent> <C-k>r         :<C-u>let &filetype=&filetype<CR>:filetype detect<CR>
    autocmd User MyVimRc nnoremap <silent> <C-k>e         :<C-u>EditOverridden %<CR>
    autocmd User MyVimRc nnoremap <silent> <C-k>E         :<C-u>EditOverridden! %<CR>
    autocmd User MyVimRc nnoremap <silent> <C-k><C-l>     :<C-u>nohlsearch<CR>
    autocmd User MyVimRc nnoremap <silent> <C-k><C-j>     :<C-u>write<CR>
    autocmd User MyVimRc nnoremap <silent> <C-k>J         :<C-u>wall \| echo 'written all !'<CR>
    autocmd User MyVimRc nnoremap <silent> <C-k><Space>   :<C-u>call vimrc#keys#clear_ends_space()<CR>
    autocmd User MyVimRc nnoremap <silent> <Space><Space> :<C-u>call vimrc#keys#compress_spaces()<CR>
    autocmd User MyVimRc nnoremap <C-k>f :<C-u>Denite filetype<CR>

    " }}}
    " insert mode {{{

    autocmd User MyVimRc imap <C-j> <CR>

    autocmd User MyVimRc inoremap <C-l> <Esc>
    autocmd User MyVimRc inoremap <C-k><C-k> <C-o>"_d$
    autocmd User MyVimRc inoremap <C-k><C-y> <Esc>k"zyyjV"zp:let @z = ''<CR>A

    autocmd User MyVimRc inoremap <silent> <C-k><C-j> <Esc>:write<CR>
    autocmd User MyVimRc inoremap <silent> <C-k>J     <Esc>:wall \| echo 'written all !'<CR>

    autocmd User MyVimRc inoremap <silent><expr> <C-b> vimrc#keys#get_webpage_title()

    " }}}
    " command-line mode {{{

    autocmd User MyVimRc cmap     <C-]>      \m\C\<\>[Left][Left]
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

    autocmd User MyVimRc vnoremap <C-l> <Esc>
    autocmd User MyVimRc vnoremap i:    :Alignta<Space>
    autocmd User MyVimRc vnoremap <silent> i= :Alignta => =/1<CR>
    autocmd User MyVimRc vnoremap <leader>s :sort<CR>

    " Don't select blank
    autocmd User MyVimRc vnoremap a" 2i"
    autocmd User MyVimRc vnoremap a' 2i'
    autocmd User MyVimRc vnoremap a` 2i`

    " }}}
    " select mode {{{

    autocmd User MyVimRc snoremap <C-l> <Esc>

    " }}}
    " operator {{{

    " Don't select blank
    autocmd User MyVimRc onoremap a" 2i"
    autocmd User MyVimRc onoremap a' 2i'
    autocmd User MyVimRc onoremap a` 2i`

    " }}}
    " digraph {{{

    digraph /= 8800  " not equal

    digraph \( 8834  " right includes left
    digraph \) 8835  " left includes right
    digraph \A 8704  " forall
    digraph \E 8707  " exists
    digraph \a 8743  " and
    digraph \o 8744  " or
    digraph \= 8803  " equivalence relation
    digraph \< 8804  " right more than left or equals
    digraph \> 8805  " left mode than right or equals
    digraph \. 9675  " compose
    digraph \*  215  " cartesian product
    digraph \U 8745  " intersect
    digraph \u 8746  " union

    digraph \|^ 8593 " arrow up
    digraph \|v 8595 " arrow down

    " }}}
    " terminal mode {{{

    autocmd User MyVimRc tnoremap <C-l>      <C-\><C-n>
    autocmd User MyVimRc tnoremap <C-\><C-n> <Esc>
    autocmd User MyVimRc tnoremap <C-[>      <Esc>
    autocmd User MyVimRc tnoremap <C-]>      <C-l>

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


"---------"
" Finally "
"---------"
" {{{

" Generate the help tags
if isdirectory(g:vimrc['vim_home'] . '/doc')
    execute 'helptags' (g:vimrc['vim_home'] . '/doc')
endif

nohlsearch
filetype plugin indent on
syntax enable
doautocmd User MyVimRc

" }}}

let g:vimrc['loaded'] = 1
