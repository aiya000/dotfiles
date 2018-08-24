"----------------------------"
"  This file supports        "
"    - *NIX (exclude MacOSX) "
"    - Cygwin                "
"    - Windows Kaoriya GVim  "
"    - NeoVim                "
"----------------------------"

"------"
" Memo "
"------"
" {{{
"
" Please see .vim/autoload/vimrc/dein/hook_source.vim if you can't find a preference of dein lazily plugins
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

" TODO: Unify augroups to one, because I may not need spliting augroups

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
    \ '_': {
        \ 'split': '',
        \ 'runner': 'system',
        \ 'runner/vimproc/updatetime': 10,
        \ 'hook/time/enable': 1,
        \ 'outputter': 'error',
        \ 'outputter/error/error': 'quickfix',
        \ 'outputter/error/success': 'buffer',
    \ },
    \ 'cpp': {
        \ 'cmdopt': '-std=c++17',
    \ },
    \ 'java': {
        \ 'cmdopt': '-encoding UTF-8 -source 1.8',
    \ },
    \ 'cs': {
        \ 'command': 'mcs',
    \ },
    \ 'vimspec': {
        \ 'command' : 'themis',
        \ 'cmdopt'  : '--runtimepath ".."',
        \ 'exec'    : '%c %o %s:p | tr -d "\r"',
        \ 'tempfile':  printf('%s/{tempname()}.vimspec', $TMP),
    \ },
    \ 'html': {
        \ 'command': 'xdg-open',
        \ 'outputter': 'null',
        \ 'exec'     : '%c %s:p',
    \ },
    \ 'tex': {
        \ 'command': 'ptex2pdf',
        \ 'cmdopt' : '-l',
        \ 'exec'   : '%c %o %s:r',
    \ },
    \ 'clojure': {
        \ 'command': 'lein',
        \ 'cmdopt' : 'exec',
    \ },
    \ 'swift': {
        \ 'command': 'swift',
    \ },
    \ 'scala': {
        \ 'cmdopt': '-feature'
    \ },
    \ 'brainfuck': {
        \ 'command': 'brainfuck'
    \ },
    \ 'nico': {
        \ 'command': 'nicorun'
    \ },
    \ 'haskell': {
        \ 'cmdopt': '--ghc-arg=-fprint-explicit-kinds',
        \ 'command': 'stack',
        \ 'exec': '%c exec runghc -- %o %s',
        \ 'runner': 'vimproc',
    \ },
    \ 'lhaskell': {
        \ 'command': 'stack exec runghc',
        \ 'exec': ['grep "^>.*$" %s | sed -r "s/^>//g" > %s:p:r.hs', '%c %o %s:p:r.hs'],
        \ 'tempfile': '%{tempname()}.lhs',
        \ 'hook/sweep/files': '%S:p:r.hs',
    \ },
    \ 'stack_test': {
        \ 'command': 'stack',
        \ 'cmdopt': 'test',
        \ 'exec': '%c %o',
        \ 'runner': 'vimproc',
        \ 'outputter': 'quickfix',
    \ },
    \ 'stack_build': {
        \ 'type': 'stack_test',
        \ 'cmdopt': 'build',
    \ },
    \ 'eta': {
        \ 'runner': 'vimproc',
    \ },
    \ 'etlas_build': {
        \ 'command': 'etlas',
        \ 'cmdopt': 'build',
        \ 'exec': '%c %o',
        \ 'runner': 'vimproc',
        \ 'outputter': 'quickfix',
    \ },
    \ 'elm': {
        \ 'runner': 'vimproc',
        \ 'command': 'elm-make',
        \ 'cmdopt': '--warn',
        \ 'exec': ['%c %s %o --output /tmp/vim-quickrun-elm.html', 'xdg-open /tmp/vim-quickrun-elm.html'],
        \ 'tempfile': '%{tempname()}.elm',
    \ },
    \ 'idris': {
        \ 'cmdopt': '-p base -p prelude -p effects -p contrib',
    \ },
    \ 'happy': {
        \ 'runner': 'vimproc',
        \ 'exec': ['happy %s', 'stack runghc %s:p:r.hs'],
        \ 'hook/sweep/files': '%S:p:r.hs',
    \ }
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
    call submode#enter_with('window_resize', 'n', '', '<C-s>w')
    call submode#map('window_resize', 'n', '', 'j', '3<C-w>+')
    call submode#map('window_resize', 'n', '', 'k', '3<C-w>-')
    call submode#map('window_resize', 'n', '', 'h', '3<C-w><')
    call submode#map('window_resize', 'n', '', 'l', '3<C-w>>')

    call submode#enter_with('buffer_change', 'n', '', '<C-s>b')
    call submode#map('buffer_change', 'n', 's', 'n', ':bnext<CR>')
    call submode#map('buffer_change', 'n', 's', 'p', ':bprevious<CR>')
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
    \ 'weblio': {
        \ 'url': 'https://ejje.weblio.jp/content/%s',
    \ },
    \ 'stackage': {
        \ 'url': 'https://www.stackage.org/lts-10.9/hoogle?q=%s&page=1',
    \ },
    \ 'hoogle': {
        \ 'url': 'https://www.haskell.org/hoogle/?hoogle=%s',
    \ },
    \ 'shellcheck': {
        \ 'url': 'https://github.com/koalaman/shellcheck/wiki/%s',
    \ },
    \ 'elm-search': {
        \ 'url': 'http://klaftertief.github.io/elm-search/?q=%s',
    \ },
\ }

let g:aref_web_buffer_opening = 'tabnew'

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
   \ 'haskell': ['hlint', 'stack ghc'],
   \ 'html': ['htmlhint', 'tidy'],
   \ 'css': ['csslint', 'stylelint'],
   \ 'kotlin': ['ktlint'],
   \ 'java': ['checkstyle', 'google-java-format', 'PMD'],
\}

let g:ale_scala_scalastyle_config = $HOME . '/.dotfiles/scalastyle_config_default.xml'

augroup PluginPrefs
    autocmd VimEnter *
        \  if filereadable('./scalastyle_config.xml')
            \| let g:ale_scala_scalastyle_config = execute('pwd')[:-1] . '/scalastyle_config.xml'
        \| endif
augroup END

" }}}
" --- elm-vim --- {{{

let g:elm_browser_command    = 'xdg-open'
let g:elm_format_autosave    = 1
let g:elm_make_output_file   = '/tmp/elm-vim-output.html'
let g:elm_make_show_warnings = 1
let g:elm_setup_keybindings  = 0

" }}}
" --- vim-fakeclip --- {{{

let fakeclip_provide_clipboard_key_mappings = g:vimrc['is_wsl']

" }}}
" --- denite.nvim --- {{{

call denite#custom#map('normal', '<C-[>', '<denite:quit>')
call denite#custom#map('normal', '<C-l>', '<denite:quit>')
call denite#custom#map('normal', '<C-j>', '<CR>')

call denite#custom#map('insert', '<C-[>', '<denite:enter_mode:normal>')
call denite#custom#map('insert', '<C-l>', '<denite:quit>')
call denite#custom#map('insert', '<C-j>', '<CR>')
call denite#custom#map('insert', '<C-n>', '<denite:move_to_next_line>')
call denite#custom#map('insert', '<C-p>', '<denite:move_to_previous_line>')
call denite#custom#map('insert', '<C-a>', '<denite:move_caret_to_head>')
call denite#custom#map('insert', '<C-e>', '<denite:move_caret_to_tail>')
call denite#custom#map('insert', '<C-f>', '<denite:move_caret_to_right>')
call denite#custom#map('insert', '<C-b>', '<denite:move_caret_to_left>')
call denite#custom#map('insert', '<C-x>', '<denite:do_action:delete>')

call denite#custom#source('buffer', 'matchers', ['matcher_substring'])
call denite#custom#source('file', 'matchers', ['matcher_substring'])
call denite#custom#source('file_mru', 'matchers', ['matcher_substring'])
call denite#custom#source('file_rec', 'matchers', ['matcher_substring'])
call denite#custom#source('line', 'matchers', ['matcher_substring'])
call denite#custom#source('tag', 'matchers', ['matcher_substring'])

augroup PluginPrefs
    autocmd BufEnter,BufWinEnter *
        \   call denite#custom#var('outline', 'command', ['ctags'])
        \|  call denite#custom#var('outline', 'options', ['--sort=no'])
        \|  call denite#custom#var('outline', 'file_opt', '-o')
    autocmd BufEnter,BufWinEnter *.hs
        \   call denite#custom#var('outline', 'command', ['hasktags'])
        \|  call denite#custom#var('outline', 'options', ['--ignore-close-implementation', '--ctags', '-x'])
    autocmd BufEnter,BufWinEnter *.md,*.markdown
        \   call denite#custom#var('outline', 'command', ['markdown2ctags.py'])
        \|  call denite#custom#var('outline', 'options', ['--sort=no']) " Keep the sections order that is top to bottom
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
    \ 'javascript': ['javascript-typescript-stdio'],
\}

" }}}
" --- vim-espeak --- " {{{

let g:espeak_speed = 100
let g:espeak_voice = 'en-us'
"let g:espeak_voice = 'ja'

"autocmd! InsertLeave * execute 'EspeakNgSay' getline('.')

" }}}
" --- vim-highlightedyank --- " {{{

let g:highlightedyank_highlight_duration = 200

" }}}
" --- vim-fmap --- " {{{

let g:fmap_mappings = [
    \ { 'strokes': ['p'], 'target': '（' },
    \ { 'strokes': ['k'], 'target': '「' },
    \ { 'strokes': ['K'], 'target': '『' },
\ ]

augroup PluginPrefs
    autocmd VimEnter * FNoreMap . 。
    autocmd VimEnter * FNoreMap , 、
    autocmd VimEnter * FNoreMap ! ！
    autocmd VimEnter * FNoreMap ? ？
augroup END

" }}}
" --- vimhelpgenerator --- " {{{

let g:vimhelpgenerator_defaultlanguage = 'en'

" }}}
" --- vim-autoformat --- " {{{

" let g:formatdef_scalafmt = '"ng scalafmt --stdin"' " Please see Makefile for about `ng scalafmt`
" let g:formatters_scala = ['scalafmt']

" }}}
" --- vim-indent-guides --- " {{{

let g:indent_guides_enable_on_vim_startup = 1
let g:indent_guides_start_level = 2
let g:indent_guides_default_mapping = 0
let g:indent_guides_guide_size = 1
let g:indent_guides_auto_colors = 0
let g:indent_guides_tab_guides = 0
let g:indent_guides_exclude_filetypes = [
   \ 'xml',
   \ 'html',
   \ 'xaml',
   \ 'fxml',
   \ 'gradle',
   \ 'csproj',
   \ 'json',
   \ 'vimspec',
   \ 'storyboard',
   \ 'aref_web',
   \ 'term-shell',
   \ 'term-sbt',
   \ 'gitcommit',
   \ 'review',
   \ 'markdown',
   \ 'haskell',
\ ]

" Define colors
augroup HighlightPref
    autocmd VimEnter,ColorScheme * highlight IndentGuidesOdd ctermbg=60
    autocmd VimEnter,ColorScheme * highlight IndentGuidesEven ctermbg=60
augroup END

augroup FileEvent
    autocmd WinEnter,BufWinEnter *
        \  if get(g:, 'vimrc#keys#indent_guides_enable', v:true)
            \| IndentGuidesEnable
        \| else
            \| IndentGuidesDisable
        \| endif
augroup END

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
set tabline=%!vimrc#set#tabline()
set backspace=indent,eol,start
set nowrapscan visualbell notimeout
set fileencodings=ucs-bom,utf-8,sjis,euc-jp,cp932,iso-2022-jp-3,iso-2022-jp,eucjp-ms,euc-jisx0213,ucs-bom,latin1,default
set path=.,,./** shellslash
set matchpairs+=<:>,（:）,｛:｝,「:」,＜:＞,『:』
set browsedir=buffer spelllang=en_US,cjk suffixes=
set hidden

if !has('nvim')
    set termkey=<C-z>
endif

" See ':h hl-User1..9' for what is the pattern of '%n*string%*' (n is a naturalnumer)
let &statusline = '%1*[%F(%n)]%*'
    \           . '%2*[FT=%y]%*'
    \           . '[%l,%v]'
    \           . '[Fenc=%{&fileencoding}]'
    \           . '[Enc=%{&encoding}]'
    \           . '%3*%m%*'
    \           . '%4*%r%*'

" ☆
set ambiwidth=double

" Define my highlight colors
" {{{

augroup HighlightPref
    autocmd ColorScheme * highlight RcEmSpace ctermbg=LightBlue
    autocmd VimEnter,WinEnter * call matchadd('RcEmSpace', '　')
    " git conflicts
    autocmd ColorScheme * call matchadd('Error', '^\(<\|=\|>\)\{7\}\([^=].\+\)\?$')
augroup END

augroup HighlightPref
    autocmd InsertEnter * highlight StatusLine ctermfg=231 ctermbg=64
    autocmd InsertLeave * highlight StatusLine ctermfg=231 ctermbg=60
augroup END

" }}}

" Set the colorscheme, but it is set only once
if !g:vimrc['loaded']
    set background=dark
    colorscheme lucariox
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

" TODO: This option doesn't work finely (?)
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
"autocmd UserGettingBored * echo 'sugar'

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

" Eta
AlterCommand etr !etlas run

" }}}
" CmdCnoreabbr {{{

" aref-web.vim
CmdCnoreabbr Hoogle     Aref hoogle
CmdCnoreabbr ShellCheck Aref shellcheck
CmdCnoreabbr Stackage   Aref stackage
CmdCnoreabbr Weblio     Aref weblio
CmdCnoreabbr ElmSearch  Aref elm-search

" vim-open-googletranslate
CmdCnoreabbr Google OpenGoogleTranslate

" TweetVim
CmdCnoreabbr SwitchAccount TweetVimSwitchAccount
CmdCnoreabbr UserTimeline  TweetVimUserTimeline

" Others
CmdCnoreabbr Lingr J6uil
CmdCnoreabbr LingrTab TabnewOverridden \| J6uil
CmdCnoreabbr Gist Gista post --stay
CmdCnoreabbr ReverseLines OperatorReverseLines

" }}}


"--------------------"
" Define keymappings "
"--------------------"
" virtual keymappings {{{

cnoremap [Left] <Left>
nnoremap <silent> [ale-previous] :<C-u>ALEPrevious<CR>
nnoremap <silent> [ale-next] :<C-u>ALENext<CR>

" }}}
" normal mode {{{

" Allow keymaps like <C-c>{foo}, and {bar}<C-c>
nnoremap <C-c> <NOP>
nnoremap <C-c><C-c> <C-c>

" listup
nnoremap <silent> m: :<C-u>marks<CR>
nnoremap <silent> q: :<C-u>register<CR>
nnoremap <silent> g: :<C-u>buffers<CR>
nnoremap <silent> z: :<C-u>tabs<CR>
nnoremap <silent> g> :<C-u>messages<CR>

" search
nnoremap <silent> g* :<C-u>execute 'silent! normal! *<C-o>'<CR>
nnoremap <silent> <C-k><C-l> :<C-u>nohlsearch<CR>

" open/close
nnoremap <silent> <leader>b :<C-u>NewOverridden \| resize 5 \| setl buftype=nofile \| setl filetype=markdown \| setl syntax=<CR>
nnoremap <silent> <leader>B :<C-u>sp ~/.backup/vim-memo.md<CR>
nnoremap <silent> <leader><leader>q :<C-u>call vimrc#keys#bufclose_filetype(<C-r>=string(g:vimrc.auto_closing_filetypes)<CR>)<CR>
let g:vimrc.auto_closing_filetypes = [
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
\]
nnoremap <silent> <C-k><C-o> :<C-u>EditOverridden %<CR>
nnoremap <silent> <C-k>o :<C-u>EditOverridden! %<CR>

" folds
nnoremap <expr> h foldclosed('.') > -1 ? 'zo' : 'h'
nnoremap <expr> l foldclosed('.') > -1 ? 'zo' : 'l'
nnoremap zj zjzo
nnoremap zk zkzo

" windows
" <Space> prefix
nnoremap <Space>h <C-w>h
nnoremap <Space>j <C-w>j
nnoremap <Space>k <C-w>k
nnoremap <Space>l <C-w>l
" gh prefix
nnoremap ghR <C-w>r
nnoremap <silent> ghq :<C-u>call vimrc#keys#hide_or_quit()<CR>
nnoremap <silent> ghQ :<C-u>quitall<CR>
nnoremap <silent> ghc :<C-u>bdelete<CR>
nnoremap <silent> ghC :<C-u>bdelete!<CR>
nnoremap <silent> gho :<C-u>only<CR>
nnoremap <silent> gh_ :<C-u>resize<CR>
nnoremap <silent> gh" :<C-u>resize 5<CR>
nnoremap <silent> gh' :<C-u>resize 10<CR>
nnoremap gh\| <C-w>\|
nnoremap <silent> gh\ :<C-u>vertical resize 1<CR>
nnoremap <silent> gh% :<C-u>vertical resize 20<CR>
nnoremap gh= <C-w>=
nmap gh+ gh_gh\|
nnoremap ghH <C-w>H
nnoremap ghJ <C-w>J
nnoremap ghK <C-w>K
nnoremap ghL <C-w>L
nnoremap ghs :<C-u>split<CR>
nnoremap ghv :<C-u>vsplit<CR>
nnoremap <silent><expr> gH  ('mZ:tabnew<CR>`Zzz'          . (foldlevel('.') > 0 ? 'zo' : ''))
nnoremap <silent><expr> ghh ('mZ:hide<CR>:tabnew<CR>`Zzz' . (foldlevel('.') > 0 ? 'zo' : ''))
" Disable defaults
nnoremap <C-w>q <NOP>
nnoremap <C-w>c <NOP>
nnoremap <C-w>r <NOP>
nnoremap <C-w>_ <NOP>
nnoremap <C-w>\ <NOP>
nnoremap <C-w>= <NOP>
nnoremap <C-w>o <NOP>
nnoremap <C-w>H <NOP>
nnoremap <C-w>J <NOP>
nnoremap <C-w>K <NOP>
nnoremap <C-w>L <NOP>
nnoremap <C-w>s <NOP>
nnoremap <C-w>v <NOP>
nnoremap gh <NOP>

" :terminal
nnoremap <silent> <leader>v :<C-u>call vimrc#open_terminal_as('term-shell', 'vertical', &shell, v:true)<CR>
nnoremap <silent> <leader><leader>v :<C-u>call vimrc#open_terminal_as('term-shell', 'horizontal', &shell, v:true)<CR>
nnoremap <silent> <leader>V :<C-u>call vimrc#open_terminal_as('term-shell', 'stay', &shell, v:true)<CR>
nnoremap <silent> <leader><leader>V :<C-u>call vimrc#open_terminal_as('term-shell', 'tabnew', &shell, v:true)<CR>
nnoremap <silent> <C-k><leader>v :<C-u>call vimrc#open_terminal_as('term-shell', 'vertical', &shell, v:false)<CR>
nnoremap <silent> <C-k><leader><leader>v :<C-u>call vimrc#open_terminal_as('term-shell', 'horizontal', &shell, v:false)<CR>
nnoremap <silent> <C-k><leader>V :<C-u>call vimrc#open_terminal_as('term-shell', 'stay', &shell, v:false)<CR>
nnoremap <silent> <C-k><leader><leader>V :<C-u>call vimrc#open_terminal_as('term-shell', 'tabnew', &shell, v:false)<CR>
" and vimshell
nnoremap <silent> <C-[><C-v> :<C-u>call vimrc#keys#toggle_shell_mode()<CR>

" set
nnoremap <silent> <C-h><C-d> :<C-u>call vimrc#keys#toggle_diff()<CR>
nnoremap <silent><expr> <C-h><C-v> ':setl virtualedit=' . (&virtualedit ==# '' ? 'all' : '') . ' virtualedit?<CR>'
nnoremap <silent><expr> zm ':setl foldmethod=' . (&foldmethod ==# 'marker' ? 'syntax' : 'marker') . ' foldmethod?<CR>'
nnoremap <silent> <C-h><C-w> :<C-u>setl wrap! wrap?<CR>
nnoremap <silent> <C-h><C-c> :<C-u>setl cursorline! cursorline?<CR>
nnoremap <silent> <C-h><C-r> :<C-u>setl relativenumber! relativenumber?<CR>
nnoremap <silent> <C-h><C-l> :<C-u>setl list! list?<CR>
nnoremap <silent> <C-h><C-n> :<C-u>setl number! number?<CR>
nnoremap <silent> <C-h><C-s> :<C-u>if exists("g:syntax_on") \| syntax off \| else \| syntax on \| endif<CR>

" others
nmap <C-j> <CR>
nnoremap Q gQ
nnoremap { {zv
nnoremap } }zv
nnoremap ( (zv
nnoremap ) )zv
nnoremap zs zszh
nnoremap g_ $
nnoremap <C-n> gt
nnoremap <C-p> gT
nnoremap <C-m> o<Esc>
nnoremap <C-]> g<C-]>
nnoremap g<C-]> <C-]>
nnoremap <leader>/ :<C-u>Migemo<CR>
nnoremap <silent> ! :!<CR>
nnoremap <silent> <C-k><Space> :<C-u>call vimrc#keys#clear_ends_space()<CR>
nnoremap <silent> <Space><Space> :<C-u>call vimrc#keys#compress_spaces()<CR>
nnoremap <silent> <C-k><C-r> :<C-u>Reload<CR>
nnoremap <silent> <C-k><C-j> :<C-u>write<CR>
nnoremap <silent> <C-k>J :<C-u>wall \| echo 'written all !'<CR>
nnoremap <silent> <leader>o :<C-u>copen<CR>
"" Don't stop
nnoremap gs <NOP>
"" Visual a last pasted range
nnoremap <expr> gp '`[' . strpart(getregtype(), 0, 1) . '`]'

" }}}
" insert mode {{{

" set
inoremap <silent> <C-k><C-w> <C-o>:setl wrap! wrap?<CR>
inoremap <silent> <C-k><C-e> <C-o>:setl expandtab! expandtab?<CR>

" others
imap <C-j> <CR>
inoremap <C-l> <Esc>
inoremap <C-k><C-k> <C-o>"_d$
inoremap <silent> <C-k><C-j> <Esc>:write<CR>
inoremap <silent> <C-k>J <Esc>:wall \| echo 'written all !'<CR>
inoremap <silent><expr> <C-b> vimrc#keys#get_webpage_title()

" }}}
" command-line mode {{{

cmap <C-]> \m\C\<\>[Left][Left]
cnoremap <C-b> <Left>
cnoremap <C-f> <Right>
cnoremap <C-a> <Home>
cnoremap <C-h> <BS>
cnoremap <C-d> <Del>
cnoremap <C-e> <End>
cnoremap <C-k><C-k> <C-\>e getcmdpos() < 2 ? '' : getcmdline()[ : getcmdpos() - 2]<CR>
cnoremap <C-l> <C-c>
cnoremap <C-g> '<,'>
cnoremap <C-o> <Up>
cnoremap <C-y> <Down>

" }}}
" visual/operator mode {{{

" folds
vnoremap zo zogv
vnoremap zO zOgv

" Don't select blanks
vnoremap a" 2i"
onoremap a" 2i"
vnoremap a' 2i'
onoremap a' 2i'
vnoremap a` 2i`
onoremap a` 2i`

" quotes
vnoremap ab 2i`
onoremap ab 2i`
vnoremap ib i`
onoremap ib i`

" brackets
vnoremap ap a(
onoremap ap a(
vnoremap aP a{
onoremap aP a{
vnoremap ak a[
onoremap ak a[
vnoremap aK a<
onoremap aK a<
vnoremap ip i(
onoremap ip i(
vnoremap iP i{
onoremap iP i{
vnoremap ik i[
onoremap ik i[
vnoremap iK i<
onoremap iK i<

" others
vnoremap <C-l> <Esc>
vnoremap <leader>s :sort<CR>

" }}}
" select mode {{{

snoremap <C-l> <Esc>

" }}}
" terminal mode {{{

tnoremap <C-l> <C-\><C-n>
tnoremap <C-\><C-n> <Esc>
tnoremap <C-[> <Esc>
tnoremap <C-]> <C-l>

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
" plugins {{{

" netrw
nnoremap <silent> <leader>e         :<C-u>call vimrc#keys#toggle_netrw_vexplorer(v:true)<CR>
nnoremap <silent> <leader><leader>e :<C-u>call vimrc#keys#netrw_wrapper('horizontal', v:true)<CR>
nnoremap <silent> <leader>E         :<C-u>call vimrc#keys#netrw_wrapper('stay', v:true)<CR>
nnoremap <silent> <leader><leader>E :<C-u>call vimrc#keys#netrw_wrapper('tabnew', v:true)<CR>
nnoremap <silent> <C-k><leader>e         :<C-u>call vimrc#keys#toggle_netrw_vexplorer(v:false)<CR>
nnoremap <silent> <C-k><leader><leader>e :<C-u>call vimrc#keys#netrw_wrapper('horizontal', v:false)<CR>
nnoremap <silent> <C-k><leader>E         :<C-u>call vimrc#keys#netrw_wrapper('stay', v:false)<CR>
nnoremap <silent> <C-k><leader><leader>E :<C-u>call vimrc#keys#netrw_wrapper('tabnew', v:false)<CR>

" open-browser.vim
nmap <leader>w <Plug>(openbrowser-open)
vmap <leader>w <Plug>(openbrowser-open)

" vim-quickrun
nmap              <leader>r <Plug>(quickrun)
nnoremap <silent> <leader>R :<C-u>QuickRun -runner shell<CR>
vmap              <leader>r <Plug>(quickrun)
vnoremap <silent> <leader>R :QuickRun -runner shell<CR>

" denite.nvim
nnoremap <leader>u :<C-u>Denite<Space>
nnoremap <silent> <C-k>e :<C-u>call vimrc#plugins#exec_at_this_buffer_dir('Denite file/rec')<CR>
nnoremap <silent> <C-k><C-e> :<C-u>call vimrc#plugins#exec_at_this_buffer_dir('Denite file')<CR>
nnoremap <silent> <leader><C-k>e :<C-u>Denite file/rec<CR>
nnoremap <silent> <leader><C-k><C-e> :<C-u>Denite file<CR>
nnoremap <silent> <C-k><C-t> :<C-u>Denite tag<CR>
nnoremap <silent> <C-k><C-f> :<C-u>Denite outline<CR>
nnoremap <silent> <C-k>f :<C-u>Denite filetype<CR>
nnoremap <silent> H :<C-u>Denite line<CR>
nnoremap <silent> M :<C-u>Denite file_mru<CR>
nnoremap <silent> L :<C-u>Denite buffer<CR>

" aref-web.vim
nnoremap <leader>K :<C-u>Aref weblio <C-r>=expand('<cword>')<CR><CR>
vnoremap <leader>K "zy:<C-u>Aref weblio <C-r>z<CR>
vnoremap <leader>S "zy:<C-u>Aref stackage <C-r>z<CR>

" vim-over
nnoremap <expr> <C-k><C-s> ':OverCommandLine %s/\m\C\<' . expand('<cword>') . '\>/<CR>'
nnoremap <expr> <C-k>s     ':OverCommandLine %s/\m\C\<' . expand('<cword>') . '\>/' . expand('<cword>') . '<CR>'
nnoremap <silent> :%s/     :<C-u>OverCommandLine %s/<CR>
nnoremap <silent> :s/      :<C-u>OverCommandLine s/<CR>
vnoremap <silent> :s/      :<C-u>OverCommandLine '<,'>s/<CR>
cnoremap <silent> <C-k>:   <Home>OverCommandLine <CR>
"NOTE: this is temporary keymapping, because vim-over do not imported cnoremap maybe
OverCommandLineNoremap <C-b> <Left>
OverCommandLineNoremap <C-f> <Right>
"OverCommandLineNoremap <C-k><C-k> <C-\>e getcmdpos() < 2 ? '' : getcmdline()[:getcmdpos()-2]<CR>

" anzu-chan
"" always n moves to forward / N moves to backward
nmap <expr> n (v:searchforward ? '<Plug>(anzu-n-with-echo)' : '<Plug>(anzu-N-with-echo)') . 'zv'
nmap <expr> N (v:searchforward ? '<Plug>(anzu-N-with-echo)' : '<Plug>(anzu-n-with-echo)') . 'zv'
nmap * <Plug>(anzu-star-with-echo)zv
nmap # <Plug>(anzu-sharp-with-echo)zv

" TaskList.vim
nmap <silent> <leader>t :<C-u>TodoList<CR>

" undotree
nnoremap <silent> <leader>U :<C-u>UndotreeToggle<CR>

" vim-indent-guides
nnoremap <silent> <C-h><C-i> :<C-u>call vimrc#keys#toggle_indent_guides()<CR>

" neosnippet.vim
imap <C-s> <Plug>(neosnippet_jump_or_expand)
smap <C-s> <Plug>(neosnippet_jump_or_expand)

" neocomplete.vim
inoremap <silent> <C-k><C-i> <C-o>:NeoCompleteToggle<CR>
inoremap <expr>   <CR>  neocomplete#close_popup()  . '<CR>'
inoremap <expr>   <Tab> neocomplete#close_popup()  . '<Tab>'
inoremap <expr>   <C-y> neocomplete#cancel_popup() . '<C-y>'
inoremap <expr>   <C-e> neocomplete#cancel_popup() . '<C-e>'

" vim-visualstar
vmap g* <Plug>(visualstar-*)Nzz

" textobj-indent
omap ai <Plug>(textobj-indent-a)
omap ii <Plug>(textobj-indent-i)
vmap ai <Plug>(textobj-indent-a)
vmap ii <Plug>(textobj-indent-i)
nmap <silent> <leader><leader>s vii:sort<CR>

" textobj-from_regexp
" Select an alphabet glob
vmap <expr> a_ textobj#from_regexp#mapexpr('[^A-Za-z0-9][A-Za-z0-9]\+[^A-Za-z0-9]')
vmap <expr> i_ textobj#from_regexp#mapexpr('[A-Za-z0-9]\+')
omap <expr> a_ textobj#from_regexp#mapexpr('[^A-Za-z0-9][A-Za-z0-9]\+[^A-Za-z0-9]')
omap <expr> i_ textobj#from_regexp#mapexpr('[A-Za-z0-9]\+')
" Select a line without a trailing line break
vmap <expr> al textobj#from_regexp#mapexpr('^.*$')
vmap <expr> il textobj#from_regexp#mapexpr('^\s*\zs.*\ze.*$')

" vim-textobj-between
vmap aB <Plug>(textobj-between-a)
vmap iB <Plug>(textobj-between-i)
omap aB <Plug>(textobj-between-a)
omap iB <Plug>(textobj-between-i)

" vim-open-googletranslate
vnoremap <silent> <leader>k "zy:OpenGoogleTranslate <C-r>z<CR>

" ale
nnoremap <silent> <C-k><C-a> :<C-u>ALEToggle<CR>
nnoremap <silent> <C-k>a :<C-u>call vimrc#keys#toggle_ale_at_buffer()<CR>
" but this doesn't overwrite diff keymaps, please see <C-h><C-d> and vimrc#keys#toggle_diff()
nmap [c [ale-previous]
nmap ]c [ale-next]

" incsearch.vim
nmap g/ <Plug>(incsearch-stay)
IncSearchNoreMap <C-j> <CR>
IncSearchNoreMap <C-b> <Left>
IncSearchNoreMap <C-f> <Right>
IncSearchNoreMap <C-a> <Home>
IncSearchNoreMap <C-h> <BS>
IncSearchNoreMap <C-d> <Del>
IncSearchNoreMap <C-e> <End>
IncSearchNoreMap <C-l> <C-c>
IncSearchNoreMap <C-o> <Up>
IncSearchNoreMap <C-y> <Down>

" gina.vim (and .vim/plugin/vimrc.vim)
nnoremap <silent> <leader>gs :<C-u>GStatus<CR>
nnoremap <silent> <leader>gS :<C-u>GitShowViewer<CR>
nnoremap <silent> <leader>gc :<C-u>GCommit<CR>
nnoremap <silent> <leader>gC :<C-u>GCAM<CR>
nnoremap <silent> <leader>ga :<C-u>GAP<CR>
nnoremap <silent> <leader>gl :<C-u>GLog<CR>
nnoremap <silent> <leader>gL :<C-u>GLP<CR>
nnoremap <silent> <leader>gd :<C-u>GDiff<CR>
nnoremap <silent> <leader>gb :<C-u>GBA<CR>
nnoremap <silent> <leader>gt :<C-u>GTree<CR>
nnoremap <silent> <leader>gT :<C-u>GTreeAll<CR>

" LanguageClient-neovim
nnoremap <silent> <C-q><C-d><C-g> :<C-u>LspDocumentDiagnostics<CR>
nnoremap <silent> <C-q><C-d><C-d> :<C-u>call LanguageClient_textDocument_definition()<CR>
nnoremap <silent> <C-q><C-d><C-f> :<C-u>call LanguageClient_textDocument_formatting()<CR>
nnoremap <silent> <C-q><C-d><C-r> :<C-u>call LanguageClient_textDocument_rangeFormatting()<CR>
nnoremap <silent> <C-q><C-d><C-s> :<C-u>call LanguageClient_textDocument_documentSymbol()<CR>
nnoremap <silent> <C-q><C-h> :<C-u>call LanguageClient_textDocument_hover()<CR>
nnoremap <silent> <C-q><C-i> :<C-u>LspImplementation<CR>
nnoremap <silent> <C-q><C-g> :<C-u>call LanguageClient_textDocument_references()<CR>
nnoremap <silent> <C-q><C-r> :<C-u>call LanguageClient_textDocument_rename()<CR>
nnoremap <silent> <C-q><C-w> :<C-u>call LanguageClient_workspace_symbol()<CR>

" vim-textobj-clang
" You are not i
vmap a;m i;m
omap a;m i;m
vmap a;f i;f
omap a;f i;f

" vim-alignta
vnoremap i: :Alignta =><Space>
vnoremap <silent> i= :Alignta => =/1<CR>

" vim-espeak-ng
"nnoremap <silent> <C-g><C-w> :<C-u>EspeakNgSay <C-r>=expand('<cWORD>')<CR><CR>
"nnoremap <silent> <C-g><C-g> :<C-u>EspeakNgSay <C-r>=getline('.')<CR><CR>:execute 'normal!' "\<C-g>"<CR>
"nnoremap <silent> <C-g><C-k> :<C-u>EspeakNgDoesntSay<CR>

" vim-operator-surround
vmap <silent> Sa <Plug>(operator-surround-append)
vmap <silent> Sd <Plug>(operator-surround-delete)
vmap <silent> Sc <Plug>(operator-surround-replace)
omap <silent> Sa <Plug>(operator-surround-append)
omap <silent> Sd <Plug>(operator-surround-delete)
omap <silent> Sc <Plug>(operator-surround-replace)

" vim-textobj-jabraces
vmap ijp <Plug>(textobj-jabraces-parens-i)
omap ijp <Plug>(textobj-jabraces-parens-i)
vmap ajp <Plug>(textobj-jabraces-parens-a)
omap ajp <Plug>(textobj-jabraces-parens-a)

" }}}
" filetypes {{{

augroup FileEvent
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

" }}}

let g:vimrc['loaded'] = 1
