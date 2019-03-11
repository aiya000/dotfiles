" NOTE: Don't use the mark Z, because this is often used by my functions

"---------------"
" Global values "
"---------------"
" {{{

" Open in preference to an entity
let $MYVIMRC = filereadable(expand('~/.dotfiles/.vimrc'))
  \ ? expand('~/.dotfiles/.vimrc')
  \ : $MYVIMRC

let $MYGVIMRC = filereadable(expand('~/.dotfiles/.gvimrc'))
  \ ? expand('~/.dotfiles/.gvimrc')
  \ : $MYGVIMRC

" Global values
let g:vimrc = get(g:, 'vimrc', {
  \ 'loaded': 0,
  \ 'vim_home': expand('~/.vim'),
  \ 'path_at_started': getcwd(),
  \ 'gui_editor': has('nvim') ? 'gonvim' : 'gvim',
  \ 'backupdir': expand('~/.backup/vim-backup'),
  \ 'is_unix': has('unix'),
  \ 'is_macos': has('macunix'),
  \ 'is_wsl': executable('uname') && (system('uname -a') =~# 'Microsoft'),
  \ 'is_windows': has('win32'),
  \ 'is_kaoriya': has('kaoriya'),
\ })

let g:vimrc['open_on_gui'] =
  \ g:vimrc['is_macos']   ? 'open' :
  \ g:vimrc['is_windows'] ? 'start' :
  \ g:vimrc['is_unix']    ? 'xdg-open' :
    \ 'no method for GUI-open'
let g:vimrc['directory']  = g:vimrc['backupdir'] . '/swp'
let g:vimrc['undodir']    = g:vimrc['backupdir'] . '/undo'
let g:vimrc['viewdir']    = g:vimrc['backupdir'] . '/view'
let g:vimrc['sessiondir'] = g:vimrc['backupdir'] . '/session'

" Please see ~/.sh_generic/aliases.sh
let $I_AM_ON_VIM = 1

" }}}


"-----------"
" Preparing "
"-----------"
" Set encodings {{{

if !g:vimrc['loaded']
  set fileencoding=utf-8 encoding=utf-8
endif

scriptencoding utf-8

" }}}
" Declare autocmd groups {{{

augroup VimRc
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

" Copy the dein.vim's document
let s:dein_doc_from = s:dein_dirname . '/doc/dein.txt'
let s:dein_doc_to   = g:vimrc['vim_home'] . '/doc/dein.txt'
if filereadable(s:dein_doc_from) && !filereadable(s:dein_doc_to)
  call writefile(readfile(s:dein_doc_from), s:dein_doc_to)
endif
unlet s:dein_doc_from s:dein_doc_to

unlet s:dein_dirname

" }}}
" Check backup directories {{{

if !isdirectory(g:vimrc.directory)
  call mkdir(g:vimrc.directory, 'p', 0700)
  call system(printf('chown -R %s:%s', $USER, $GROUP, g:vimrc.directory))
endif

if !isdirectory(g:vimrc.undodir)
  call mkdir(g:vimrc.undodir, '', 0700)
  call system(printf('chown -R %s:%s', $USER, $GROUP, g:vimrc.undodir))
endif

if !isdirectory(g:vimrc.sessiondir)
  call mkdir(g:vimrc.sessiondir, '', 0700)
  call system(printf('chown -R %s:%s', $USER, $GROUP, g:vimrc.sessiondir))
endif

" }}}


"---------"
" Plugins "
"---------"
" {{{

call dein#load_toml('~/.vim/dein.toml',      {'lazy': 0})
call dein#load_toml('~/.vim/dein_lazy.toml', {'lazy': 1})

if has('nvim')
  call dein#load_toml('~/.vim/dein-neovim.toml',      {'lazy': 0})
  call dein#load_toml('~/.vim/dein_lazy-neovim.toml', {'lazy': 1})
endif

call dein#add('Shougo/dein.vim', {'rtp': ''})

" }}}


"---------------"
" Local scripts "
"---------------"
" {{{
"NOTE: This section must be put at between dein#begin() and dein#end()

if filereadable(expand('~/.vimrc_private'))
  source ~/.vimrc_private
endif

if filereadable(expand('~/.vimrc_env'))
  source ~/.vimrc_env
endif

" }}}


"----------"
" Augroups "
"----------"
" {{{

augroup VimRc
  " Auto set cursor position in the file
  autocmd BufReadPost * call vimrc#autocmd#visit_past_position()
  autocmd BufNew,TerminalOpen,BufEnter,WinEnter * call vimrc#autocmd#lcd_buffer_dir_or_base()

  " Auto load filetype dictionary
  autocmd FileType *
    \  if filereadable(printf('%s/dict/filetype/%s.dict', g:vimrc['vim_home'], &filetype))
      \| execute 'setl dict+=' . printf('%s/dict/filetype/%s.dict', g:vimrc['vim_home'], &filetype)
    \| endif

  " RelativeNumber is used current window only
  autocmd BufEnter,WinEnter * if &number | setl relativenumber | end
  autocmd BufLeave,Winleave * setl norelativenumber

  autocmd InsertEnter * call deoplete#enable()

  " Set the 'none' filetype to the empty filetype
  autocmd VimEnter,BufNew * if empty(&ft) | setf none | endif

  "NOTE: Remove this after the auto indent bug is fixed
  autocmd FileType int-* set indentkeys-=:
augroup END

" }}}


"---------"
" Plugins "
"---------"
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
    \ 'command': g:vimrc['open_on_gui'],
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
    \ 'exec': [
      \ '%c %s %o --output /tmp/vim-quickrun-elm.html',
      \ g:vimrc['open_on_gui'] . ' /tmp/vim-quickrun-elm.html',
    \ ],
    \ 'tempfile': '%{tempname()}.elm',
  \ },
  \ 'idris': {
    \ 'cmdopt': '-p base -p prelude -p effects -p contrib',
  \ },
  \ 'happy': {
    \ 'runner': 'vimproc',
    \ 'exec': ['happy %s', 'stack runghc %s:p:r.hs'],
    \ 'hook/sweep/files': '%S:p:r.hs',
  \ },
  \ 'dhall': {
    \ 'exec': ['dhall --explain --plain %o < %s'],
  \ },
  \ 'dot': {
    \ 'runner': 'vimproc',
    \ 'exec': [
      \ 'dot -T png %o %s -o %s.png',
      \ g:vimrc['open_on_gui'] . ' %s.png',
    \ ],
    \ 'hook/sweep/files': '%S:p:r.png',
    \ 'outputter/error/error': 'quickfix',
    \ 'outputter/error/success': 'message',
  \ }
\ }

" Append config of each environment
if g:vimrc['is_windows']
  call vimrc#plugins#append_config_quickrun_windows()
endif

" }}}
" --- TweetVim --- {{{

let g:tweetvim_async_post = 1
let g:twibill_use_job     = 1

" Avoid unite-tweetvim lazy loading error
let g:tweetvim_config_dir = expand('~/.tweetvim')

" }}}
" --- vimshell.vim --- {{{

let g:vimshell_no_save_history_commands = {
  \ 'history': 1,
  \ 'ls'     : 1,
  \ 'clear'  : 1
\ }
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
" --- foldCC --- {{{

let g:foldCCtext_maxchars = 120

" }}}
" --- vim-submode --- {{{

let g:submode_timeout = 0

call submode#enter_with('window_resize', 'n', '', '<C-s>w')
call submode#map('window_resize', 'n', '', 'j', '3<C-w>+')
call submode#map('window_resize', 'n', '', 'k', '3<C-w>-')
call submode#map('window_resize', 'n', '', 'h', '3<C-w><')
call submode#map('window_resize', 'n', '', 'l', '3<C-w>>')

call submode#enter_with('tab_move', 'n', 's', '<C-s>n', ':<C-u>call vimrc#keys#move_tab_next()<CR>')
call submode#enter_with('tab_move', 'n', 's', '<C-s>p', ':<C-u>call vimrc#keys#move_tab_prev()<CR>')
call submode#map('tab_move', 'n', 's', 'n', ':<C-u>call vimrc#keys#move_tab_next()<CR>')
call submode#map('tab_move', 'n', 's', 'p', ':<C-u>call vimrc#keys#move_tab_prev()<CR>')

call submode#enter_with('window_move', 'n', 's', '<C-s>N', ':<C-u>call vimrc#keys#move_window_forward()<CR>')
call submode#enter_with('window_move', 'n', 's', '<C-s>P', ':<C-u>call vimrc#keys#move_window_backward()<CR>')
call submode#map('window_move', 'n', 's', 'N', ':<C-u>call vimrc#keys#move_window_forward()<CR>')
call submode#map('window_move', 'n', 's', 'P', ':<C-u>call vimrc#keys#move_window_backward()<CR>')
call submode#map('window_move', 'n', 'e', 'H', '"\<C-w>H" . (foldlevel(".") > 0 ? "zO" : "") . "zz"')
call submode#map('window_move', 'n', 'e', 'J', '"\<C-w>J" . (foldlevel(".") > 0 ? "zO" : "") . "zz"')
call submode#map('window_move', 'n', 'e', 'K', '"\<C-w>K" . (foldlevel(".") > 0 ? "zO" : "") . "zz"')
call submode#map('window_move', 'n', 'e', 'L', '"\<C-w>L" . (foldlevel(".") > 0 ? "zO" : "") . "zz"')
call submode#map('window_move', 'n', 's', '_', '<C-w>_')
call submode#map('window_move', 'n', 's', '"', ':resize 5<CR>')
" }}}
" --- vimdoc-ja --- {{{

" vimdoc-ja is secondary order
set helplang=en,ja

" }}}
" --- vim-colors-solarized --- {{{

" Use dark color
let g:solarized_contrast = 'high'

" }}}
" --- aho-bakaup.vim --- {{{

let g:bakaup_backup_dir  = g:vimrc['backupdir']
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
" --- unite-tag --- {{{

" Fully showing name
let g:unite_source_tag_max_name_length  = 100
let g:unite_source_tag_max_fname_length = 100

" }}}
" --- deoplete.nvim --- {{{

let g:deoplete#enable_at_startup = 1

" }}}
" --- vim-visualstar --- {{{ 
" Do zzzv after execute visualstar
let g:visualstar_extra_commands = 'zzzv'

" }}}
" --- vim-gista --- {{{

" Don't ask description for :Gista post
let g:gista#command#post#interactive_description = 0
let g:gista#command#post#allow_empty_description = 1

augroup VimRc
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

let g:ale_vim_vint_show_style_issues = v:false

let g:ale_linters = {
  \ 'haskell': ['hlint', 'stack ghc'],
  \ 'dhall' : ['dhall lint'],
  \ 'html': ['htmlhint', 'tidy'],
  \ 'css': ['csslint', 'stylelint'],
  \ 'kotlin': ['ktlint'],
  \ 'java': ['checkstyle', 'google-java-format', 'PMD'],
\ }

let g:ale_scala_scalastyle_config = $HOME . '/.dotfiles/scalastyle_config_default.xml'

augroup VimRc
  autocmd ColorScheme * highlight ALEError ctermbg=gray ctermfg=black

  autocmd VimEnter *
    \  if filereadable('./scalastyle_config.xml') && (input('locally scalastyle_config.xml was found, Do you want to load? (y/n)') == 'y')
      \| let g:ale_scala_scalastyle_config = execute('pwd')[:-1] . '/scalastyle-config.xml'
      \| echomsg 'a scalastyle config loaded: ' . g:ale_scala_scalastyle_config
    \| endif
augroup END

" }}}
" --- elm-vim --- {{{

let g:elm_browser_command    = g:vimrc['open_on_gui']
let g:elm_format_autosave    = 1
let g:elm_make_output_file   = '/tmp/elm-vim-output.html'
let g:elm_make_show_warnings = 1
let g:elm_setup_keybindings  = 0

" }}}
" --- vim-fakeclip --- {{{

let g:fakeclip_provide_clipboard_key_mappings = g:vimrc['is_wsl']

" }}}
" --- denite.nvim --- {{{

call denite#custom#map('normal', 'Q', '<denite:quit>')
call denite#custom#map('normal', '<C-[>', '<denite:quit>')
call denite#custom#map('normal', '<C-l>', '<denite:quit>')
call denite#custom#map('normal', '<C-j>', '<CR>')
call denite#custom#map('normal', '<C-i>', '<denite:toggle_select_down>')

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
call denite#custom#source('file_rec', 'matchers', ['matcher_substring'])
call denite#custom#source('line', 'matchers', ['matcher_substring'])
call denite#custom#source('tag', 'matchers', ['matcher_substring'])
call denite#custom#source('file_mru', 'matchers', ['matcher_substring', 'matcher_ignore_globs'])
call denite#custom#source('tag', 'converters', ['converter/abbr_word'])

" NOTE: How I can ignore it without this DEPRECATED value
let g:neomru#file_mru_ignore_pattern = '^gina://.*'

augroup VimRc
  autocmd BufEnter,BufWinEnter *
    \   call denite#custom#var('outline', 'command', ['ctags'])
    \|  call denite#custom#var('outline', 'options', ['--sort=no'])
    \|  call denite#custom#var('outline', 'file_opt', '-o')
  autocmd BufEnter,BufWinEnter *.hs
    \   call denite#custom#var('outline', 'command', ['hasktags'])
    \|  call denite#custom#var('outline', 'options', ['--ctags'])
    \|  call denite#custom#var('outline', 'file_opt', '-f')
augroup END

" }}}
" --- idris-vim --- {{{

let g:idris_vim_enable_keymappings_by_default = v:false

" }}}
" --- vim-textobj-clang --- {{{

let g:textobj_clang_more_mappings = 1

" }}}
" --- vim-operator-surround --- {{{

" Please see hook_source.vim

" }}}
" --- vim-highlightedyank --- " {{{

let g:highlightedyank_highlight_duration = 200

" }}}
" --- vim-fmap --- " {{{

let g:fmap_use_default_keymappings = v:false
let g:fmap_escape_keys = ['', '', '']

augroup VimRc
  autocmd VimEnter * FNoreMap / ・
  autocmd VimEnter * FNoreMap tt …
  autocmd VimEnter * FNoreMap p （
  autocmd VimEnter * FNoreMap k 「
  autocmd VimEnter * FNoreMap K 〈
  autocmd VimEnter * FNoreMap -k 『
augroup END

" }}}
" --- vimhelpgenerator --- " {{{

let g:vimhelpgenerator_defaultlanguage = 'en'

" }}}
" --- vim-autoformat --- " {{{

let g:formatdef_dhall_format = '"dhall format"'
let g:formatters_dhall = ['dhall_format']

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
  \ 'vimspec',
  \ 'storyboard',
  \ 'aref_web',
  \ 'term-shell',
  \ 'term-sbt',
  \ 'gitcommit',
  \ 'review',
  \ 'markdown',
  \ 'haskell',
  \ 'happy',
  \ 'tweetvim',
  \ 'tweetvim_say',
  \ 'help',
  \ 'man',
  \ 'elm',
  \ 'stack_test',
  \ 'adrone_home',
\ ]

" Define colors
augroup VimRc
  autocmd VimEnter,ColorScheme * highlight IndentGuidesOdd ctermbg=60 guibg=#468F8C
  autocmd VimEnter,ColorScheme * highlight IndentGuidesEven ctermbg=60 guibg=#468F8C
augroup END

augroup VimRc
  autocmd WinEnter,BufWinEnter *
    \  if get(g:, 'vimrc#keys#indent_guides_enable', v:true)
      \| IndentGuidesEnable
    \| else
      \| IndentGuidesDisable
    \| endif
augroup END

" }}}
" --- lexima.vim --- " {{{

let g:lexima_no_default_rules = 1

" Don't it if the cursor is in a word
call map(g:lexima#default_rules, { _, keymap ->
  \ lexima#add_rule(extend(keymap, {'except': '[a-zA-Z0-9]\%#[a-zA-Z0-9]'}))
\ })

call lexima#add_rule({'char': '「', 'input_after': '」'})
call lexima#add_rule({'char': '『', 'input_after': '』'})
call lexima#add_rule({'char': '（', 'input_after': '）'})
call lexima#add_rule({'char': '｛', 'input_after': '｝'})

for s:deleter in ['<C-h>', '<BS>', '<C-w>']
  call lexima#add_rule({'char': s:deleter, 'at': '(\%#)', 'delete': 1})
  call lexima#add_rule({'char': s:deleter, 'at': '{\%#}', 'delete': 1})
  call lexima#add_rule({'char': s:deleter, 'at': '[\%#]', 'delete': 1})
  call lexima#add_rule({'char': s:deleter, 'at': '<\%#>', 'delete': 1})
  call lexima#add_rule({'char': s:deleter, 'at': "'\%#'", 'delete': 1})
  call lexima#add_rule({'char': s:deleter, 'at': '"\%#"', 'delete': 1})
  call lexima#add_rule({'char': s:deleter, 'at': '`\%#`', 'delete': 1})
endfor
unlet s:deleter

for ft in ['kotlin', 'typescript', 'typescript.tsx']
  call lexima#add_rule({'filetype': ft, 'char': '<', 'input_after': '>', 'except': '[a-zA-Z0-9]\%#[a-zA-Z0-9]'})
endfor

" }}}
" --- gina.vim --- " {{{

let g:gina#command#blame#formatter#format = '%su%=on %ti by %au %ma%in'

" }}}
" --- vim-hopping --- {{{

let g:hopping#prompt = '(*^v^) '

let g:hopping#keymapping = {
  \ "\<C-l>": "\<Esc>",
\ }

" }}}
" --- vim-altercmd --- {{{

call altercmd#load()

AlterCommand new NewOverridden
AlterCommand e[dit] EditOverridden
AlterCommand vne[w] VnewOverridden
AlterCommand ene[w] EnewOverridden
AlterCommand tabnew TabnewOverridden

" }}}
" --- vim-lsp --- " {{{

" TODO: いつか有効になってくれますように
augroup VimRc
  autocmd User lsp_setup call lsp#register_server({
    \ 'name': 'language-server-stdio.js',
    \ 'cmd': { _ -> [&shell, &shellcmdflag, 'language-server-stdio.js'] },
    \ 'root_uri': { _ -> lsp#utils#path_to_uri(
      \ lsp#utils#find_nearest_parent_file_directory(
        \ lsp#utils#get_buffer_path(),
        \ 'tsconfig.json'
      \ )
    \ )},
    \ 'whitelist': ['javascript', 'typescript', 'typescript.tsx'],
  \ })
augroup END

" }}

call dein#end()


"---------"
" Options "
"---------"
" {{{

set
  \ number
  \ relativenumber
  \ nowrap
  \ hlsearch
  \ list
  \ scrolloff=16
  \ incsearch
  \ textwidth=0
  \ tabstop=4
  \ shiftwidth=4
  \ expandtab
  \ breakindent
  \ linebreak
  \ autoindent
  \ cindent
  \ nojoinspaces
  \ previewheight=40
  \ laststatus=2
  \ wildmenu
  \ wildignorecase
  \ noruler
  \ cmdheight=2
  \ history=500
  \ nowrapscan
  \ visualbell
  \ notimeout
  \ path=.,,./**
  \ shellslash
  \ hidden
  \ browsedir=buffer
  \ spelllang=en_US,cjk
  \ suffixes=
  \ tabline=%!vimrc#set#tabline()
  \ backspace=indent,eol,start
  \ listchars=tab:»_,trail:_,extends:»,precedes:«,nbsp:%,eol:↲
  \ fileencodings=ucs-bom,utf-8,sjis,euc-jp,cp932,iso-2022-jp-3,iso-2022-jp,eucjp-ms,euc-jisx0213,ucs-bom,latin1,default
  \ matchpairs+=<:>,（:）,｛:｝,「:」,＜:＞,『:』
  \ termwinkey=<F1>

" See ':h hl-User1..9' for what is the pattern of '%n*string%*' (n is a naturalnumer)
let &statusline =
  \ '%1*[%F(%n)]%*' .
  \ '%2*[FT=%y]%*' .
  \ '[%l,%v]' .
  \ '[Fenc=%{&fileencoding}]' .
  \ '[Enc=%{&encoding}]' .
  \ '%3*%m%*' .
  \ '%4*%r%*'

" ☆
set ambiwidth=double

" Define my highlight colors
" {{{

augroup VimRc
  autocmd ColorScheme * highlight RcEmSpace ctermbg=LightBlue
  autocmd VimEnter,WinEnter * call matchadd('RcEmSpace', '　')
  " git conflictions
  autocmd ColorScheme * call matchadd('Error', '^\(<\|=\|>\)\{7\}\([^=].\+\)\?$')
augroup END

augroup VimRc
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
set foldtext=FoldCCtext()
  \ foldcolumn=1
  \ foldopen=search,jump,mark,percent,insert,tag,undo
  \ foldclose=all
  \ foldmethod=marker
let &fillchars = 'vert:|,fold: '

" The backup options
let &directory = g:vimrc.directory
let &viewdir = g:vimrc.viewdir
set undofile
let &undodir = g:vimrc.undodir

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
\ ], ',')

" }}}


"---------"
" Keymaps "
"---------"
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
nnoremap <silent> <C-k><C-o> :<C-u>EditOverridden %<CR>
nnoremap <silent> <C-k>o :<C-u>EditOverridden! %<CR>
nnoremap <silent> <leader>b :<C-u>NewOverridden \| resize 5 \| setl buftype=nofile \| setl filetype=markdown \| setl syntax=<CR>
nnoremap <silent> <leader>B :<C-u>Memo<CR>
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
\ ]

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
nnoremap <silent> <leader>v :<C-u>call vimrc#open_terminal_as('term-shell', 'vertical', &shell)<CR>
nnoremap <silent> <leader><leader>v :<C-u>call vimrc#open_terminal_as('term-shell', 'horizontal', &shell)<CR>
nnoremap <silent> <leader>V :<C-u>call vimrc#open_terminal_as('term-shell', 'stay', &shell)<CR>
nnoremap <silent> <leader><leader>V :<C-u>call vimrc#open_terminal_as('term-shell', 'tabnew', &shell)<CR>
nnoremap <silent> 'v :<C-u>call vimrc#open_terminal_as('term-shell', 'vertical', &shell, {'path': g:vimrc.path_at_started})<CR>
nnoremap <silent> ''v :<C-u>call vimrc#open_terminal_as('term-shell', 'horizontal', &shell, {'path': g:vimrc.path_at_started})<CR>
nnoremap <silent> 'V :<C-u>call vimrc#open_terminal_as('term-shell', 'stay', &shell, {'path': g:vimrc.path_at_started})<CR>
nnoremap <silent> ''V :<C-u>call vimrc#open_terminal_as('term-shell', 'tabnew', &shell, {'path': g:vimrc.path_at_started})<CR>

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
nnoremap Y yg_
nnoremap { {zv
nnoremap } }zv
nnoremap ( (zv
nnoremap ) )zv
nnoremap zs zszh
nnoremap g_ $
nnoremap 'p "+p
nnoremap 'P "+P
nnoremap 'y "+y
nnoremap 'Y "+yg_
nnoremap 'd "+d
nnoremap 'D "+D
nnoremap <C-n> gt
nnoremap <C-p> gT
nnoremap <C-m> o<Esc>
nnoremap g<C-]> <C-]>
nnoremap <silent> ! :!<CR>
nnoremap <silent> <C-k><Space> :<C-u>call vimrc#keys#clear_ends_space()<CR>
nnoremap <silent> <Space><Space> :<C-u>call vimrc#keys#compress_spaces()<CR>
nnoremap <silent> <C-k><C-j> :<C-u>write<CR>
nnoremap <silent> <C-k>J :<C-u>wall \| echo 'written all !'<CR>
nnoremap <silent> <leader>o :<C-u>copen<CR>
"" Visual a last pasted range
nnoremap <expr> gp '`[' . strpart(getregtype(), 0, 1) . '`]'

  " }}}
" insert mode {{{

" set
inoremap <silent> <C-k><C-w> <C-o>:setl wrap! wrap?<CR>
inoremap <silent> <C-k><C-e> <C-o>:setl expandtab! expandtab?<CR>

" fake digraphs
inoremap <C-k>++ ＋
inoremap <C-k>?= ≒

" others
imap <C-j> <CR>
inoremap <C-l> <Esc>
inoremap <C-a> <Right>
inoremap <C-r>' <C-r>+
inoremap <C-k><C-k> <C-o>"_d$
inoremap <silent> <C-k><C-j> <Esc>:write<CR>
inoremap <silent> <C-k>J <Esc>:wall \| echo 'written all !'<CR>
inoremap <silent><expr> <C-b> vimrc#keys#get_webpage_title()

" }}}
" command-line mode {{{

cmap <C-]> \m\C\<\><Left><Left>
cnoremap <C-b> <Left>
cnoremap <C-f> <Right>
cnoremap <C-a> <Home>
cnoremap <C-h> <BS>
cnoremap <C-d> <Del>
cnoremap <C-e> <End>
cnoremap <C-k><C-k> <C-\>e getcmdpos() < 2 ? '' : getcmdline()[ : getcmdpos() - 2]<CR>
" TODO: Escape from ex
cnoremap <C-l> <C-c>
cnoremap <C-g> '<,'>
cnoremap <C-o> <Up>
cnoremap <C-y> <Down>
cnoremap <C-r>' <C-r>+

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
vnoremap g_ $
vnoremap 'p "+p
vnoremap 'P "+P
vnoremap 'y "+y
vnoremap 'd "+d

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
digraph (( 8834  " right includes left
digraph )) 8835  " left includes right
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
digraph xx 215   " product
digraph pi 960   " pi

" }}}
" plugins {{{

" netrw
nnoremap <silent> <leader>e         :<C-u>call vimrc#keys#toggle_netrw_vexplorer()<CR>
nnoremap <silent> <leader><leader>e :<C-u>execute ':Sexplore' getcwd()<CR>
nnoremap <silent> <leader>E         :<C-u>execute ':Explore' getcwd()<CR>
nnoremap <silent> <leader><leader>E :<C-u>execute ':Texplore' getcwd()<CR>
nnoremap <silent> 'e  :<C-u>call vimrc#keys#execute_on_base_path(function('vimrc#keys#toggle_netrw_vexplorer'))<CR>
nnoremap <silent> ''e :<C-u>call vimrc#keys#execute_on_base_path({ -> execute(':Sexplore ' . fnameescape(getcwd()))})<CR>
nnoremap <silent> 'E  :<C-u>call vimrc#keys#execute_on_base_path({ -> execute(':Explore ' . fnameescape(getcwd()))})<CR>
nnoremap <silent> ''E :<C-u>call vimrc#keys#execute_on_base_path({ -> execute(':Texplore ' . fnameescape(getcwd()))})<CR>

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
nnoremap <silent> <C-]> :<C-u>execute printf('Denite tag -input=%s', expand('<cword>'))<CR>
nnoremap <silent> <C-k>e :<C-u>Denite file/rec<CR>
nnoremap <silent> <C-k><C-e> :<C-u>Denite file<CR>
nnoremap <silent> '<C-k>e :<C-u>call vimrc#keys#execute_on_base_path(function('denite#start'), [{'name':'file/rec','args':[]}])<CR>
nnoremap <silent> '<C-k><C-e> :<C-u>call vimrc#keys#execute_on_base_path(function('denite#start'), [{'name':'file','args':[]}])<CR>
nnoremap <silent> <C-k><C-t> :<C-u>Denite tag<CR>
nnoremap <silent> <C-k><C-f> :<C-u>Denite outline<CR>
nnoremap <silent> <C-k>f :<C-u>Denite filetype<CR>
nnoremap <silent> H :<C-u>Denite line<CR>
nnoremap <silent> M :<C-u>Denite file_mru<CR>
nnoremap <silent> L :<C-u>Denite buffer<CR>

" aref-web.vim
"nnoremap <leader>K :<C-u>Aref weblio <C-r>=expand('<cword>')<CR><CR>
" NOTE: Avoid gonvim crushing
nmap <leader>K viw[K
vnoremap <leader>K "zy:<C-u>Aref weblio <C-r>z<CR>
vnoremap <leader>S "zy:<C-u>Aref stackage <C-r>z<CR>

" vim-hopping
nnoremap <expr> <C-k><C-s> printf(':HoppingStart --input=\m\C\<%s\>/', expand('<cword>')) . "\<CR>"
nnoremap <expr> <C-k>s printf(':HoppingStart --input=\m\C\<%s\>/%s', expand('<cword>'), expand('<cword>')) . "\<CR>"
nnoremap :%s/ :<C-u>HoppingStart<CR>
nnoremap :: :<C-u>HoppingStart<CR>

" anzu-chan
"" always n moves to forward / N moves to backward
nmap <expr> n (v:searchforward ? '<Plug>(anzu-n-with-echo)' : '<Plug>(anzu-N-with-echo)') . 'zv'
nmap <expr> N (v:searchforward ? '<Plug>(anzu-N-with-echo)' : '<Plug>(anzu-n-with-echo)') . 'zv'
nmap * <Plug>(anzu-star-with-echo)zv
nmap # <Plug>(anzu-sharp-with-echo)zv

" undotree
nnoremap <silent> <leader>U :<C-u>UndotreeToggle<CR>

" vim-indent-guides
nnoremap <silent> <C-h><C-i> :<C-u>call vimrc#keys#toggle_indent_guides()<CR>

" deoplete.nvim
inoremap <CR> <CR>
inoremap <Tab> <Tab>
inoremap <expr> <C-y> deoplete#mappings#cancel_popup() . '<C-y>'
inoremap <expr> <C-e> deoplete#mappings#cancel_popup() . '<C-e>'
imap <expr> <C-k><C-i> deoplete#toggle() ? '' : ''

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
"" basic between
vmap a* <Plug>(textobj-between-a)*
vmap i* <Plug>(textobj-between-i)*
omap a* <Plug>(textobj-between-a)*
omap i* <Plug>(textobj-between-i)*

" vim-open-googletranslate
vnoremap <silent> <leader>k "zy:OpenGoogleTranslate <C-r>z<CR>

" ale
nnoremap <silent> <C-k><C-a> :<C-u>ALEToggle<CR>
nnoremap <silent> <C-k>a :<C-u>call vimrc#keys#toggle_ale_at_buffer()<CR>
" but this doesn't overwrite diff keymaps, please see <C-h><C-d> and vimrc#keys#toggle_diff()
nmap [c :<C-u>ALEPrevious<CR>
nmap ]c :<C-u>ALENext<CR>

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
nnoremap <silent> <leader>gC :<C-u>GCommitAmmend<CR>
nnoremap <silent> <leader>ga :<C-u>GAddPatch<CR>
nnoremap <silent> <leader>gl :<C-u>GLog<CR>
nnoremap <silent> <leader>gL :<C-u>GLogPatch<CR>
nnoremap <silent> <leader>go :<C-u>GLogOneline --pretty='%h %ad %s' --date='format:%Y-%m-%d %H:%M'<CR>
nnoremap <silent> <leader>gd :<C-u>GDiff<CR>
nnoremap <silent> <leader>gb :<C-u>GBrahcnAll<CR>
nnoremap <silent> <leader>gt :<C-u>GLogTree<CR>
nnoremap <silent> <leader>gT :<C-u>GLogTreeAll<CR>

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
vmap ga <Plug>(operator-surround-append)
omap ga <Plug>(operator-surround-append)
nmap <silent> ga :<C-u>call vimrc#keys#append_surround('viw')<CR>
nmap <silent> gs :<C-u>call vimrc#keys#append_surround('viW')<CR>
nmap <silent> ds :<C-u>call vimrc#keys#delete_mostly_inner()<CR>
nmap <silent> cs :<C-u>call vimrc#keys#replace_mostly_inner()<CR>

" vim-textobj-jabraces
vmap ijp <Plug>(textobj-jabraces-parens-i)
vmap ajp <Plug>(textobj-jabraces-parens-a)
omap ijp <Plug>(textobj-jabraces-parens-i)
omap ajp <Plug>(textobj-jabraces-parens-a)

" TODO: Why this doesn't work
vmap ijP <Plug>(textobj-jabraces-brackets-i)
vmap ajP <Plug>(textobj-jabraces-brackets-a)
omap ijP <Plug>(textobj-jabraces-brackets-i)
omap ajP <Plug>(textobj-jabraces-brackets-a)

" neosnippet.vim
inoremap <silent><expr> <C-s> neosnippet#mappings#expand_or_jump_impl()
snoremap <silent><expr> <C-s> neosnippet#mappings#expand_or_jump_impl()

" NOTE: These overrides a part of the marking keys.
" vim-fmap
nmap 'f <Plug>(fmap-forward-f)
nmap 'F <Plug>(fmap-backward-f)
nmap 't <Plug>(fmap-forward-t)
nmap 'T <Plug>(fmap-backward-T)

vmap 'f <Plug>(fmap-forward-f)
vmap 'F <Plug>(fmap-backward-f)
vmap 't <Plug>(fmap-forward-t)
vmap 'T <Plug>(fmap-backward-T)

omap 'f <Plug>(fmap-forward-f)
omap 'F <Plug>(fmap-backward-f)
omap 't <Plug>(fmap-forward-t)
omap 'T <Plug>(fmap-backward-T)

" }}}
" filetypes {{{

augroup VimRc
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

execute 'helptags' (g:vimrc['vim_home'] . '/doc')
nohlsearch
filetype plugin indent on
syntax enable
let g:vimrc['loaded'] = 1
