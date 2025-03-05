" NOTE: Don't use the mark Z, this is reserved by some my functions.

let s:List = vital#vimrc#import('Data.List')
let s:Msg = vital#vimrc#import('Vim.Message')

"---------------"
" Global values "
"---------------"
" g:vimrc {{{

" Global values
let g:vimrc = get(g:, 'vimrc', #{
  \ loaded: v:false,
  \ vim_home: $'{$HOME}/.vim',
  \ path_at_started: getcwd(),
  \ is_wsl: executable('uname') && (system('uname -a') =~# 'microsoft-standard'),
  \ is_unix: has('unix'),
  \ is_macos: has('macunix'),
  \ git_root: v:null,
  \ indent_guides_enable: v:true,
  \ memo_path: expand('~/.backup/memo.md'),
\ })
call vimrc#read_git_root_to_set_g_vimrc_async()

let g:vimrc.open_on_gui =
  \ g:vimrc.is_macos ? 'open' :
  \ g:vimrc.is_wsl ? 'wslview' :
  \ g:vimrc.is_unix ? 'xdg-open' : s:Msg.warn('no method for GUI-open')

let g:vimrc.backupdir  = $'{$HOME}/.backup/vim-backup'
let g:vimrc.directory  = $'{g:vimrc.backupdir}/swp'
let g:vimrc.undodir    = $'{g:vimrc.backupdir}/undo'
let g:vimrc.viewdir    = $'{g:vimrc.backupdir}/view'
let g:vimrc.sessiondir = $'{g:vimrc.backupdir}/session'

" Please see 'vimrc#bufclose_filetype()'.
let g:vimrc.temporary_buftypes = [
  \ 'aref_web',
  \ 'diff',
  \ 'gin-branch',
  \ 'gin-log',
  \ 'gin-status',
  \ 'gitdiffviewer',
  \ 'gitlogviewer',
  \ 'gitreflogviewer',
  \ 'gitshowviewer',
  \ 'help',
  \ 'man',
  \ 'netrw',
  \ 'dirvish',
  \ 'quickrun',
  \ 'scratch',
  \ 'ddu-ff',
  \ 'ddu-filter',
  \ 'stack_build',
  \ 'fern',
\ ]

" }}}
" Others {{{

" TODO: Remove this after https://github.com/aiya000/bash-toys/issues/12 fixed
" Please see https://github.com/aiya000/bash-toys
let $BASH_TOYS_DUSTBOX_DIR = $'{$HOME}/.backup/dustbox'

let s:typescript_variants = [
  \ 'typescript',
  \ 'javascript',
  \ 'vue',
  \ 'typescript.tsx',
  \ 'javascript.jsx',
\ ]

" }}}


"-----------"
" Preparing "
"-----------"
" Set encoding {{{

if !g:vimrc.loaded
  set fileencoding=utf-8 encoding=utf-8
endif

scriptencoding utf-8

" }}}
" Prepare dein.vim {{{

" Start dein.vim
let s:dein_dirname = $'{g:vimrc.vim_home}/bundle/repos/github.com/Shougo/dein.vim'
let &runtimepath   = $'{&runtimepath},{s:dein_dirname}'

try
  call dein#begin($'{$HOME}/.vim/bundle')
catch /E117/  " If dein.vim is not found
  try
    call vimrc#fetch_dein(s:dein_dirname)
    call dein#begin($'{$HOME}/.vim/bundle')
    echo 'dein.vim installation was completed.'
    echo 'Please execute :call dein#install(),'
    echo 'and restart your vim.'
  catch /FALIED/
    call vimrc#echo_error('cloning or starting dein.vim failed.')
    call vimrc#echo_error('>> Error build vim environment <<')
  endtry
endtry

" Copy the dein.vim's document
let s:dein_doc_from = $'{s:dein_dirname}/doc/dein.txt'
let s:dein_doc_to   = $'{g:vimrc.vim_home}/doc/dein.txt'
if filereadable(s:dein_doc_from) && !filereadable(s:dein_doc_to)
  call writefile(readfile(s:dein_doc_from), s:dein_doc_to)
endif
unlet s:dein_doc_from s:dein_doc_to

unlet s:dein_dirname

" }}}
" Check backup directories {{{

if !isdirectory(g:vimrc.directory)
  call mkdir(g:vimrc.directory, 'p', 0700)
  call system($'chown -R "{$USER}:{$GROUP}" "{g:vimrc.directory}"')
endif

if !isdirectory(g:vimrc.undodir)
  call mkdir(g:vimrc.undodir, '', 0700)
  call system($'chown -R "{$USER}:{$GROUP}" "{g:vimrc.undodir}"')
endif

if !isdirectory(g:vimrc.sessiondir)
  call mkdir(g:vimrc.sessiondir, '', 0700)
  call system($'chown -R "{$USER}:{$GROUP}" "{g:vimrc.sessiondir}"')
endif

" }}}


"----------"
" dein.vim "
"----------"
" {{{

call dein#load_toml('~/.vim/dein.toml', {'lazy': 0})
call dein#load_toml('~/.vim/dein_lazy.toml', {'lazy': 1})

if filereadable($'{$HOME}/dein_env.toml')
  call dein#load_toml('~/dein_env.toml', {'lazy': 0})
endif

call dein#add('Shougo/dein.vim', {'rtp': ''})

" }}}


"---------------"
" Local scripts "
"---------------"
" {{{
" NOTE: This section must be put at between dein#begin() and dein#end()

if filereadable($'{$HOME}/.dotfiles/.private/.vimrc_private')
  source ~/.dotfiles/.private/.vimrc_private
endif

if filereadable($'{$HOME}/.vimrc_env')
  source ~/.vimrc_env
endif

" }}}


"----------"
" Augroups "
"----------"
" {{{

" Avoids nmap onto unterminal buffer
function s:nmap_p_to_put_if_on_terminal() abort
  if !s:List.has(term_list(), winbufnr('.'))
    return
  endif
  nnoremap <buffer><expr> p vimrc#put_as_stdin(@")
endfunction

" Simular to s:nmap_p_to_put_if_on_terminal()
function s:nmap_plus_p_to_put_if_on_terminal() abort
  if !s:List.has(term_list(), winbufnr('.'))
    return
  endif
  nnoremap <buffer><expr> "+p vimrc#put_as_stdin(@+)
endfunction

augroup vimrc
  autocmd!

  autocmd VimEnter * ScdCurrentDir
  autocmd BufReadPost * call vimrc#visit_past_position()
  autocmd InsertEnter * call ddc#enable()
  autocmd WinEnter,BufEnter,InsertLeave,Winleave,BufLeave vim-scratch-buffer-*.md silent! write

  if !has('nvim')
    " TODO: for any registers
    autocmd TerminalOpen * call s:nmap_p_to_put_if_on_terminal()
    autocmd TerminalOpen * call s:nmap_plus_p_to_put_if_on_terminal()
    autocmd TerminalOpen * nmap <buffer> 'p "+p
  endif

  " Show simply for terminal buffers
  autocmd TerminalOpen * setlocal nolist nonumber norelativenumber
  " TODO: When I move to another window, the terminal buffer also becomes IndentGuidesEnable in the autocmd below
  autocmd BufEnter,WinEnter *
    \  if &buftype ==# 'terminal'
      \| IndentGuidesDisable
    \| endif
  autocmd BufLeave,Winleave *
    \  setl norelativenumber
    \| IndentGuidesEnable

  " Show relative numbers only on the current window
  autocmd BufEnter,WinEnter * if &number | setl relativenumber | end
  autocmd BufLeave,Winleave * setl norelativenumber

  " Show full-width spaces
  autocmd ColorScheme * highlight EmSpace ctermbg=LightBlue guibg=LightBlue
  autocmd VimEnter,WinEnter * call matchadd('EmSpace', '　')

  " Colorize git conflicts
  autocmd ColorScheme * highlight GitConflict ctermbg=Red guibg=Red
  autocmd VimEnter,WinEnter * call matchadd('GitConflict', '^\(<\|=\|>\)\{7\}\([^=].\+\)\?$')

  " StatusLine
  autocmd InsertEnter * highlight StatusLine ctermfg=231 ctermbg=64
  autocmd InsertLeave * highlight StatusLine ctermfg=231 ctermbg=60

  " vim-indent-guides
  autocmd VimEnter,ColorScheme * highlight IndentGuidesOdd ctermbg=60 guibg=#468F8C
  autocmd VimEnter,ColorScheme * highlight IndentGuidesEven ctermbg=60 guibg=#468F8C
  " TODO: Enable this
  " autocmd WinEnter,BufWinEnter *
  "   \  if g:vimrc.indent_guides_enable && get(b:, 'vimrc_indent_guides_enable', v:true)
  "     \| IndentGuidesEnable
  "   \| else
  "     \| IndentGuidesDisable
  "   \| endif
augroup END

" }}}


"---------"
" Plugins "
"---------"
" vim-quickrun {{{

let g:quickrun_no_default_key_mappings = 0

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
    \ 'tempfile':  $TMP .. '/{tempname()}.vimspec',
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
    \ 'cmdopt': '-feature',
  \ },
  \ 'typescript': {
    \ 'command': 'ts-node',
    \ 'exec': ['%c %o %s'],
    \ 'cmdopt': '',
    \ 'tempfile': '%{tempname()}.ts',
  \ },
  \ 'brainfuck': {
    \ 'command': 'brainfuck',
  \ },
  \ 'nico': {
    \ 'command': 'nicorun',
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
      \  $'{g:vimrc.open_on_gui}  /tmp/vim-quickrun-elm.html',
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
      \  $'{g:vimrc.open_on_gui} %s.png',
    \ ],
    \ 'hook/sweep/files': '%S:p:r.png',
    \ 'outputter/error/error': 'quickfix',
    \ 'outputter/error/success': 'message',
  \ },
  \ 'python': {
    \ 'command': 'python3',
  \ },
\ }

if g:vimrc.is_wsl
  let g:quickrun_config.ps1 = #{
    \ command: 'powershell.exe',
    \ exec: ['%c `wslpath -m %s`'],
    \ tempfile: '%{tempname()}.ps1',
  \ }
endif

" }}}
" foldCC {{{

let g:foldCCtext_maxchars = 120

" }}}
" vim-submode {{{

let g:submode_timeout = 0

call submode#enter_with('win_resize', 'n', '', '<C-s>w')
call submode#map('win_resize', 'n', '', 'j', '3<C-w>+')
call submode#map('win_resize', 'n', '', 'k', '3<C-w>-')
call submode#map('win_resize', 'n', '', 'h', '3<C-w><')
call submode#map('win_resize', 'n', '', 'l', '3<C-w>>')
call submode#map('win_resize', 'n', '', '<', '20<C-w><')
call submode#map('win_resize', 'n', '', '>', '20<C-w>>')

call submode#enter_with('tab_move', 'n', 's', '<C-s>n', ':<C-u>call vimrc#move_tab_next()<CR>')
call submode#enter_with('tab_move', 'n', 's', '<C-s>p', ':<C-u>call vimrc#move_tab_prev()<CR>')
call submode#map('tab_move', 'n', 's', 'n', ':<C-u>call vimrc#move_tab_next()<CR>')
call submode#map('tab_move', 'n', 's', 'p', ':<C-u>call vimrc#move_tab_prev()<CR>')

call submode#enter_with('win_move', 'n', 's', '<C-s>N', ':<C-u>call vimrc#move_window_forward()<CR>')
call submode#enter_with('win_move', 'n', 's', '<C-s>P', ':<C-u>call vimrc#move_window_backward()<CR>')
call submode#map('win_move', 'n', 's', 'N', ':<C-u>call vimrc#move_window_forward()<CR>')
call submode#map('win_move', 'n', 's', 'P', ':<C-u>call vimrc#move_window_backward()<CR>')
call submode#map('win_move', 'n', 'e', 'H', '"\<C-w>H" .. (foldlevel(".") > 0 ? "zO" : "") .. "zz"')
call submode#map('win_move', 'n', 'e', 'J', '"\<C-w>J" .. (foldlevel(".") > 0 ? "zO" : "") .. "zz"')
call submode#map('win_move', 'n', 'e', 'K', '"\<C-w>K" .. (foldlevel(".") > 0 ? "zO" : "") .. "zz"')
call submode#map('win_move', 'n', 'e', 'L', '"\<C-w>L" .. (foldlevel(".") > 0 ? "zO" : "") .. "zz"')
call submode#map('win_move', 'n', 's', '_', '<C-w>_')
call submode#map('win_move', 'n', 's', '"', ':resize 5<CR>')

" }}}
" aho-bakaup.vim {{{

let g:bakaup_backup_dir  = g:vimrc['backupdir']
let g:bakaup_auto_backup = 1

" }}}
" neosnippet.vim {{{

let g:neosnippet#snippets_directory = $'{g:vimrc.vim_home}/neosnippets'
let g:neosnippet#disable_select_select_mappings = 1

" }}}
" vim-textobj-indent {{{

let g:textobj_indent_no_default_key_mappings = 1

" }}}
" vim-visualstar {{{

" Do zzzv after execute visualstar
let g:visualstar_extra_commands = 'zzzv'

" }}}
" autofmt {{{

set formatexpr=autofmt#japanese#formatexpr()

"}}}
" vim-textobj-between {{{

let g:textobj_between_no_default_key_mappings = 1

" }}}
" ale {{{

" ======
" Common
" ======

let g:ale_set_highlights = v:false
let g:ale_vim_vint_show_style_issues = v:false
let g:ale_virtualtext_cursor = 'current'

" =======
" Linters
" =======

" let s:ghc_standard_extensions {{{
let s:ghc_standard_extensions = [
  \ 'AutoDeriveTypeable',
  \ 'BangPatterns',
  \ 'BinaryLiterals',
  \ 'ConstraintKinds',
  \ 'DataKinds',
  \ 'DefaultSignatures',
  \ 'DeriveDataTypeable',
  \ 'DeriveFoldable',
  \ 'DeriveFunctor',
  \ 'DeriveGeneric',
  \ 'DeriveTraversable',
  \ 'DoAndIfThenElse',
  \ 'DuplicateRecordFields',
  \ 'EmptyDataDecls',
  \ 'ExistentialQuantification',
  \ 'FlexibleContexts',
  \ 'FlexibleInstances',
  \ 'FunctionalDependencies',
  \ 'GADTs',
  \ 'GeneralizedNewtypeDeriving',
  \ 'InstanceSigs',
  \ 'KindSignatures',
  \ 'LambdaCase',
  \ 'MonadFailDesugaring',
  \ 'MultiParamTypeClasses',
  \ 'MultiWayIf',
  \ 'NamedFieldPuns',
  \ 'NoImplicitPrelude',
  \ 'OverloadedStrings',
  \ 'PartialTypeSignatures',
  \ 'PatternGuards',
  \ 'PolyKinds',
  \ 'RankNTypes',
  \ 'RecordWildCards',
  \ 'ScopedTypeVariables',
  \ 'StandaloneDeriving',
  \ 'TupleSections',
  \ 'TypeApplications',
  \ 'TypeFamilies',
  \ 'TypeSynonymInstances',
  \ 'ViewPatterns',
\ ]
" }}}
let s:ale_linters_hlint =
  \ 'hlint ' ..
  \ map(s:ghc_standard_extensions, {_, ext ->
    \ '-X ' .. ext
  \ })
  \ ->join()

let g:ale_linters = #{
  \ haskell: [s:ale_linters_hlint, 'stack ghc'],
  \ dhall : ['dhall lint'],
  \ html: ['htmlhint', 'tidy'],
  \ css: ['csslint', 'stylelint'],
  \ kotlin: ['ktlint'],
  \ java: ['checkstyle', 'google-java-format', 'PMD'],
\ }

for s:ts in s:typescript_variants
  let g:ale_linters[s:ts] = ['prettier', 'eslint', 'vim-lsp']
endfor

let g:ale_scala_scalastyle_config = $'{$HOME}/.dotfiles/scalastyle_config_default.xml'

augroup vimrc
  autocmd ColorScheme * highlight ALEError ctermbg=gray ctermfg=black

  autocmd VimEnter *
    \  if
        \ filereadable('./scalastyle_config.xml') &&
        \ input('locally scalastyle_config.xml was found, Do you want to load? (y/n)') == 'y'
      \| let g:ale_scala_scalastyle_config =  $'{execute("pwd")[:-1]}/scalastyle-config.xml'
      \| echomsg $'a scalastyle config loaded: {g:ale_scala_scalastyle_config}'
    \| endif
augroup END

" ==========
" Formatters
" ==========

let g:ale_fix_on_save = v:true

let g:ale_fixers = #{
  \ sh: ['shfmt'],
  \ go: ['gofmt', 'goimports'],
\ }

for s:ts in s:typescript_variants
  let g:ale_fixers[s:ts] = ['prettier', 'eslint']
endfor

" }}}
" elm-vim {{{

let g:elm_browser_command = g:vimrc['open_on_gui']
let g:elm_format_autosave = 1
let g:elm_make_output_file = '/tmp/elm-vim-output.html'
let g:elm_make_show_warnings = 1
let g:elm_setup_keybindings = 0

" }}}
" vim-fakeclip {{{

if g:vimrc.is_wsl
  let g:fakeclip_provide_clipboard_key_mappings = v:true
  let g:fakeclip_force_override_clip_command = 'nkf -s | clip.exe'

  call vimrc#job#start_simply(
    \ ['/bin/which', 'nkf'],
    \ v:null,
    \ { _, __ -> s:Msg.warn('nkf is not found.') }
  \ )
endif

" }}}
" idris-vim {{{

let g:idris_vim_enable_keymappings_by_default = v:false

" }}}
" vim-operator-surround {{{

" Please see ~/.vim/autoload/vimrc/dein/hook_source.vim

" }}}
" vim-highlightedyank {{{

let g:highlightedyank_highlight_duration = 200

" }}}
" vim-fmap {{{

let g:fmap_use_default_keymappings = v:false
let g:fmap_escape_keys = ['', '', '']

augroup vimrc
  autocmd VimEnter * FNoreMap / ・
  autocmd VimEnter * FNoreMap T ・
  autocmd VimEnter * FNoreMap tt …
  autocmd VimEnter * FNoreMap '' 　
  autocmd VimEnter * FNoreMap p （
  autocmd VimEnter * FNoreMap k 「
  autocmd VimEnter * FNoreMap K 〈
  autocmd VimEnter * FNoreMap -k 『
augroup END

" }}}
" vim-indent-guides {{{

let g:indent_guides_enable_on_vim_startup = 1
let g:indent_guides_start_level = 2
let g:indent_guides_default_mapping = 0
let g:indent_guides_guide_size = 1
let g:indent_guides_auto_colors = 0
let g:indent_guides_tab_guides = 0
let g:indent_guides_exclude_filetypes = [
  \ '',
  \ 'adrone_home',
  \ 'aref_web',
  \ 'gitcommit',
  \ 'happy',
  \ 'haskell',
  \ 'help',
  \ 'man',
  \ 'markdown',
  \ 'review',
\ ]

" }}}
" vim-lsp {{{

let g:lsp_async_completion = 1
let g:lsp_diagnostics_enabled = 0
let g:lsp_document_code_action_signs_enabled = 0

" NOTE: To debug
let g:lsp_log_file = expand('~/vim-lsp.log')
let g:lsp_log_verbose = 1

function s:find_root_uri_by_file(file) abort
  return lsp#utils#path_to_uri(
    \ lsp#utils#find_nearest_parent_file_directory(
      \ lsp#utils#get_buffer_path(),
      \ a:file
    \ )
  \ )
endfunction

" }}}
" vim-lsp-settings {{{

" solargraph is temporary disabled because it occurs error at runtime (dein.vim?)
let g:lsp_settings = #{
  \ solargraph: #{ disabled: 1 },
\ }

let g:lsp_settings_filetype_vue = ['typescript-language-server', 'volar-server']
let g:lsp_settings_filetype_typescript = ['typescript-language-server', 'deno']
let g:lsp_settings_filetype_javascript = ['typescript-language-server', 'deno']

" }}}
" vim-ghcid-quickfix {{{

function s:make_ghcid_event_hooks_to_show_started_on_popup(qf_bufnr) abort
  const super = ghcid_quickfix#event_hooks#quickfix_on_error#new(a:qf_bufnr)
  const this = copy(super)

  function! this.on_quickfix_buffer_created() abort dict closure
    call super.on_quickfix_buffer_created()
    call popup_notification('ghcid-quickfix started.', #{
      \ time: 3000,
      \ tab: -1,
      \ border: [],
    \ })
  endfunction

  return this
endfunction

let g:ghcid_quickfix = #{
  \ showing: function('s:make_ghcid_event_hooks_to_show_started_on_popup'),
\ }

" }}}
" translate.vim {{{

let g:translate_source = 'en'
let g:translate_target = 'ja'
let g:translate_winsize = 10

" }}}
" vim-precious {{{

let g:precious_enable_switch_CursorMoved = {
  \ '*': v:false,
\ }

let g:precious_enable_switch_CursorMoved_i = g:precious_enable_switch_CursorMoved
let g:precious_enable_switchers = #{}
let g:textobj_precious_no_default_key_mappings = v:true

augroup vimrc
  autocmd User PreciousFileType IndentGuidesToggle | IndentGuidesToggle
  autocmd WinEnter,BufEnter,TabEnter * PreciousSwitch
augroup END

" }}}
" context_filetype.vim {{{

let g:context_filetype#filetypes = #{
  \ help: [],
  \ vue: [],
  \ html: [],
  \ erb: [],
  \ review: [
    \ #{
      \ start: '//list\[[^]]\+\]\[[^]]\+\]\[\([^]]\+\)\]{',
      \ end: '//}',
      \ filetype: '\1',
    \ },
  \ ],
\ }

" }}}
" sync-term-cwd.vim {{{

let g:synctermcwd_cd_command = 'lcd'

" }}}
" vim-webpage {{{

let g:webpage_source = #{
  \ weblio: 'http://ejje.weblio.jp/content/%s',
  \ stackage: 'https://www.stackage.org/lts-15.4/hoogle?q=%s',
\ }

" }}}
" jumpy.vim {{{

let g:jumpy_map = [')', '(']

" }}}
" vim-quickrepl {{{

let g:quickrepl_config = {
  \ 'vue': ['ts-node'],
  \ 'typescript.tsx': ['ts-node'],
  \ 'go': ['gore'],
  \ 'ps1': ['powrshell', 'powershell.exe'],
\ }

let g:quickrepl_use_default_key_mapping = v:true
let g:quickrepl_enable_debug = v:true

" }}}
" vim-cursorword {{{

augroup vimrc
  autocmd VimEnter,ColorScheme * highlight CursorWord0 ctermbg=LightGray ctermfg=Black
  autocmd VimEnter,ColorScheme * highlight CursorWord1 ctermbg=LightGray ctermfg=Black
augroup END

" }}}
" gist.vim {{{

if g:vimrc.is_wsl
  let g:gist_clip_command = 'clip.exe'
endif

" }}}
" open-browser.vim {{{

if g:vimrc.is_wsl
  " Copied from the help of open-browser.vim
  let g:openbrowser_browser_commands = [
    \ #{
      \ name: 'wslview',
      \ args: ['{browser}', '{uri}'],
    \ },
  \ ]
endif

" }}}
" previm {{{

let g:previm_code_language_show = 1
let g:previm_hard_line_break = 1

if g:vimrc.is_wsl
  let g:previm_wsl_mode = v:true
  let g:previm_open_cmd = 'wslview'
endif

" }}}
" ddu.vim {{{

call ddu#custom#patch_global(#{
  \ ui: 'ff',
  \ kindOptions: #{
    \ file: #{ defaultAction: 'open' },
    \ help: #{ defaultAction: 'open' },
    \ lsp: #{ defaultAction: 'open' },
  \ },
  \ sourceOptions: #{
    \ _: #{
      \ matchers: ['matcher_substring'],
      \ ignoreCase: v:true,
    \ },
  \ },
\ })

" boolean | string
let g:vimrc_ddu_start_with_insert_next = v:false

function s:get_feedkeys_for_ddu_start() abort
  if type(g:vimrc_ddu_start_with_insert_next) ==# type('')
    return 'i' .. g:vimrc_ddu_start_with_insert_next
  endif
  if g:vimrc_ddu_start_with_insert_next
    return 'i'
  endif
  throw 'Nothing feedkeys'
endfunction

" When the next ddu ready, ddu starts from insert
augroup vimrc
  autocmd user Ddu:uiReady
    \ if g:vimrc_ddu_start_with_insert_next !=# v:false
      \| call feedkeys(s:get_feedkeys_for_ddu_start())
      \| let g:vimrc_ddu_start_with_insert_next = v:false
    \| endif
augroup END

" }}}
" ddc.vim {{{

call ddc#custom#patch_global(#{
  \ ui: 'native',
  \ sources: ['vim-lsp', 'around', 'neosnippet', 'file', 'buffer'],
  \ sourceOptions: #{
    \ _: #{
      \ matchers: ['matcher_fuzzy'],
      \ sorters: ['sorter_fuzzy'],
      \ converters: ['converter_fuzzy'],
      \ ignoreCase: v:true,
    \ },
    \ vim-lsp: #{
      \ matchers: ['matcher_head'],
      \ sorters: ['sorter_rank'],
      \ mark: 'lsp',
      \ ignoreCase: v:true,
    \ },
    \ file: #{
      \ matchers: ['matcher_head'],
      \ sorters: ['sorter_rank'],
      \ mark: 'F',
      \ isVolatile: v:true,
      \ forceCompletionPattern: '\S/\S*',
    \ },
    \ buffer: #{ mark: 'B' },
    \ around: #{ mark: 'A' },
    \ neosnippet: #{ mark: 'ns', dup: v:true },
  \ },
  \ sourceParams: #{
    \ around: #{ maxSize: 500 },
    \ buffer: #{
      \ requireSameFiletype: v:false,
      \ limitBytes: 5000000,
      \ fromAltBuf: v:true,
      \ forceCollect: v:true,
    \ },
  \ },
\ })

" }}}
" lexima.vim {{{

call lexima#add_rule(#{char: '<', input_after: '>'})
" call lexima#add_rule(#{char: '<', at: '\%#\>', leave: 1})
" call lexima#add_rule(#{char: '<BS>', at: '\<\%#\>', delete: 1})
call lexima#add_rule(#{char: '「', input_after: '」'})
call lexima#add_rule(#{char: '（', input_after: '）'})
call lexima#add_rule(#{char: '【', input_after: '】'})

" }}}
" vital.vim {{{

" If you want to :Vitalize,
" do `make install-vital-vim` first,
" then add installed vital.vim and plugins onto &rutimepath using `set rtp+=`.

" }}}
" quickpeek.vim {{{

let g:quickpeek_auto = v:true

" }}}
" fern.vim {{{

let g:fern#default_hidden = 1

" }}}
" copilot.vim {{{

let g:copilot_no_tab_map = v:true

" }}}
" ddu-source-lsp {{{

let g:ddu_source_lsp_clientName = 'vim-lsp'

" }}}
" rainbow {{{

let g:rainbow_active = 1

" }}}
" gin.vim {{{

let g:gin_proxy_editor_opener = 'vsplit'

" }}}
" deepl.vim {{{

let g:deepl#endpoint = 'https://api-free.deepl.com/v2/translate'

" }}}

call dein#end()


"---------"
" Options "
"---------"
" {{{

set
  \ autoindent
  \ backspace=indent,eol,start
  \ breakindent
  \ browsedir=buffer
  \ cindent
  \ cmdheight=2
  \ completeopt-=preview
  \ fileencodings=ucs-bom,utf-8,sjis,euc-jp,cp932,iso-2022-jp-3,iso-2022-jp,eucjp-ms,euc-jisx0213,ucs-bom,latin1,default
  \ hidden
  \ history=1000
  \ hlsearch
  \ incsearch
  \ laststatus=2
  \ linebreak
  \ list
  \ listchars=tab:»_,trail:_,extends:»,precedes:«,nbsp:%,eol:↲
  \ matchpairs+=<:>,（:）,｛:｝,「:」,＜:＞,『:』
  \ nojoinspaces
  \ noruler
  \ notimeout
  \ nowrap
  \ nowrapscan
  \ number
  \ omnifunc=lsp#complete
  \ path=.,,./*
  \ previewheight=40
  \ relativenumber
  \ scrolloff=16
  \ sessionoptions=buffers,tabpages,localoptions,winsize,winpos
  \ shellslash
  \ suffixes=
  \ tabline=%!vimrc#tabline#make()
  \ expandtab
  \ shiftwidth=2
  \ tabstop=2
  \ visualbell
  \ wildignorecase
  \ wildmenu
  \ helplang=ja,en
  \ cursorline
  \ viminfo='400,<50,s10,h

if !has('nvim')
  set termwinkey=<C-q>
endif

" See ':h hl-User1..9' for what is the pattern of '%n*string%*' (n is a natural number)
let &statusline = join([
  \ '%1*[%F(%n)]%*',
  \ '%2*[FT=%y]%*',
  \ '[%l,%v]',
  \ '[Fenc=%{&fileencoding}]',
  \ '[Enc=%{&encoding}]',
  \ '%3*%{vimrc#statusline#is_yankround_active() ? "[yankround]" : ""}%*',
  \ '%4*%m%*',
  \ '%5*%r%*',
\ ], '')

" ☆
set ambiwidth=double

set background=dark
colorscheme lucariox

" The tabline is always shown
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

" Always I use the ignorecase
set ignorecase noinfercase

" I control the IME state by myself
set iminsert=0

" Open .tex as LaTex
let g:tex_flavor = 'latex'

" Reference tags of ctags
let &tags = &tags .. ',' .. join([
  \ 'tags',
  \ '.git/tags',
  \ $'{g:vimrc.path_at_started}/tags',
  \ $'{g:vimrc.path_at_started}/.git/tags',
\ ], ',')

let mapleader = get(g:, 'mapleader', '[')
let maplocalleader = get(g:, 'maplocalleader', '[')

" }}}


"---------"
" Keymaps "
"---------"
" variables {{{

nnoremap <expr> <Plug>(vimrc-clear) <SID>clear()
nnoremap <expr> <Plug>(vimrc-clear-deep) <SID>clear_deep()

" }}}
" normal mode {{{

" Allow keymaps like <C-c>{foo}, and {bar}<C-c>
nnoremap <C-c> <NOP>
nnoremap <C-c><C-c> <C-c>

nmap <C-[> <Plug>(vimrc-clear)
nmap <Esc> <Plug>(vimrc-clear)
nmap <C-l> <Plug>(vimrc-clear)
nmap <C-k><C-l> <Plug>(vimrc-clear-deep)

function s:clear() abort
  call yankround#inactivate()
  PreciousSwitch
  call popup_clear()
  return ':nohlsearch' .. "\<CR>"
endfunction

function s:clear_deep() abort
  echo 'clearing...'
  PreciousReset  " heavy

  const result = s:clear()
  echo 'cleared!'
  return result
endfunction

" listup
nmap <silent> g: :<C-u>call vimrc#open_buffer_to_execute('buffers')<CR>gh_
nmap <silent> g> :<C-u>call vimrc#open_buffer_to_execute('messages')<CR>gh_
nmap <silent> m: :<C-u>call vimrc#open_buffer_to_execute('marks')<CR>gh_
nmap <silent> q> :<C-u>call vimrc#open_buffer_to_execute('register')<CR>gh_
" TODO: Alternative.
" nmap <silent> y: :<C-u>Denite unite:yankround<CR>
nnoremap <silent> z: :<C-u>tabs<CR>
nnoremap q: gQ
nnoremap <silent> # "zyiw?\m\C\<<C-r>z\><CR>
nnoremap <silent> * "zyiw/\m\C\<<C-r>z\><CR>
nnoremap <silent> <C-k>o :<C-u>e! %<CR>
nnoremap <silent> <leader># #
nnoremap <silent> <leader>* *
" NOTE: ? <Cmd> instead of :<C-u> is doesn't work
nnoremap <silent> <leader>B :<C-u>sp <C-r>=g:vimrc.memo_path<CR><CR>
nnoremap <silent> <leader>b <Cmd>ScratchBufferOpenFile md sp<CR>
nnoremap <silent> g* :<C-u>execute 'silent! normal! *<C-o>'<CR>
nnoremap <silent> Q :<C-u>call vimrc#bufclose_filetype(g:vimrc.temporary_buftypes)<CR>

" folds
nnoremap <expr> h foldclosed('.') > -1 ? 'zo' : 'h'
nnoremap <expr> l foldclosed('.') > -1 ? 'zo' : 'l'
nnoremap zj zjzo
nnoremap zk zkzo[zzt

" windows
" <Space> prefix
nnoremap <Space>h <C-w>h
nnoremap <Space>j <C-w>j
nnoremap <Space>k <C-w>k
nnoremap <Space>l <C-w>l
" gh prefix
nnoremap ghR <C-w>r
nnoremap <silent> ghq :<C-u>call vimrc#hide_or_quit()<CR>
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
nnoremap <silent> ghs :<C-u>split<CR>
nnoremap <silent> ghv :<C-u>vsplit<CR>
nnoremap <silent><expr> gH  ('mZ:tabnew<CR>`Zzz'          .. (foldlevel('.') > 0 ? 'zo' : ''))
nnoremap <silent><expr> ghh ('mZ:hide<CR>:tabnew<CR>`Zzz' .. (foldlevel('.') > 0 ? 'zo' : ''))
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
let g:vimrc.default_term_options = #{
  \ term_finish: 'close',
  \ vertical: v:true,
\ }
nnoremap <silent> <leader>v <Cmd>call term_start(&shell, g:vimrc.default_term_options->extendnew(#{
\ cwd: vimrc#get_current_buffer_dir(#{ alt_dir: g:vimrc.git_root }),
\ }))<CR>
nnoremap <silent> <leader><leader>v <Cmd>call term_start(&shell, g:vimrc.default_term_options->extendnew(#{
  \ cwd: vimrc#get_current_buffer_dir(#{ alt_dir: g:vimrc.git_root }),
  \ vertical: v:false,
\ }))<CR>
nnoremap <silent> <leader>V <Cmd>call term_start(&shell, g:vimrc.default_term_options->extendnew(#{
  \ cwd: vimrc#get_current_buffer_dir(#{ alt_dir: g:vimrc.git_root }),
  \ curwin: v:true,
\ }))<CR>
nnoremap <silent> <leader><leader>V <Cmd>tabnew \| call term_start(&shell, g:vimrc.default_term_options->extendnew(#{
  \ cwd: vimrc#get_current_buffer_dir(#{ alt_dir: g:vimrc.git_root }),
  \ curwin: v:true,
\ }))<CR>

" set
nnoremap <silent> <C-h><C-d> :<C-u>call vimrc#toggle_diff()<CR>
nnoremap <silent><expr> <C-h><C-v> ':setl virtualedit=' .. (&virtualedit ==# '' ? 'all' : '') .. ' virtualedit?<CR>'
nnoremap <silent><expr> zm ':setl foldmethod=' .. (&foldmethod ==# 'marker' ? 'syntax' : 'marker') .. ' foldmethod?<CR>'
nnoremap <silent> <C-h><C-w> :<C-u>setl wrap! wrap?<CR>
nnoremap <silent> <C-h><C-c> :<C-u>setl cursorline! cursorline?<CR>
nnoremap <silent> <C-h><C-r> :<C-u>setl relativenumber! relativenumber?<CR>
nnoremap <silent> <C-h><C-l> :<C-u>setl list! list?<CR>
nnoremap <silent> <C-h><C-n> :<C-u>setl number! number?<CR>

" Visualize a last pasted range
nnoremap <expr> gp '`[' .. strpart(getregtype(), 0, 1) .. '`]'

" copy & paste
"" clipboard
"" NOTE: Don't use noremap to allow remap with fakeclip
nmap <leader>p "+p
nmap <leader>P "+P
nmap <leader>y "+y
nmap <leader>Y "+yg_
nmap <leader>dd "+dd
nmap <leader>D "+D
nmap <leader>d "+d
nmap <leader>x "+x
" nnoremap "%p <Cmd>put=expand('%:p')<CR>
" nnoremap "%P <Cmd>put!=expand('%:p')<CR>
"" Put the relative path of a current file
nnoremap "gp <Cmd>put=system($'git ls-files --full-name {expand('%')}')<CR>
nnoremap "gP <Cmd>put!=system($'git ls-files --full-name {expand('%')}')<CR>

" cr
nmap <C-j> <CR>
nmap <C-m> <CR>
nnoremap <CR> o<Esc>

" lsp
nnoremap <silent> <C-g><C-o> :<C-u>LspHover<CR>
nnoremap <silent> <C-g><C-a> :<C-u>LspCodeAction<CR>
nnoremap <silent> <C-g><C-d> :<C-u>LspDefinition<CR>
nnoremap <silent> <C-g><C-i> :<C-u>LspPeekImplementation<CR>
nnoremap <silent> <C-g><C-t> :<C-u>LspPeekTypeDefinition<CR>
nnoremap <C-g><C-g> <C-g>

" others
nnoremap gG ggVG
nnoremap ( (zv
nnoremap ) )zv
nnoremap :: :%s/
nnoremap :ev :<C-u>e<Space><C-r>=g:vimrc.path_at_started<CR>/
nnoremap :eg :<C-u>e<Space><C-r>=g:vimrc.git_root<CR>/
nnoremap :eb :<C-u>e<Space><C-r>=expand('%:p:h')<CR>/
nnoremap <C-]> g<C-]>
nnoremap <expr> <C-k><C-s> printf(":%%s/\\m\\C\\<%s\\>//g\<Left>\<Left>", expand('<cword>'))
nnoremap <expr> <C-k>s printf(":%%s/\\m\\C\\<%s\\>/%s/g\<Left>\<Left>", expand('<cword>'), expand('<cword>'))
nnoremap <silent> <C-k><C-j> :<C-u>call <SID>save_clear()<CR>
nnoremap <silent> <C-k><Space> :<C-u>call vimrc#remove_trailing_spaces()<CR>
nnoremap <silent> <C-k>J :<C-u>wall \| echo 'written all !'<CR>
nnoremap <silent> <Space><Space> :<C-u>call vimrc#compress_spaces()<CR>
nnoremap <silent> <leader>o :<C-u>copen<CR>
nnoremap Y yg_
nnoremap g<C-]> <C-]>
nnoremap g_ $
nnoremap zs zszh
nnoremap { {zv
nnoremap } }zv
nnoremap <C-x><C-n> <C-n>
nnoremap <C-x><C-p> <C-p>

function s:save_clear() abort
  write
  call yankround#inactivate()
endfunction

" }}}
" insert mode {{{

" fake digraphs
inoremap <C-k>\+ ＋
inoremap <C-k>\- −
inoremap <C-k>\= ＝
inoremap <C-k>?= ≒
inoremap <C-k>=~ ≅
inoremap <C-k>\N ℕ
inoremap <C-k>\Z ℤ
inoremap <C-k>\R ℝ
inoremap <C-k>\Q ℚ
inoremap <C-k>\C ℂ
inoremap <C-k>.. ◉
inoremap <C-k>\|> ↦

" others
imap <C-j> <CR>
inoremap <C-a> <Right>
inoremap <C-k><C-k> <C-o>"_d$
inoremap <silent> <C-k><C-j> <Esc>:write<CR>
inoremap <silent> <C-k>J <Esc>:wall \| echo 'written all!'<CR>
inoremap <silent><expr> <C-b> vimrc#get_webpage_title(@+)
" Don't noremap for fake-clip
imap <C-r>' <C-r>+
" Execute iabbr and Escape
inoremap <C-l> <Space><Backspace><Esc>

" Meaning "n"ame
"" TODO: For some reason I have to hit `<C-r>=expand('%:t')<CR>` once manually to get out with `<C-r>n`
inoremap <C-r>n <C-r>=expand('%:t')<CR>

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
cnoremap <C-o> <Up>
cnoremap <C-y> <Down>
cnoremap <C-r>' <C-r>+
" Meaning "n"ame
cnoremap <C-r>n <C-r>=expand('%:t')<CR>

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
" Don't noremap for fake-clip
vmap 'p "+p
vmap 'P "+P
vmap 'y "+y
vmap 'd "+d
vmap 'x "+x

" }}}
" select mode {{{

snoremap <C-l> <Esc>

" }}}
" terminal mode {{{

tnoremap <C-l> <C-\><C-n>
tnoremap <C-\><C-n> <Esc>
tnoremap <C-[> <Esc>
tnoremap <C-]> <C-l>
tnoremap <C-q><C-v> <C-q><C-n><C-w>H
" tnoremap <C-q><C-s> <C-q><C-n>
tnoremap <C-q><C-s> <C-q><C-n><Cmd>GLogOneline --pretty='%h %ad %s' --date='format:%Y-%m-%d %H:%M'<CR><Cmd>resize 5<CR>

" }}}
" abbr {{{

" typo {{{

inoreabbr reuslt result
inoreabbr unkonwn unknown
inoreabbr uknown unknown
inoreabbr Parnes Parens
inoreabbr parnes parens
inoreabbr reuslt result
inoreabbr Encrpyt Encrypt
inoreabbr encrpyt encrypt

" }}}

" }}}
" digraph {{{

digraph (( 8834   " ⊂ right includes left
digraph )) 8835   " ⊃ left includes right
digraph /= 8800   " ≠ not equal
digraph \* 215    " × cartesian product
digraph xx 215    " × cartesian product
digraph \. 9675   " ○ composite
digraph \/ 247    " ÷ division
digraph \< 8804   " ≤ right more than left or equals
digraph \= 8803   " ＝ equivalence relation
digraph \> 8805   " ≥ left mode than right or equals
digraph \A 8704   " ∀ forall
digraph \E 8707   " ∃ exists
digraph \U 8745   " ∩ intersect
digraph \u 8746   " ∪ union
digraph \a 8743   " ∧ and
digraph \o 8744   " ∨ or
digraph \|^ 8593  " ↑ arrow up
digraph \|v 8595  " ↓ arrow down
digraph ph 934    " Φ phi
digraph pi 960    " π pi

" }}}
" plugins {{{

" Some explorer
nnoremap <silent> <leader>e         :<C-u>call vimrc#toggle_explorer()<CR>
nnoremap <silent> <leader><leader>e :<C-u>call vimrc#open_explorer('split')<CR>
nnoremap <silent> <leader>E         :<C-u>call vimrc#open_explorer('stay')<CR>
nnoremap <silent> <leader><leader>E :<C-u>call vimrc#open_explorer('tabnew')<CR>
nnoremap <silent> \e  :<C-u>call vimrc#toggle_explorer(g:vimrc.path_at_started)<CR>
nnoremap <silent> \\e :<C-u>call vimrc#open_explorer('split', g:vimrc.path_at_started)<CR>
nnoremap <silent> \E  :<C-u>call vimrc#open_explorer('stay', g:vimrc.path_at_started)<CR>
nnoremap <silent> \\E :<C-u>call vimrc#open_explorer('tabnew', g:vimrc.path_at_started)<CR>

" open-browser.vim
nmap <leader>w <Plug>(openbrowser-open)
vmap <leader>w <Plug>(openbrowser-open)

" vim-quickrun
nmap <leader>r <Plug>(quickrun)
vmap <leader>r <Plug>(quickrun)

" TODO: Alternative.
" denite.nvim
"" map i to do open_filter_buffer
" nmap <C-k><C-f> :<C-u>Denite outline<CR>i
" nmap <C-k><C-t> :<C-u>Denite tag<CR>i

" ddu.vim
function s:ddu_start_file_rec(in_current_directory) abort
  call ddu#custom#patch_global('sourceOptions', #{
    \ file_rec: #{
      \ path:
        \ a:in_current_directory ? expand('%:p:h') :
        \ g:vimrc.git_root !=# v:null ? g:vimrc.git_root :
        \ g:vimrc.path_at_started
    \ },
  \ })

  call vimrc#ddu_start_from_insert(#{
    \ sources: [
      \ #{
        \ name: 'file_rec',
        \ params: #{ ignoredDirectories: ['.git', 'node_modules'] },
      \ },
    \ ],
  \ })
endfunction

nnoremap <C-k><C-e> <Cmd>call <SID>ddu_start_file_rec(v:true)<CR>
nnoremap <C-k>e <Cmd>call <SID>ddu_start_file_rec(v:false)<CR>

nnoremap H :<C-u>call ddu#start(#{
  \ sources: [#{ name: 'line' }],
  \ sourceOptions: #{
    \ _: #{
      \ matchers: ['matcher_regex'],
    \ },
  \ },
\ })<CR>

nnoremap L :<C-u>call vimrc#ddu_start_from_insert(#{
  \ sources: [#{ name: 'buffer' }],
\ })<CR>

nnoremap M :<C-u>call ddu#start(#{
  \ sources: [#{ name: 'file_old' }],
\ })<CR>

nnoremap :h :<C-u>call vimrc#ddu_start_from_insert(#{
  \ sources: [#{ name: 'help' }],
\ })<CR>

nnoremap <C-k><C-f> :<C-u>call ddu#start(#{
  \ sources: [#{ name: 'lsp_documentSymbol' }],
  \ sourceOptions: #{
    \ lsp: #{ volatile: v:true },
  \ },
  \ uiParams: #{
    \ ff: #{
      \ displayTree: v:true,
      \ immediateAction: 'open',
      \ ignoreEmpty: v:false,
    \ },
  \ },
\ })<CR>

" vim-webpage
nnoremap <leader>K :<C-u>Weblio <C-r>=expand('<cword>')<CR><CR>
vnoremap <leader>K "zy:<C-u>Weblio <C-r>z<CR>

" vim-anzu
"" always n moves to forward / N moves to backward
nmap <expr> n (v:searchforward ? '<Plug>(anzu-n-with-echo)' : '<Plug>(anzu-N-with-echo)') .. 'zv'
nmap <expr> N (v:searchforward ? '<Plug>(anzu-N-with-echo)' : '<Plug>(anzu-n-with-echo)') .. 'zv'
nmap * <Plug>(anzu-star-with-echo)zv
nmap # <Plug>(anzu-sharp-with-echo)zv

" undotree
nnoremap <silent> <leader>U :<C-u>UndotreeToggle<CR>

" vim-indent-guides
nnoremap <silent> <C-h><C-i> :<C-u>call vimrc#toggle_indent_guides()<CR>

" deoplete.nvim
" inoremap <CR> <CR>
" inoremap <Tab> <Tab>
" imap <expr> <C-k><C-i> deoplete#toggle() ? '' : ''

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

" translate.vim
vnoremap <silent> <leader>k "zy:Translate<Space><C-r>z<CR>

" ale
nnoremap <silent> <C-k><C-a> :<C-u>ALEToggle<CR>
nnoremap <silent> <C-k>a :<C-u>call vimrc#toggle_ale_at_buffer()<CR>
nmap [] :<C-u>ALEDetail<CR>
" but this doesn't overwrite diff keymaps, please see <C-h><C-d> and vimrc#toggle_diff()
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

" git (also see .vim/plugin/vimrc.vim)
nnoremap <silent> <leader>gs <Cmd>GStatus<CR>
nnoremap <silent> <leader>gS <Cmd>GitShowViewer<CR>
nnoremap <silent> <leader>gc <Cmd>GCommit<CR>
nnoremap <silent> <leader>gC <Cmd>GCommitAmmend<CR>
nnoremap <silent> <leader>ga <Cmd>GAddPatch<CR>
nnoremap <silent> <leader>gl <Cmd>GLog<CR>
nnoremap <silent> <leader>gL <Cmd>GLogPatch<CR>
nnoremap <silent> <leader>go <Cmd>GLogOneline --pretty='%h %ad %s' --date='format:%Y-%m-%d %H:%M'<CR>
nnoremap <silent> <leader>gd <Cmd>GDiff<CR>
nnoremap <silent> <leader>gb <Cmd>GBrahcnAll<CR>
nnoremap <silent> <leader>gt <Cmd>GLogTree<CR>
nnoremap <silent> <leader>gT <Cmd>GLogTreeAll<CR>
nnoremap <silent> \gs <Cmd>tabnew \| GStatus<CR>
nnoremap <silent> \gl <Cmd>tabnew \| GLog<CR>
nnoremap <silent> \gL <Cmd>tabnew \| GLogPatch<CR>
nnoremap <silent> \go <Cmd>tabnew \| GLogOneline --pretty='%h %ad %s' --date='format:%Y-%m-%d %H:%M'<CR>

" vim-textobj-clang
" You are not i
vmap a;m i;m
omap a;m i;m
vmap a;f i;f
omap a;f i;f

" vim-easy-align
xmap i: :<C-u>Align<Space>

" vim-operator-surround
vmap ga <Plug>(operator-surround-append)
omap ga <Plug>(operator-surround-append)
nmap ga <Plug>(vimrc-surround-append-choice)
nmap gs <Plug>(vimrc-surround-append-choice-wide)
nmap ds <Plug>(vimrc-surround-delete-mostly-inner)
nmap cs <Plug>(vimrc-surround-replace-mostly-inner)

" vim-textobj-jabraces
vmap ijp <Plug>(textobj-jabraces-parens-i)
vmap ajp <Plug>(textobj-jabraces-parens-a)
omap ijp <Plug>(textobj-jabraces-parens-i)
omap ajp <Plug>(textobj-jabraces-parens-a)

vmap ijK <Plug>(textobj-jabraces-yama-kakko-i)
vmap ajK <Plug>(textobj-jabraces-yama-kakko-a)
omap ijK <Plug>(textobj-jabraces-yama-kakko-i)
omap ajK <Plug>(textobj-jabraces-yama-kakko-a)

vmap ij-k <Plug>(textobj-jabraces-double-kakko-i)
vmap ij-k <Plug>(textobj-jabraces-double-kakko-a)
omap ij-k <Plug>(textobj-jabraces-double-kakko-i)
omap ij-k <Plug>(textobj-jabraces-double-kakko-a)

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

" operator-camelize.vim
nmap <silent> <leader><leader>c :<C-u>call vimrc#operator_camelize_toggle_current_word_with_setting_repeatable()<CR>
vmap <leader><leader>c <Plug>(operator-camelize-toggle)

" vim-repeat
nmap . <Plug>(repeat-.)

for x in s:List.char_range('a', 'z')
  execute 'nnoremap' '<silent>' $'@{x}' (":\<C-u>" .. $'call vimrc#execute_repeatable_macro("{x}")\<CR>')
endfor

" vim-yankround
nmap <silent><expr> <C-n> (yankround#is_active() ? "\<Plug>(yankround-next)" : 'gt')
nmap <silent><expr> <C-p> (yankround#is_active() ? "\<Plug>(yankround-prev)" : 'gT')
nmap P <Plug>(yankround-P)
nmap p <Plug>(yankround-p)
xmap p <Plug>(yankround-p)

" vim-dirvish
"" Don't waste my global - key
nmap <Plug>(nomap-dirvish_up) <Plug>(dirvish_up)

" vim-quickrepl
nmap <leader>R <Plug>(quickrepl-open)

" kensaku-search.vim
cmap <C-j> <CR>
cnoremap <CR> <Plug>(kensaku-search-replace)<CR>

" copilot.vim
imap <silent><script><expr> <C-g><Tab> copilot#Accept("\<CR>")
imap <C-]> <Plug>(copilot-next)

" deepl.vim
vnoremap <silent> <leader><leader>k :DeeplTranslateToJaOpenBuffer<CR>

" }}}

if filereadable($'{$HOME}/.vimrc_env_post')
  source ~/.vimrc_env_post
endif

execute 'helptags' $'{g:vimrc.vim_home}/doc'
filetype plugin indent on
syntax enable
let g:vimrc.loaded = v:true
