scriptencoding utf-8
scriptversion 3

let s:List = vital#vimrc#import('Data.List')

nnoremap <silent> <Plug>(vimrc-surround-append-choice) :<C-u>call vimrc#append_choose_surround()<CR>
nnoremap <silent> <Plug>(vimrc-surround-append-choice-wide) :<C-u>call vimrc#append_choose_surround_wide()<CR>
nnoremap <silent> <Plug>(vimrc-surround-delete-mostly-inner) :<C-u>call vimrc#delete_mostly_inner_surround()<CR>
nnoremap <silent> <Plug>(vimrc-surround-replace-mostly-inner) :<C-u>call vimrc#replace_mostly_inner_surround()<CR>

" Vim common
"" Scripts
command! -bar -nargs=? -complete=filetype FtpluginEditAfter
    \ execute ':edit' printf('%s/after/ftplugin/%s.vim', g:vimrc['vim_home'], (empty(<q-args>) ? &filetype : <q-args>))

command! -bar -nargs=? -complete=filetype FtDictionaryEdit
    \ execute ':edit' printf('%s/dict/filetype/%s.dict', g:vimrc['vim_home'], (empty(<q-args>) ? &filetype : <q-args>))

command! -bar -nargs=? -complete=filetype SyntaxEdit
    \ execute ':edit' printf('%s/syntax/%s.vim', g:vimrc['vim_home'], (empty(<q-args>) ? &filetype : <q-args>))

command! -bar -nargs=? -complete=filetype IndentEdit
    \ execute ':edit' printf('%s/indent/%s.vim', g:vimrc['vim_home'], (empty(<q-args>) ? &filetype : <q-args>))

command! -bar -nargs=? -complete=filetype FtDetectEdit
    \ execute ':edit' printf('%s/ftdetect/%s.vim', g:vimrc['vim_home'], (empty(<q-args>) ? &filetype : <q-args>))

command! -bar -nargs=? -complete=filetype PluginEdit
    \ execute ':edit' printf('%s/plugin/%s.vim', g:vimrc['vim_home'], (empty(<q-args>) ? &filetype : <q-args>))

command! -bar -nargs=? -complete=filetype AutoloadEdit
    \ execute ':edit' printf('%s/autoload/%s.vim', g:vimrc['vim_home'], (empty(<q-args>) ? &filetype : <q-args>))
"" Others
command! -bar GUI call vimrc#open_this_file_in_gui()
command! -bar ReverseLines !tac

"" Clear quickfix
command! -bar CClear call setqflist([])

"" Rename a file of the current buffer
command! -bar -nargs=1 -complete=file Rename call vimrc#rename_to(<q-args>)

"" Pull and Insert <title>\(.*\)</title>
command! -bar -nargs=1 InsertWebPageTitle execute 'normal! i' . vimrc#pull_webpage_title(<q-args>)

"" Save session and specify session name automatically
command! -bar SessionSaveInGitBranch call vimrc#git_branch_session_save()

"" Haskell
command! -bar HaskDogs call vimrc#execute_haskdogs_async()
command! -bar EtaDogs call vimrc#execute_haskdogs_in_eta_async()

"" Kotlin
command! -bar KtlintAutoFix call system('ktlint --format ' . fnameescape(expand('%'))) | edit %
command! -bar -nargs=* QuickfixRunGradle call vimrc#run_gradle_quickfix(<q-args>)

"" Scala
command! -bar -nargs=* QuickfixRunSbtCompileWatch call vimrc#run_scala_compile_watch_quickfix(<q-args>)
command! -bar QuickfixStopSbtCompileWatch call vimrc#stop_scala_compile_watch_quickfix()

"" TypeScript
command! -bar -nargs=* QuickfixRunYarn call vimrc#run_yarn_quickfix(<q-args>)

"" Make
command! -bar -nargs=* QuickfixRunMake call vimrc#run_make_quickfix(<q-args>)

function! s:terminal_at_started(filetype, command, ...) abort
  let options = get(a:, 1, {})
  return vimrc#open_terminal_as(
    \ a:filetype,
    \ 'stay',
    \ a:command,
    \ extend(options, {'path': g:vimrc.path_at_started})
    \ )
endfunction

" Git commands
command! -bar -nargs=* GStatus GinStatus <args>
command! -bar -nargs=* GLog GitLogViewer -100 --name-only <args>
command! -bar -nargs=* GLogPatch GitLogViewer --patch -100 <args>
command! -bar -nargs=* GLogOneline GitLogViewer --oneline <args>
command! -bar -nargs=* GDiff GitDiffViewer <args>
command! -bar -nargs=* GDS GitDiffViewer --staged <args>
command! -bar -nargs=* GDH GitDiffViewer HEAD~ <args>
command! -bar -nargs=* GCommit Gin commit --verbose <args>
command! -bar -nargs=* GCommitAmmend Gin commit --verbose --amend <args>
command! -bar -nargs=1 GCommitFixup echomsg system('git commit --fixup ' .. <q-args>)
command! -bar -nargs=* GAddPatch terminal git add -p <args>
command! -bar -nargs=* GTree GinLog --graph --decorate --oneline <args>
command! -bar -nargs=* GTreeAll GinLog --graph --decorate --oneline --all <args>
command! -bar -nargs=* GBrahcnAll GinBranch --all <args>
command! -bar -nargs=* GBlame Gin blame <args>

" Twitter
command! -bar TweetVRChat call vimrc#tweet(g:vimrc.twitter.vrchat)
command! -bar TweetNico call vimrc#tweet(g:vimrc.twitter.nico)
command! -bar TweetPrivate call vimrc#tweet(g:vimrc.twitter.private)
command! -bar TweetPublic call vimrc#tweet(g:vimrc.twitter.public)
command! -bar TwitterVRChat call vimrc#twitter(g:vimrc.twitter.vrchat)
command! -bar TwitterVRChatTab tabnew | TwitterVRChat
command! -bar TwitterNico call vimrc#twitter(g:vimrc.twitter.nico)
command! -bar TwitterNicoTab tabnew | TwitterNico
command! -bar TwitterPrivate call vimrc#twitter(g:vimrc.twitter.private)
command! -bar TwitterPrivateTab tabnew | TwitterPrivate
command! -bar TwitterPublic call vimrc#twitter(g:vimrc.twitter.public)
command! -bar TwitterPublicTab tabnew | TwitterPublic

" vim-webpage
command! -bar -nargs=+ Weblio WebpageShow weblio <args>
command! -nargs=+ Stackage WebpageShow stackage <args>

"" Others
command! -bar CdBufDir execute ':cd' fnameescape(expand('%:p:h'))
command! -bar CdStarted execute ':cd' g:vimrc.path_at_started
command! -bar CdGitRoot call vimrc#cd_git_root(':cd')
command! -bar LcdBufDir execute ':lcd' fnameescape(expand('%:p:h'))
command! -bar LcdStarted execute ':lcd' g:vimrc.path_at_started
command! -bar LcdGitRoot call vimrc#cd_git_root(':lcd')
command! -bar ScdBufDir let g:vimrc.path_at_started = expand('%:p:h')
command! -bar ScdCurrentDir let g:vimrc.path_at_started = getcwd()
command! -bar ScdGitRoot let g:vimrc.path_at_started = g:vimrc.git_root
command! -bar GitReadRoot call vimrc#read_to_set_git_root()
command! -bar ReadGitRoot call vimrc#read_to_set_git_root()
"""
command! -bar -nargs=? Grep call vimrc#ddu_start_from_input(#{
  \ sources: [#{
    \ name: 'rg',
    \ options: #{
      \ matchers: [],
      \ volatile: v:true,
    \ },
  \ }],
  \ uiParams: #{
    \ ff: #{
      \ startFilter: v:true,
      \ ignoreEmpty: v:false,
      \ autoResize: v:false,
    \ },
  \ },
\ }, <f-args>)
command! -bar -nargs=+ SetTabTitle let t:vimrc_tabtitle = <q-args>
command! -bar UnsetTabTitle unlet t:vimrc_tabtitle

" deepl.vim
command! -bar -range=% DeeplTranslateToEn call vimrc#deepl_translate(<line1>, <line2>, 'EN', 'JA', 'yank')
command! -bar -range=% DeeplTranslateToJa call vimrc#deepl_translate(<line1>, <line2>, 'JA', 'EN', 'yank')
command! -bar -range=% DeeplTranslateToEnEcho call vimrc#deepl_translate(<line1>, <line2>, 'EN', 'JA', 'echo')
command! -bar -range=% DeeplTranslateToJaEcho call vimrc#deepl_translate(<line1>, <line2>, 'JA', 'EN', 'echo')

"
" Tapis
"

function Tapi_Tabnew(_, args) abort
  const files = a:args[1:]
  const paths = s:List.map(files, { file -> fnameescape(file) })

  for path in paths
    execute 'tabnew' path
  endfor
endfunction

function Tapi_Verticalnew(_, args) abort
  const files = a:args[1:]
  const paths = s:List.map(files, { file -> fnameescape(file) })

  for path in paths
    execute 'vertical' 'new' path
  endfor
endfunction
