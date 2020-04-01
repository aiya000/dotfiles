scriptencoding utf-8
scriptversion 3

nnoremap <silent> <Plug>(vimrc-surround-append-choice) :<C-u>call vimrc#append_choose_surround()<CR>
nnoremap <silent> <Plug>(vimrc-surround-append-choice-wide) :<C-u>call vimrc#append_choose_surround_wide()<CR>
nnoremap <silent> <Plug>(vimrc-surround-delete-mostly-inner) :<C-u>call vimrc#delete_mostly_inner_surround()<CR>
nnoremap <silent> <Plug>(vimrc-surround-replace-mostly-inner) :<C-u>call vimrc#replace_mostly_inner_surround()<CR>

" Vim common
"" buffer open commands with filetype 'none'
command! -bar -bang NewOverridden new<bang> | if empty(&ft) | setf none | endif
command! -bar -bang -complete=file -nargs=? EditOverridden e<bang> <args> | if empty(&ft) | setf none | endif
command! -bar -bang -complete=file -nargs=? VnewOverridden vnew<bang> <args> | if empty(&ft) | setf none | endif
command! -bar -bang -complete=file -nargs=? EnewOverridden enew<bang> <args> | if empty(&ft) | setf none | endif
command! -bar -bang -complete=file -nargs=? TabnewOverridden tabnew<bang> <args> | if empty(&ft) | setf none | endif
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
command! -bar -nargs=* GStatus Gina status -s <args>
command! -bar -nargs=* GLog GitLogViewer -100 --name-only <args>
command! -bar -nargs=* GLogPatch GitLogViewer --patch -100 <args>
command! -bar -nargs=* GLogOneline GitLogViewer --oneline <args>
command! -bar -nargs=* GDiff GitDiffViewer <args>
command! -bar -nargs=* GDS GitDiffViewer --staged <args>
command! -bar -nargs=* GDH GitDiffViewer HEAD~ <args>
command! -bar -nargs=* GCommit Gina commit --verbose <args>
command! -bar -nargs=* GCommitAmmend Gina commit --verbose --amend <args>
command! -bar -nargs=1 GCommitFixup echomsg system('git commit --fixup ' . <q-args>)
command! -bar -nargs=* GAddPatch terminal git add -p <args>
command! -bar -nargs=* GTree Gina log --graph --decorate --oneline <args>
command! -bar -nargs=* GTreeAll Gina log --graph --decorate --oneline --all <args>
command! -bar -nargs=* GBrahcnAll Gina branch --all <args>
command! -bar -nargs=* GBlame Gina blame <args>
command! -bar -nargs=* Gist Gista post --stay <args>

" Twitter
command! -bar TweetNico call vimrc#tweet(g:vimrc.twitter.nico)
command! -bar TweetPrivate call vimrc#tweet(g:vimrc.twitter.private)
command! -bar TweetPublic call vimrc#tweet(g:vimrc.twitter.public)
command! -bar TwitterNico call vimrc#twitter(g:vimrc.twitter.nico)
command! -bar TwitterNicoTab tabnew | TwitterNico
command! -bar TwitterPrivate call vimrc#twitter(g:vimrc.twitter.private)
command! -bar TwitterPrivateTab tabnew | TwitterPrivate
command! -bar TwitterPublic call vimrc#twitter(g:vimrc.twitter.public)
command! -bar TwitterPublicTab tabnew | TwitterPublic

" vim-webpage
command! -bar -nargs=+ Weblio WebpageShow weblio <args>
command! -nargs=+ Stackage WebpageShow stackage <args>

" vim-dirvish
command! -nargs=? -complete=dir Explore Dirvish <args>
command! -nargs=? -complete=dir Sexplore split | silent Dirvish <args>
command! -nargs=? -complete=dir Vexplore vsplit | silent Dirvish <args>
command! -nargs=? -complete=dir Texplore tabnew | silent Dirvish <args>

"" Others
command! -bar -nargs=* Vim call vimrc#open_terminal_as('term-vim', 'stay', 'vim ' . <q-args>)
command! -bar CdBufDir cd %:p:h
command! -bar CdStarted execute ':cd' g:vimrc.path_at_started
command! -bar CdGitRoot execute ':cd' system('git rev-parse --show-toplevel')
command! -bar LcdBufDir lcd %:p:h
command! -bar LcdStarted execute ':lcd' g:vimrc.path_at_started
command! -bar LcdGitRoot execute ':lcd' system('git rev-parse --show-toplevel')
command! -bar StartedCdBufDir let g:vimrc.path_at_started = expand('%:p:h')
command! -bar -nargs=+ Grep call vimrc#grep_those(<f-args>)
command! -bar TodoList Grep TODO FIXME XXX

" Tapis
function! Tapi_Tabnew(_, args) abort
  let pwd = a:args[0]
  let files = a:args[1:]
  let paths =
    \ map(files, { _, file ->
      \ fnameescape(pwd .. '/' .. file)
    \ })

  for path in paths
    execute 'tabnew' path
  endfor
endfunction
