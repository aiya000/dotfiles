let s:Job = vital#vimrc#new().import('System.Job')

" Vim common
"" buffer open commands with filetype 'none'
command! -bar -bang NewOverridden new<bang> | if empty(&ft) | setf none | endif
command! -bar -bang -complete=file -nargs=? EditOverridden e<bang> <args> | if empty(&ft) | setf none | endif
command! -bar -bang -complete=file -nargs=? VnewOverridden vnew<bang> <args> | if empty(&ft) | setf none | endif
command! -bar -bang -complete=file -nargs=? EnewOverridden enew<bang> <args> | if empty(&ft) | setf none | endif
command! -bar -bang -complete=file -nargs=? TabnewOverridden tabnew<bang> <args> | if empty(&ft) | setf none | endif
"" Clear quickfix
command! -bar CClear call setqflist([])
"" Rename a file of the current buffer
command! -bar -nargs=1 -complete=file Rename call vimrc#cmd#rename_to(<q-args>)
"" Pull and Insert <title>\(.*\)</title>
command! -bar -nargs=1 InsertWebPageTitle execute 'normal! i' . vimrc#cmd#pull_webpage_title(<q-args>)
"" Save session and specify session name automatically
command! -bar SessionSaveInGitBranch call vimrc#cmd#git_branch_session_save()
"" CSS
command! -bar CssShowDecompressed call vimrc#cmd#decompress_to_buffer()
"" Others
command! -bar -nargs=* Vim call vimrc#open_terminal_as('term-vim', 'stay', 'vim ' . <q-args>)
command! -bar Memo sp ~/vim-memo.md
command! -bar CdGitRoot execute ':cd' system('git rev-parse --show-toplevel')
command! -bar -nargs=+ Grep echo {-> [execute('grep ' . <q-args> . ' %', "silent!"), execute('copen')]}()

" Developments
command! -bar -nargs=1 TestCodeEdit EditOverridden ~/.tmp/Test.<args>
command! -bar CtagsAuto call vimrc#plugins#ctags_auto()<CR>
"" Haskell
command! -bar HaskDogs call vimrc#plugins#execute_haskdogs_async()
command! -bar EtaDogs call vimrc#plugins#execute_haskdogs_in_eta_async()
command! -bar -nargs=* WatchExecStack call vimrc#plugins#watchexec_stack_quickfix(<q-args>)
command! -bar -nargs=* QuickfixRunStack call vimrc#plugins#run_stack_quickfix(<q-args>)
"TODO: ^^^ Detect hs-sonoda/src/Sonoda/Types/Lexer.hs:7:1: warning: [-Wunused-imports] as a warning
" REPLs
command! -bar -nargs=? Ghci call vimrc#open_terminal_as('term-stack-exec-ghci', 'stay', 'stack exec ghci ' . <q-args>)
"NOTE: 'e' suffix means 'environment of the project' :D
command! -bar -nargs=? Ghcie call vimrc#open_terminal_as('term-stack-ghci', 'stay', 'stack ghci ' . <q-args>)
command! -bar -nargs=? GhcieTastyTest call vimrc#open_terminal_as('term-stack-ghci', 'stay', 'stack ghci :tasty-test ' . <q-args>)
command! -bar -nargs=? IdrisRepl call vimrc#open_terminal_as('term-idris', 'stay', 'idris ' . <q-args>)
command! -bar CLisp call vimrc#open_terminal_as('none', 'stay', 'clisp')
command! -bar LeinRepl call vimrc#open_terminal_as('none', 'stay', 'lein repl')
command! -bar ElmRepl call vimrc#open_terminal_as('term-elm-repl', 'stay', 'elm repl')
command! -bar PythonRepl call vimrc#open_terminal_as('none', 'stay', 'PAGER=cat python')
command! -bar IPyRepl call vimrc#open_terminal_as('none', 'stay', 'ipython')
command! -bar SwiftRepl call vimrc#open_terminal_as('none', 'stay', 'swift')

" Plugins
"" dein.vim
command! -bar DeinInstall   call dein#install()
command! -bar DeinUpdate    call dein#update()
command! -bar DeinLog       new | setl buftype=nofile noreadonly modifiable ft=deinlog | put=dein#get_log()
command! -bar DeinUpdateLog new | setl buftype=nofile noreadonly modifiable ft=deinlog | put=dein#get_updates_log()
command! -bar DeinRecacheRuntimepath call dein#recache_runtimepath()
"" dein-ui.vim
"command! -bar DeinUpdate SPUpdate

" git commands
command! -bar -nargs=* GStatus Gina status -s <args>
command! -bar -nargs=* GLog GitLogViewer -100 --name-only <args>
command! -bar -nargs=* GLP GitLogViewer --patch -100 <args>
command! -bar -nargs=* GDiff GitDiffViewer <args>
command! -bar -nargs=* GDS GitDiffViewer --staged <args>
command! -bar -nargs=* GDH GitDiffViewer HEAD~ <args>
command! -bar -nargs=* GCommit Gina commit --verbose <args>
command! -bar -nargs=* GCAM Gina commit --verbose --amend <args>
command! -bar -nargs=1 GCF echomsg system('git commit --fixup ' . <q-args>)
command! -bar -nargs=* GAP terminal git add -p <args>
command! -bar -nargs=* GTree Gina log --graph --decorate --oneline <args>
command! -bar -nargs=* GTreeAll GTree --all <args>
command! -bar -nargs=* GBA Gina branch --all <args>
command! -bar -nargs=* GBlame Gina blame <args>

" espeak
command! -bar -nargs=* EspeakSay call vimrc#plugins#espeak_say(<q-args>)
command! -bar EspeakDoesntSay call vimrc#plugins#espeak_doesnt_say()
