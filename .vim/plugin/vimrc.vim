let s:Job = vital#vimrc#new().import('System.Job')


" buffer open commands with filetype 'none'
command! -bar -bang NewOverridden new<bang> | if empty(&ft) | setf none | endif
command! -bar -bang -complete=file -nargs=? EditOverridden e<bang> <args> | if empty(&ft) | setf none | endif
command! -bar -bang -complete=file -nargs=? VnewOverridden vnew<bang> <args> | if empty(&ft) | setf none | endif
command! -bar -bang -complete=file -nargs=? EnewOverridden enew<bang> <args> | if empty(&ft) | setf none | endif
command! -bar -bang -complete=file -nargs=? TabnewOverridden tabnew<bang> <args> | if empty(&ft) | setf none | endif

" Rename the file of current buffer
command! -bar -nargs=1 -complete=file Rename call vimrc#cmd#rename_to(<q-args>)

" Pull and Insert <title>\(.*\)</title>
command! -bar -nargs=1 InsertWebPageTitle execute 'normal! i' . vimrc#cmd#pull_webpage_title(<q-args>)

" r! to scratch buffer
command! -bar -nargs=* ReadBangBuf call vimrc#cmd#read_bang_to_buf(<q-args>)

" Save session and specify session name automatically
command! -bar SessionSaveInGitBranch call vimrc#cmd#git_branch_session_save()

" CSS
command! -bar CssShowDecompressed call vimrc#cmd#decompress_to_buffer()

"
command! -bar -nargs=* Vim call vimrc#open_terminal_as('term-vim', 'stay', 'vim ' . <q-args>)
command! -bar Memo sp ~/vim-memo.md
command! -bar CdGitRoot execute ':cd' system('git rev-parse --show-toplevel')

" Haskell
command! -bar -nargs=? Snowtify call s:Job.start('snowtify ' . <q-args>)
command! -bar -nargs=? SnowtifyWatchStart call vimrc#plugins#start_snowtify_watch(<q-args>)
command! -bar SnowtifyWatchStop call vimrc#plugins#stop_snowtify_watch()
command! -bar HaskDogs call vimrc#plugins#execute_haskdogs_async({-> execute('echom ""')})
command! -bar EtaDogs call vimrc#plugins#execute_haskdogs_in_eta_async()

" dein.vim
command! -bar DeinInstall   call dein#install()
command! -bar DeinUpdate    call dein#update()
command! -bar DeinLog       new | setl buftype=nofile noreadonly modifiable ft=deinlog | put=dein#get_log()
command! -bar DeinUpdateLog new | setl buftype=nofile noreadonly modifiable ft=deinlog | put=dein#get_updates_log()
command! -bar DeinRecacheRuntimepath call dein#recache_runtimepath()

" REPLs
command! -bar -nargs=? Ghci call vimrc#open_terminal_as('term-stack-exec-ghci', 'stay', 'stack exec ghci ' . <q-args>)
"NOTE: 'e' suffix means 'environment of the project' :D
command! -bar Ghcie call vimrc#open_terminal_as('term-stack-ghci', 'stay', 'stack ghci')
command! -bar CLisp call vimrc#open_terminal_as('none', 'stay', 'clisp')
command! -bar LeinRepl call vimrc#open_terminal_as('none', 'stay', 'lein repl')
command! -bar ElmRepl call vimrc#open_terminal_as('term-elm-repl', 'stay', 'elm repl')
command! -bar PythonRepl call vimrc#open_terminal_as('none', 'stay', 'PAGER=cat python')
command! -bar IPyRepl call vimrc#open_terminal_as('none', 'stay', 'ipython')

" git
command! -bar -nargs=* GStatus Gina status -s <args>
command! -bar -nargs=* GLog GitLogViewer -100 <args>
command! -bar -nargs=* GLP GitLogViewer --patch -100 <args>
command! -bar -nargs=* GDiff GitDiffViewer <args>
command! -bar -nargs=* GDS GitDiffViewer --staged <args>
command! -bar -nargs=* GDH GitDiffViewer HEAD~ <args>
command! -bar -nargs=* GCommit Gina commit <args>
command! -bar -nargs=* GCAM Gina commit --amend <args>
command! -bar -nargs=* GAP Gina!! add -p <args>
