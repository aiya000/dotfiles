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

" snowtify
command! -bar SnowtifyBuild call s:Job.start('snowtify build')
command! -bar SnowtifyTest call s:Job.start('snowtify test')

" dein.vim
command! -bar DeinInstall   call dein#install()
command! -bar DeinUpdate    call dein#update()
command! -bar DeinLog       new \| setl buftype=nofile noreadonly modifiable ft=deinlog \| put=dein#get_log()
command! -bar DeinUpdateLog new \| setl buftype=nofile noreadonly modifiable ft=deinlog \| put=dein#get_updates_log()
