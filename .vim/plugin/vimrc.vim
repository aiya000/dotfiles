" For vimrc
command! -bar VimConfig    e $MYVIMRC
command! -bar VimConfigTab tabnew $MYVIMRC
command! -bar Reload       so $MYVIMRC
\|	if has('gui_running') && filereadable($MYGVIMRC)
\|		so $MYGVIMRC
\|	endif

" Edit .vim/*/
" g:vimrc is defined by .vimrc
command! -bar -nargs=? -complete=filetype FtpluginEditAfter
\	execute ':edit' printf('%s/after/ftplugin/%s.vim', g:vimrc['vim_home'], (empty(<q-args>) ? &filetype : <q-args>))
command! -bar -nargs=? -complete=filetype FtDictionaryEdit
\	execute ':edit' printf('%s/dict/filetype/%s.dict', g:vimrc['vim_home'], (empty(<q-args>) ? &filetype : <q-args>))
command! -bar -nargs=? -complete=filetype SyntaxEdit
\	execute ':edit' printf('%s/syntax/%s.vim', g:vimrc['vim_home'], (empty(<q-args>) ? &filetype : <q-args>))
command! -bar -nargs=? -complete=filetype IndentEdit
\	execute ':edit' printf('%s/indent/%s.vim', g:vimrc['vim_home'], (empty(<q-args>) ? &filetype : <q-args>))

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
