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

command! -bar -nargs=? -complete=filetype FtDetectEdit
\	execute ':edit' printf('%s/ftdetect/%s.vim', g:vimrc['vim_home'], (empty(<q-args>) ? &filetype : <q-args>))

command! -bar -nargs=? -complete=filetype PluginEdit
\	execute ':edit' printf('%s/plugin/%s.vim', g:vimrc['vim_home'], (empty(<q-args>) ? &filetype : <q-args>))

command! -bar -nargs=? -complete=filetype AutoloadEdit
\	execute ':edit' printf('%s/autoload/%s.vim', g:vimrc['vim_home'], (empty(<q-args>) ? &filetype : <q-args>))

" Dev
command! -bar -nargs=1 TestCodeEdit TabnewOverridden ~/.tmp/Test.<args>
