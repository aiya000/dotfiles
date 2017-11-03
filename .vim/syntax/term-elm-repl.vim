let s:elm_syntax_file = g:vimrc['vim_home'] . '/bundle/repos/github.com/ElmCast/elm-vim/syntax/elm.vim'
if filereadable(s:elm_syntax_file)
    execute 'source' s:elm_syntax_file
else
    echomsg s:elm_syntax_file . ' is not found'
endif
