let s:clojure_syntax_file = g:vimrc['vim_home'] . '/bundle/repos/github.com/thinca/vim-ft-clojure/syntax/clojure.vim'
if filereadable(s:clojure_syntax_file)
	execute 'source' s:clojure_syntax_file
else
	echomsg 'syntax/clojure.vim was not found'
endif
unlet s:clojure_syntax_file
