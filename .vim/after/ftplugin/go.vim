let b:undo_ftplugin = 'setl ' . join([
\	'commentstring<',
\])

let &commentstring = ' /*%s*/'
execute 'setl dict+=' . (g:vimrc['vim_home'] . '/dict/filetype/go.dict')
