let b:undo_ftplugin = 'setl ' . join([
\	'commentstring<'
\])

let &commentstring = ' /*%s*/'

highlight default link ftCsTypeInference Identifier
augroup MyFtpluginCSharp
	autocmd!
	autocmd BufWinEnter *.cs syntax keyword ftCsTypeInference var
augroup END
