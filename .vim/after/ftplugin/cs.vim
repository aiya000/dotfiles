let b:undo_ftplugin = 'setl ' . join([
\	'commentstring<'
\])

let &commentstring = ' /*%s*/'

highlight default link ftCsTypeInference Identifier
syntax keyword ftCsTypeInference var

augroup MyFtpluginCSharp
	autocmd!
	autocmd FileType,BufEnter,BufWinEnter {*.,}cs syntax keyword ftCsTypeInference var
augroup END
