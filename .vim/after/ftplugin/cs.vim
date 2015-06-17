let b:undo_ftplugin = 'setl ' . join([
\	'commentstring<',
\	'cinkeys<'
\])

let &commentstring = ' /*%s*/'
setl cinkeys-=0#

highlight default link ftCsTypeInference Identifier
syntax keyword ftCsTypeInference var

augroup MyFtpluginCSharp
	autocmd!
	autocmd FileType,BufEnter,BufWinEnter {*.,}cs syntax keyword ftCsTypeInference var
augroup END
