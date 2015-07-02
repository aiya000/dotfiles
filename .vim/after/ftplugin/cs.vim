let b:undo_ftplugin = 'setl ' . join([
\	'commentstring<',
\	'cinkeys<'
\])

let &commentstring = ' /*%s*/'
setl cinkeys-=0#

highlight default link ftCsTypeSpecial Identifier
syntax keyword ftCsTypeSpecial var dynamic

augroup MyFtpluginCSharp
	autocmd!
	autocmd FileType,BufEnter,BufWinEnter {*.,}cs syntax keyword ftCsTypeSpecial var dynamic
augroup END
