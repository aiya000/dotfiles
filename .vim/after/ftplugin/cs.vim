let b:undo_ftplugin = 'setl ' . join([
\	'commentstring<'
\])

let &commentstring = ' /*%s*/'

augroup MyFtpluginCSharp
	autocmd!
	autocmd VimEnter,ColorScheme      *    highlight default link GrcTypeInference Identifier
	autocmd VimEnter,WinEnter,BufRead *.cs syntax keyword GrcTypeInference var
augroup END
