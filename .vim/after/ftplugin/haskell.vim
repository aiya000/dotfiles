let b:undo_ftplugin = 'setl ' . join([
\	'tabstop<',
\	'shiftwidth<',
\	'expandtab<',
\	'commentstring<'
\])

setl tabstop=2
setl shiftwidth=2
setl expandtab
let &commentstring = ' -- %s'

augroup MyFtpluginHaskell
	autocmd!
	autocmd VimEnter,ColorScheme * highlight ftHaskellHeadSpace cterm=underline ctermfg=DarkGray
	autocmd VimEnter             * call matchadd('ftHaskellHeadSpace', '^\s\+')
augroup END
