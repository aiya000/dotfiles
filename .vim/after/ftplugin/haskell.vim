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

highlight ftHaskellHeadSpace cterm=underline ctermfg=DarkGray gui=underline guifg=Cyan
augroup MyFtpluginHaskell
	autocmd!
	autocmd ColorScheme * highlight ftHaskellHeadSpace cterm=underline ctermfg=DarkGray gui=underline guifg=Cyan
	autocmd BufWinEnter *.hs call matchadd('ftHaskellHeadSpace', '^\s\+')
augroup END
