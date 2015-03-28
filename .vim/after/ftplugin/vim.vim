" -- Vi Improved --

let b:undo_ftplugin = 'setl ' . join([
\	'commentstring<'
\])

let &commentstring = ' "%s'

augroup MyFtpluginVim
	autocmd!
	autocmd VimEnter,ColorScheme * highlight ftVimMyHint
	\                              cterm=standout ctermfg=DarkYellow
	\                              gui=bold guifg=#ef5939
	autocmd VimEnter             * call matchadd('ftVimMyHint', '\s*"\zs@\w\+(.*)\ze')
augroup END
