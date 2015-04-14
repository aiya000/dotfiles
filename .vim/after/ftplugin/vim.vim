" -- Vi Improved --

let b:undo_ftplugin = 'setl ' . join([
\	'commentstring<'
\])

let &commentstring = ' "%s'

highlight ftVimMyHint cterm=standout ctermfg=DarkYellow gui=bold guifg=#ef5939
augroup MyFtpluginVim
	autocmd!
	autocmd VimEnter,ColorScheme * highlight ftVimMyHint
	\                              cterm=standout ctermfg=DarkYellow
	\                              gui=bold guifg=#ef5939
	"@Incomplete('this do not functioned')
	autocmd FileType,BufEnter,BufWinEnter {*.,}vim call matchadd('ftVimMyHint', '\s*"\zs@\w\+(.*)\ze')
augroup END
