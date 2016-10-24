"TODO: Implement without exceptions
"augroup MyFtpluginIntGhci
"	autocmd!
"	"TODO: restore only if neocomplete unlocked when winentered
"	autocmd BufEnter,WinEnter * NeoCompleteLock
"	autocmd BufLeave,WinLeave * NeoCompleteUnLock
"augroup END

let b:undo_ftplugin = 'setl ' . join([
\	'nonumber<',
\	'norelativenumber<'
\])

setl nonumber norelativenumber
