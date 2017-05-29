let b:undo_ftplugin = 'setl ' . join([
\	'ts<',
\	'sw<',
\	'et<',
\])

setl ts=2 sw=2 et

nnoremap <silent><buffer> <localleader>r :<C-u>ReadBangBuf themis <C-r>=expand('%')<CR><CR>
