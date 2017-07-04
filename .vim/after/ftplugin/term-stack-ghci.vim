let b:undo_ftplugin = 'setl ' . join([
\	'commentstring<'
\])

setl nonu nornu nolist

nnoremap <buffer> <localleader>r i<End><C-u>:reload<CR>
