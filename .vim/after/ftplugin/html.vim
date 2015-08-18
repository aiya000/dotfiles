let b:undo_ftplugin = 'setl ' . join([
\	'tabstop<',
\	'shiftwidth<',
\	'expandtab<'
\])

setl tabstop=2
setl shiftwidth=2
setl expandtab

nnoremap <silent><buffer> <localleader>r :<C-u>!xdg-open %<CR><CR>
