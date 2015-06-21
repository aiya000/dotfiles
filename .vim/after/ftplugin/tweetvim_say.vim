let b:undo_ftplugin = 'setl ' . join([
\	'number<',
\	'relativenumber<',
\	'tabstop<',
\	'shiftwidth<',
\	'expandtab<'
\])

setl nonumber
setl norelativenumber
setl tabstop=2
setl shiftwidth=2
setl expandtab

" avoid <C-j> to say
nnoremap <buffer> <C-j> <C-o>o
inoremap <buffer> <C-i> <Tab>
