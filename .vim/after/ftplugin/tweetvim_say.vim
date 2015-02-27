let b:undo_ftplugin = 'setl ' . join([
\	'tabstop<',
\	'shiftwidth<',
\	'expandtab<'
\])

setl tabstop=2
setl shiftwidth=2
setl expandtab

" avoid <C-j> to say
nnoremap <buffer> <C-j> <C-o>o
inoremap <buffer> <C-i> <Tab>
