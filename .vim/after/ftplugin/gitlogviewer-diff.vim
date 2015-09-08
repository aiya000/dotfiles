" This filetype was presented by .vimrc

let b:undo_ftplugin = 'setl ' . join([
\	'nolist<'
\])
setl nolist

nnoremap <buffer><silent> Q :<C-u>bdelete!<CR>
