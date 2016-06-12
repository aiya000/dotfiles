let b:undo_ftplugin = 'setl ' . join([
\	'tabstop<',
\	'shiftwidth<',
\	'expandtab<',
\	'commentstring<'
\])

setl tabstop=4
setl shiftwidth=4
setl expandtab
let &commentstring = '<!--%s-->'

nnoremap <silent><buffer> <localleader>r :<C-u>PrevimOpen<CR>
nnoremap <silent><buffer> <Esc>          <Esc>:syntax sync fromstart<CR>
nmap             <buffer> <C-l>          <Esc>

" Disable deleting tail spaces
nnoremap <buffer> <C-k><Space> <NOP>

syntax sync fromstart
