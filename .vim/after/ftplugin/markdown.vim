let b:undo_ftplugin = 'setl ' . join([
\	'tabstop<',
\	'shiftwidth<',
\	'expandtab<',
\	'commentstring<'
\])

setl tabstop=2
setl shiftwidth=2
setl expandtab
let &commentstring = '<!--%s-->'

nnoremap <silent><buffer> <localleader>r :<C-u>PrevimOpen<CR>
nnoremap <silent><buffer> <Esc>          <Esc>:syntax sync fromstart<CR>
nmap             <buffer> <C-l>          <Esc>

syntax sync fromstart
