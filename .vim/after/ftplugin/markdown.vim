let b:undo_ftplugin = 'setl ' . join([
\	'tabstop<',
\	'shiftwidth<',
\	'expandtab<',
\	'completefunc<',
\	'commentstring<'
\])

setl tabstop=4
setl shiftwidth=4
setl expandtab
setl completefunc=github_complete#complete
setl omnifunc=github_complete#complete
let &commentstring = '<!--%s-->'

" require `npm install -g doctoc`
nnoremap <silent><buffer> <localleader>R :<C-u>w<CR>:!doctoc %<CR>:edit %<CR>
nnoremap <silent><buffer> <localleader>r :<C-u>PrevimOpen<CR>
nnoremap <silent><buffer> <Esc>          <Esc>:syntax sync fromstart<CR>
nmap             <buffer> <C-l>          <Esc>

" Disable deleting tail spaces
nnoremap <buffer> <C-k><Space> <NOP>

syntax sync fromstart
