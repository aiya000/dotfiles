let b:undo_ftplugin = 'setl ' . join([
\	'tabstop<',
\	'shiftwidth<',
\	'expandtab<',
\	'completefunc<',
\	'commentstring<',
\	'conceallevel<',
\])

setl tabstop=4 shiftwidth=4 expandtab
setl completefunc=github_complete#complete
setl omnifunc=github_complete#complete
let &commentstring = '<!--%s-->'
setl conceallevel=2

" + Require
"   - npm install -g doctoc
"   - yaourt -S python-grip
nnoremap <silent><buffer> <localleader>R :<C-u>w<CR>:!doctoc %<CR>:edit %<CR>
nnoremap <silent><buffer> <localleader>r :<C-u>PrevimOpen<CR>
nnoremap <silent><buffer> <localleader><localleader>r :<C-u>sp \| resize 3 \| terminal grip --browser %<CR>
nnoremap <silent><buffer> <Esc> <Esc>:syntax sync fromstart<CR>
nmap             <buffer> <C-l> <Esc>

" Disable deleting tail spaces
nnoremap <buffer> <C-k><Space> <NOP>

syntax sync fromstart
