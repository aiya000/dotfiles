let b:undo_ftplugin = 'setl ' . join([
\   'tabstop<',
\   'shiftwidth<',
\   'expandtab<',
\   'completefunc<',
\   'commentstring<',
\   'conceallevel<',
\])

setl tabstop=4 shiftwidth=4 expandtab
setl completefunc=github_complete#complete
setl omnifunc=github_complete#complete
let &commentstring = '<!--%s-->'
set conceallevel=0

" 論文
command! -bar Ronbunize call s:ronbunize()

" 論文
function! s:ronbunize() abort
    :%s/、/，/g
    :%s/。/．/g
endfunction

" + Require
"   - npm install -g doctoc
"   - yaourt -S python-grip
nnoremap <silent><buffer> <localleader>R :<C-u>w<CR>:!doctoc %<CR>:edit %<CR>
nnoremap <silent><buffer> <localleader>r :<C-u>PrevimOpen<CR>
nnoremap <silent><buffer> <localleader><localleader>r :<C-u>call vimrc#open_terminal_as('none', 'horizontal', 'grip --browser ' . expand('%')) \| resize 3<CR>
nnoremap <silent><buffer> <localleader><localleader>R :<C-u>silent !shiba % > /dev/null 2>&1 &<CR>
nnoremap <silent><buffer> <Esc> <Esc>:syntax sync fromstart<CR>
nmap             <buffer> <C-l> <Esc>

" Disable deleting tail spaces
nnoremap <buffer> <C-k><Space> <NOP>

syntax sync fromstart
