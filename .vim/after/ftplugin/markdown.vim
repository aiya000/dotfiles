let b:undo_ftplugin = 'setl ' . join([
  \ 'tabstop<',
  \ 'shiftwidth<',
  \ 'expandtab<',
  \ 'completefunc<',
  \ 'commentstring<',
  \ 'conceallevel<',
\ ])

setl tabstop=4 shiftwidth=4 expandtab
setl completefunc=github_complete#complete
let &commentstring = '<!-- %s -->'
set conceallevel=0

nnoremap <silent><buffer> <localleader>R :<C-u>w<CR>:!doctoc %<CR>:edit %<CR>
nnoremap <silent><buffer> <localleader>r :<C-u>PrevimOpen<CR>
nnoremap <silent><buffer> <localleader><localleader>r :<C-u>call <SID>open_grip()<CR>
nnoremap <silent><buffer> <localleader><localleader>R :<C-u>silent !shiba % > /dev/null 2>&1 &<CR>
"nnoremap <silent><buffer> <localleader>f :<C-u>!textlint --fix <C-r>=expand('%:p')<CR><CR>
nmap <silent><buffer> <C-l> <C-[>:syntax sync fromstart<CR>

vnoremap <silent><buffer> i{ :<C-u>call <SID>organize_this_table()<CR>

syntax sync fromstart

function! s:open_grip() abort
    let cmd = printf('grip --user %s --pass %s --browser %s',
        \ g:vimrc.github.username,
        \ g:vimrc.github.password,
        \ expand('%:p'),
    \ )
    call vimrc#open_terminal_as('none', 'horizontal', cmd)
    hide
endfunction

"TODO: Don't remove `:--`, `:-:`, `--:`
function! s:organize_this_table() abort
  execute 'normal' "viio\<Esc>j"
  execute 's/-//g'
  execute 'normal' "vii:Alignta => \|\<CR>"
  normal! j
  execute 's/[^\|]/-/g'
  normal! k
endfunction
