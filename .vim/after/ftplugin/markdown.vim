setl tabstop=4 shiftwidth=4 expandtab
setl completefunc=github_complete#complete
let &commentstring = '<!-- %s -->'
set conceallevel=0

nnoremap <silent><buffer> <localleader>r :<C-u>PrevimOpen<CR>
nnoremap <silent><buffer> <localleader><localleader>d :<C-u>w<CR>:!doctoc %<CR>:edit %<CR>
nnoremap <silent><buffer> <localleader><localleader>r :<C-u>call <SID>open_grip()<CR>
" nnoremap <silent><buffer> <localleader>f :<C-u>!textlint --fix <C-r>=expand('%:p')<CR><CR>
" nmap <silent><buffer> <C-l> <C-[>:syntax sync fromstart<CR>

syntax sync fromstart

if g:vimrc->has_key('git_root') && filereadable(g:vimrc.git_root .. '/.textlintrc')
  let g:ale_fixers['markdown'] = ['textlint']
endif

function s:open_grip() abort
  let cmd = printf(
    \ 'grip --pass %s --browser %s',
    \ g:vimrc.github.access_token,
    \ expand('%:p'),
  \ )
  call vimrc#open_terminal_as('none', 'horizontal', cmd)
  hide
endfunction
