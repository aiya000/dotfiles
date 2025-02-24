setl tabstop=4 shiftwidth=4 expandtab
setl completefunc=github_complete#complete
let &commentstring = '<!-- %s -->'
set conceallevel=0

nnoremap <silent><buffer> <localleader>r <Cmd>PrevimOpen<CR>
nnoremap <silent><buffer> <localleader><localleader>d <Cmd>w<CR>:!doctoc %<CR>:edit %<CR>
" nnoremap <silent><buffer> <localleader>f <Cmd>!textlint --fix <C-r>=expand('%:p')<CR><CR>
" nmap <silent><buffer> <C-l> <C-[>:syntax sync fromstart<CR>

nnoremap <silent><buffer> <localleader><localleader>r <Cmd>call vimrc#open_terminal_as(
  \ 'none',
  \ 'horizontal',
  \ $'grip --pass {g:vimrc.github.access_token} --browser "{expand("%:p")}"'
\ )<CR>

syntax sync fromstart

if g:vimrc->has_key('git_root') && filereadable($'{g:vimrc.git_root}/.textlintrc')
  if !g:ale_fixers->has_key('markdown')
    let g:ale_fixers.markdown = []
  endif

  call add(g:ale_fixers.markdown, 'textlint')
endif
