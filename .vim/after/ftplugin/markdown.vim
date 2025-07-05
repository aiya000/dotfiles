let b:undo_ftplugin = 'setlocal ' .. join([
  \ 'tabstop<',
  \ 'shiftwidth<',
  \ 'conceallevel<',
  \ 'commentstring<',
  \ 'completefunc<',
\ ])
setlocal
  \ tabstop=4
  \ shiftwidth=4
  \ conceallevel=0
  \ commentstring=\ <!--\ %s\ -->
  \ completefunc=github_complete#complete

nnoremap <silent><buffer> <localleader>r <Cmd>PrevimOpen<CR>
nnoremap <silent><buffer> <localleader><localleader>d <Cmd>w<CR>:!doctoc %<CR>:edit %<CR>
" nnoremap <silent><buffer> <localleader>f <Cmd>!textlint --fix <C-r>=expand('%:p')<CR><CR>
" nmap <silent><buffer> <C-l> <C-[>:syntax sync fromstart<CR>

nnoremap <silent><buffer> <localleader><localleader>r :<C-u>call <SID>start_grip()<CR>
" TODO: Do 'gg' after glow finished
nnoremap <silent><buffer> <localleader><localleader>R <Cmd>call term_start(
  \ $'glow {fnameescape(expand('%:p'))}',
  \ #{ vertical: v:true }
\ )<CR>

syntax sync fromstart

if g:vimrc->has_key('git_root') && filereadable($'{g:vimrc.git_root}/.textlintrc')
  if !g:ale_fixers->has_key('markdown')
    let g:ale_fixers.markdown = []
  endif

  call add(g:ale_fixers.markdown, 'textlint')
endif

function! s:start_grip() abort
  const token_option = $DOTFILES_PRIVATE_GITHUB_GRIP_TOKEN ==# ''
    \ ? ''
    \ : $'--pass {$DOTFILES_PRIVATE_GITHUB_GRIP_TOKEN}'

  try
    call term_start(
      \ $'grip {token_option} {fnameescape(expand('%:p'))} 25252',
      \ #{
        \ vertical: v:true,
        \ hidden: v:true,
        \ term_finish: 'close',
      \ },
    \ )
    " NOTE: なぜか`setl nonumber norelativenumber nolist`になるので、とりあえず直打ちで直している
    " TODO: なんでこうなるのか調査して、修正する
    setl number relativenumber list
  catch
    echomsg $'grip error: {v:exception}'
  endtry

  call system($'{g:vimrc.open_on_gui} http://localhost:25252')
endfunction
