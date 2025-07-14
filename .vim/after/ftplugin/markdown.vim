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

" TODO: ちゃんと.vimrcと同様に、lsp_documentSymbolあたりを使う。可能ならここで<C-k><C-f>を押すとオーバーライドするよりも、lspを導入することで済むなら、そちらの方がよい
nnoremap <silent><buffer> <C-k><C-f> :<C-u>call <SID>open_ddu_section_list()<CR>

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
  const port = s:find_free_port(25252)

  try
    call term_start(
      \ $'grip {token_option} {fnameescape(expand('%:p'))} {port}',
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

  call system($'{g:vimrc.open_on_gui} http://localhost:{port}')
endfunction

function! s:find_free_port(port) abort
  if trim(system($"ss -tuln | grep ':{a:port}' | wc -l")) ==# '0'
    return a:port
  endif
  return s:find_free_port(a:port + 1)
endfunction

function! s:open_ddu_section_list() abort
  " TODO: なんかここ効いてないので直す
  normal! gg
  try
    call vimrc#ddu_start_from_input(#{
      \ sources: [#{ name: 'line' }],
      \ sourceOptions: #{
        \ _: #{
          \ matchers: ['matcher_regex'],
        \ },
      \ },
    \ }, '^#+ ')
  finally
    " TODO: なんかここ効いてないので直す
    execute 'normal!' "\<C-o>"
  endtry
endfunction
