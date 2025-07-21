vim.cmd("setlocal")
  vim.cmd("\\ tabstop=4")
  vim.cmd("\\ shiftwidth=4")
  vim.cmd("\\ conceallevel=0")
  vim.cmd("\\ commentstring=\\ <!--\\ %s\\ -->")
  vim.cmd("\\ completefunc=github_complete#complete")

vim.keymap.set('n', "r", function() vim.cmd("PrevimOpen") end, { buffer = true, silent = true })
vim.keymap.set('n', "d", function() vim.cmd("w<CR>:!doctoc %<CR>:edit %") end, { buffer = true, silent = true })
-- nnoremap <silent><buffer> <localleader>f <Cmd>!textlint --fix <C-r>=expand('%:p')<CR><CR>
-- nmap <silent><buffer> <C-l> <C-[>:syntax sync fromstart<CR>

vim.keymap.set('n', "r", function() vim.call("<SID>start_grip()") end, { buffer = true, silent = true })
-- TODO: Do 'gg' after glow finished
vim.keymap.set('n', "R", "<Cmd>call term_start(", { buffer = true, silent = true })
  vim.cmd("\\ $'glow {fnameescape(expand('%:p'))}',")
  vim.cmd("\\ #{ vertical: v:true }")
vim.cmd("\\ )<CR>")

-- TODO: ちゃんと.vimrcと同様に、lsp_documentSymbolあたりを使う。可能ならここで<C-k><C-f>を押すとオーバーライドするよりも、lspを導入することで済むなら、そちらの方がよい
vim.keymap.set('n', "<silent><buffer>", "<C-k><C-f> :<C-u>call <SID>open_ddu_section_list()<CR>", { buffer = true, silent = true })

vim.cmd("syntax sync fromstart")

vim.cmd([[
if g:vimrc->has_key('git_root') && filereadable($'{g:vimrc.git_root}/.textlintrc')
  if !g:ale_fixers->has_key('markdown')
    let g:ale_fixers.markdown = []
  endif

  call add(g:ale_fixers.markdown, 'textlint')
endif
]])

vim.cmd([[
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
]])

vim.cmd([[
function! s:find_free_port(port) abort
  if trim(system($"ss -tuln | grep ':{a:port}' | wc -l")) ==# '0'
    return a:port
  endif
  return s:find_free_port(a:port + 1)
endfunction
]])

vim.cmd([[
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
]])
