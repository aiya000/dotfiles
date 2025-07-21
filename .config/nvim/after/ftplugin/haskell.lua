
vim.opt_local.ts = 2
vim.opt_local.sw = 2
vim.opt_local.et = true
vim.opt_local.conceallevel = 0
vim.opt.commentstring = " -- %s"
vim.opt.errorformat = "'%f:%l:%c:%m' \" a format for stack build and stack test"

vim.keymap.set('n', "R", function() vim.call("<SID>stack_test_tasty()") end, { buffer = true, silent = true })
vim.keymap.set('n', "R", function() vim.call("<SID>stack_test_doctest()") end, { buffer = true, silent = true })
-- NOTE: v  This is useful for that is building happy codes and show its warnings/errors on vim-ghcid-quickfix
vim.keymap.set('n', "b", function() vim.call("vimrc#open_terminal_as('stack_build', 'vertical', 'stack build --fast', {'noclose': v:true, 'path': g:vimrc.path_at_started})") end, { buffer = true, silent = true })
vim.keymap.set('n', "w", function() vim.call("<SID>ghcid_quickfix_start_on_path_started()") end, { buffer = true, silent = true })
vim.keymap.set('n', "W", function() vim.cmd("<C-u>GhcidQuickfixStop") end, { buffer = true, silent = true })
vim.keymap.set('n', "T", function() vim.call("<SID>stack_integrate_test_or_unit_or_both()") end, { buffer = true, silent = true })
vim.keymap.set('n', "t", function() vim.cmd("<C-u>GhcidQuickfixStart \"--command=stack ghci :tasty\"") end, { buffer = true, silent = true })
vim.keymap.set('n', "S", function() vim.cmd("<C-u>Stackage <C-r>=expand('<cword>')<CR>") end, { buffer = true })

-- TODO: Detect a context of Eta, and set filetype=eta, please
-- nnoremap <buffer><silent> <localleader><localleader><localleader>r :<C-u>QuickRun eta<CR>
-- nnoremap <buffer><silent> <localleader><localleader><localleader>b :<C-u>echo 'etlas build is started'<CR>:QuickRun etlas_build<CR>
-- nnoremap <buffer><silent> <localleader><localleader><localleader>B :<C-u>call vimrc#open_terminal_as('none', 'horizontal', 'etlas build')<CR>

local augroup_FtpluginHaskell = vim.api.nvim_create_augroup("FtpluginHaskell", { clear = true })
  vim.api.nvim_create_autocmd("BufWritePre", { group = augroup_FtpluginHaskell, pattern = "*.hs", callback = function() vim.cmd("HaskellSortImport") end })

vim.cmd([[
function s:stack_test(test_name) abort
  call vimrc#open_terminal_as(
    \ 'stack_test',
    \ 'vertical',
    \ 'stack test --fast ' .. a:test_name,
    \ {
      \ 'noclose': v:true,
      \ 'path': g:vimrc.path_at_started
    \ })
endfunction
]])

vim.cmd([[
function s:stack_test_tasty() abort
  call s:stack_test(':tasty')
endfunction
]])

vim.cmd([[
function s:stack_test_doctest() abort
  call s:stack_test(':doctest')
endfunction
]])

vim.cmd([[
function s:ghcid_quickfix_start_on_path_started() abort
  let current = getcwd()
  copen
  LcdStarted
  GhcidQuickfixStart
  execute 'lcd' current
endfunction
]])

vim.cmd([[
function s:stack_integrate_test_or_unit_or_both() abort
  echon join([
    \ 'a: do tasty-test (default)',
    \ 'b: do doctest',
    \ 'c: do liquid-haskell',
    \ 'd: all',
  \ ], "\n")

  let answer = getchar()
  let target = answer is char2nr('a') ? ':tasty-test'
    \        : answer is char2nr('b') ? ':doctest'
    \        : answer is char2nr('c') ? 'liquid-haskell'
    \        : answer is char2nr('d') ? ''
    \                                 : ':tasty-test'
  call vimrc#open_terminal_as('stack_test', 'horizontal', 'stack test --fast ' . target)
endfunction
]])
