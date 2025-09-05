local helper = require('helper')
local fn = require('utils.functions')

vim.opt_local.tabstop = 2
vim.opt_local.shiftwidth = 2
vim.opt_local.expandtab = true
vim.opt_local.conceallevel = 0
vim.opt_local.commentstring = ' -- %s'
vim.opt_local.errorformat = '%f:%l:%c:%m' -- format for stack build and stack test

local function stack_test(test_name)
  helper.open_terminal_as('stack_test', 'vertical', 'stack test --fast ' .. test_name, {
    noclose = true,
    path = vim.g.vimrc.path_at_started
  })
end

local function stack_test_tasty()
  stack_test(':tasty')
end

local function stack_test_doctest()
  stack_test(':doctest')
end

local function ghcid_quickfix_start_on_path_started()
  local current = vim.fn.getcwd()
  vim.cmd('copen')
  vim.cmd('LcdStarted')
  vim.cmd('GhcidQuickfixStart')
  vim.cmd('lcd ' .. current)
end

local function stack_integrate_test_or_unit_or_both()
  local options = {
    'a: do tasty-test (default)',
    'b: do doctest',
    'c: do liquid-haskell',
    'd: all'
  }
  print(table.concat(options, '\n'))

  local answer = vim.fn.getchar()
  local selection = {
    a = ':tasty-test',
    b = ':doctest',
    c = 'liquid-haskell',
    d = '',
  }
  local target = fn.get_or_default(selection[answer], ':tasty-test')

  helper.open_terminal_as('stack_test', 'horizontal', 'stack test --fast ' .. target)
end

-- Key mappings
vim.keymap.set('n', '<localleader><localleader>R', stack_test_tasty, { buffer = true, silent = true })
vim.keymap.set('n', '<localleader><localleader><localleader>R', stack_test_doctest, { buffer = true, silent = true })

vim.keymap.set('n', '<localleader><localleader>b', function()
  helper.open_terminal_as('stack_build', 'vertical', 'stack build --fast', {
    noclose = true,
    path = vim.g.vimrc.path_at_started
  })
end, { buffer = true, silent = true })

vim.keymap.set('n', '<localleader><localleader>w', ghcid_quickfix_start_on_path_started, { buffer = true, silent = true })
vim.keymap.set('n', '<localleader><localleader>W', function()
  vim.cmd('GhcidQuickfixStop')
end, { buffer = true, silent = true })

vim.keymap.set('n', '<localleader><localleader>T', stack_integrate_test_or_unit_or_both, { buffer = true, silent = true })
vim.keymap.set('n', '<localleader><localleader>t', function()
  vim.cmd('GhcidQuickfixStart "--command=stack ghci :tasty"')
end, { buffer = true, silent = true })

vim.keymap.set('n', '<localleader>S', function()
  local word = vim.fn.expand('<cword>')
  vim.cmd('Stackage ' .. word)
end, { buffer = true })

local augroup_ftplugin_haskell = vim.api.nvim_create_augroup('FtpluginHaskell', { clear = true })
vim.api.nvim_create_autocmd('BufWritePre', {
  group = augroup_ftplugin_haskell,
  pattern = '*.hs',
  callback = function()
    vim.cmd('HaskellSortImport')
  end,
})
