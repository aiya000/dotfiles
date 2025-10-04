---特定のプラグインに関連する操作をここに書かないでください。
---我々はこれによって、繰り返し失敗してきました

local helper = require('helper')

local M = {}

local augroup = vim.api.nvim_create_augroup('InitLuaAutocmds', { clear = true })

-- Move the cursor position to the last position of a file
vim.api.nvim_create_autocmd('BufReadPost', {
  group = augroup,
  callback = function()
    vim.cmd(':' .. vim.fn.line('\'"'))
  end,
})

-- Setup Cmdwin
vim.api.nvim_create_autocmd('CmdwinEnter', {
  group = augroup,
  callback = function()
    vim.keymap.set('n', 'Q', '<Cmd>quit<CR>', { buffer = true, silent = true })
    vim.keymap.set('n', 'ghq', '<Cmd>quit<CR>', { buffer = true, silent = true })
    vim.keymap.set('n', '<C-l>', '<Cmd>quit<CR>', { buffer = true })
    vim.keymap.set('n', 'ghQ', '<Cmd>qall<CR>', { buffer = true, silent = true })
    vim.keymap.set('n', '<C-j>', '<Esc><CR>', { buffer = true })
    vim.keymap.set('n', '<CR>', '<Esc><CR>', { buffer = true })
  end,
})

-- Show :help by vertical split default
vim.api.nvim_create_autocmd('BufWinEnter', {
  group = augroup,
  pattern = { '*.txt' },
  callback = function()
    if vim.bo.filetype == 'help' then
      vim.cmd('wincmd H')
    end
  end,
})

-- cmdpalette.nvimと連携するための、疑似的な`nnoremap ::: :<C-u>%s/`
vim.api.nvim_create_autocmd('CmdlineChanged', {
  group = augroup,
  callback = function()
    helper.replace_line(
      {
        [':'] = '%s/',
      },
      vim.fn.getcmdline(),
      vim.fn.setcmdline
    )
  end,
})

-- Show relative numbers only on the current window {{{

vim.api.nvim_create_autocmd({ 'BufEnter', 'WinEnter' }, {
  group = augroup,
  callback = function()
    if vim.wo.number then
      vim.wo.relativenumber = true
    end
  end,
})

vim.api.nvim_create_autocmd({ 'BufLeave', 'WinLeave' }, {
  group = augroup,
  callback = function()
    vim.wo.relativenumber = false
  end,
})

-- }}}

return M
