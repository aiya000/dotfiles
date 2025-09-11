-- This filetype defined by plugin/gitlog.vim

local list = require('utils.list')

vim.opt_local.list = false
vim.opt_local.cul = true

local function open_git_show()
  vim.cmd('normal! [z')
  vim.cmd('vsplit')

  local args = vim.split(vim.b.gitlog_args or '', '%s+')
  if list.has(args, '--oneline') then
    vim.cmd('normal! _"zyiw')
  else
    -- TODO: Currently, this is not working if I'm on wrapped line.
    vim.cmd('normal! g_"zyiw')
  end

  vim.cmd('GitShow ' .. vim.fn.getreg('z'))
end

vim.keymap.set('n', 'Q', function()
  vim.api.nvim_buf_delete(0, { force = true })
end, { buffer = true, silent = true })

-- Refresh
vim.keymap.set('n', '<C-r>', function()
  vim.cmd('GitLog ' .. (vim.b.gitlog_args or ''))
end, { buffer = true, silent = true })

vim.keymap.set('n', 'S', open_git_show, { buffer = true, silent = true })
