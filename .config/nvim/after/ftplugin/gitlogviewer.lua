-- This filetype was presented by plugin/gitlogviewer.vim

local list = require('utils.list')

vim.opt_local.list = false
vim.opt_local.cul = true

local function try_show_git_show()
  local ok, err = pcall(show_git_show)
  if not ok then
    vim.notify(err, vim.log.levels.ERROR)
    vim.cmd('close')
    show_git_show()
  end
end

local function show_git_show()
  vim.cmd('normal! [z')
  vim.cmd('vsplit')

  local args = vim.split(vim.b.gitlogviewer_args or '', '%s+')
  if list.has(args, '--oneline') then
    vim.cmd('normal! _"zyiw')
  else
    -- TODO: Currently, this is not working if I'm on wrapped line.
    vim.cmd('normal! g_"zyiw')
  end

  vim.cmd('GitShowViewer ' .. vim.fn.getreg('z'))
end

vim.keymap.set('n', 'Q', function()
  vim.api.nvim_buf_delete(0, { force = true })
end, { buffer = true, silent = true })

vim.keymap.set('n', 'S', try_show_git_show, { buffer = true, silent = true })
vim.keymap.set('n', 'p', try_show_git_show, { buffer = true, silent = true })

vim.keymap.set('n', '<C-r>', function()
  vim.cmd('GitLogViewer ' .. (vim.b.gitlogviewer_args or ''))
end, { buffer = true, silent = true })
