-- This filetype defined by plugin/gitlog.vim

local git_log = require('git-log')
local list = require('utils.list')

vim.opt_local.list = false
vim.opt_local.cul = true

local function open_git_show()
  vim.cmd('normal! [z')
  vim.cmd('vsplit')

  if list.has(vim.b.gitlog_args or {}, '--oneline') then
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
  git_log.open_buffer(vim.b.gitlog_args)
end, { buffer = true, silent = true })

vim.keymap.set('n', 'S', open_git_show, { buffer = true, silent = true })

if list.has(vim.b.gitlog_args or {}, '--oneline') then
  local function rebase_interactive_autosquash_to_current_line_revision()
    local line = vim.api.nvim_get_current_line()
    local revision = vim.split(line, ' ')[1]
    if revision == nil then
      vim.notify('Invalid line:\n' .. line, vim.log.levels.ERROR)
      return
    end

    vim.cmd.tabnew()
    vim.fn.jobstart({ 'git', 'rebase', '--interactive', '--autosquash', revision }, {
      term = true,
    })
  end

  vim.keymap.set('n', 'R', rebase_interactive_autosquash_to_current_line_revision, { buffer = true })
end
