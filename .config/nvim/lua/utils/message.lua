-- Message utilities (replacement for vital#vimrc#import('Vim.Message'))

local s = require('utils.functions').s

local M = {}

-- Display error message
function M.error(message)
  vim.cmd('echohl ErrorMsg')
  vim.cmd(s'echo {vim.fn.string(message)}')
  vim.cmd('echohl None')
end

-- Display warning message
function M.warn(message)
  vim.cmd('echohl WarningMsg')
  vim.cmd(s'echo {vim.fn.string(message)}')
  vim.cmd('echohl None')
  return message -- Return message for chaining like original
end

-- Display info message
function M.info(message)
  vim.cmd(s'echo {vim.fn.string(message)}')
end

-- Display success message
function M.ok(message)
  vim.cmd('echohl ModeMsg')
  vim.cmd(s'echo {vim.fn.string(message)}')
  vim.cmd('echohl None')
end

-- Display message and wait for input
function M.ask(message)
  return vim.fn.input(message)
end

-- Display confirmation dialog
function M.confirm(message, choices, default)
  choices = choices or '&Yes\n&No'
  default = default or 1
  return vim.fn.confirm(message, choices, default)
end

return M
