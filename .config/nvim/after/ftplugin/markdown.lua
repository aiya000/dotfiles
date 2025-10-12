local helper = require('helper')
local list = require('utils.list')
local pipe = require('utils.functions').pipe

vim.opt_local.conceallevel = 0
vim.opt_local.tabstop = 4
vim.opt_local.shiftwidth = 4
vim.opt_local.conceallevel = 0
vim.opt_local.commentstring = ' <!-- %s -->'
vim.opt_local.completefunc = 'github_complete#complete'

---Finds free port for grip server
---@param port integer
---@param rest_count_to_cancel integer --When this is 0, it stops searching and throws an error
local function find_free_port(port, rest_count_to_cancel)
  if rest_count_to_cancel == 0 then
    error('Cannot find free port. The expected port: ' .. port)
  end

  local result = vim.system({ 'ss', '-tuln' }):wait()
  if result.code ~= 0 or result.stdout == nil then
    error('Failed to check port: ' .. result.stderr)
  end

  local lines = vim.split(result.stdout, '\n')
  local free_port = vim.iter(lines):find(function(line)
    return line:match(':' .. port) ~= nil
  end)

  if free_port == nil then
    return port
  end
  return find_free_port(port + 1, rest_count_to_cancel - 1)
end

---Returns A GitHub Token for grip from environment variable,
---if the environment variable is a correct token.
---Or returns nil.
---@return string | nil
local function get_grip_token()
  return vim.env.DOTFILES_PRIVATE_GITHUB_GRIP_TOKEN:match(' ') == nil and vim.env.DOTFILES_PRIVATE_GITHUB_GRIP_TOKEN
    or nil
end

local function start_grip()
  local token = get_grip_token()
  local token_option = (token == nil) and {} or {'--pass', token}
  local filepath = vim.fn.fnameescape(vim.fn.expand('%:p'))
  local port = find_free_port(2525, 5) -- 2525 and 5 is very random value. Can change if needed
  local cmd = list.format({ 'grip', list.ss(), filepath, tostring(port) }, token_option)

  vim.cmd('vertical new')
  vim.fn.jobstart(cmd, { term = true })
  vim.cmd('quit')

  vim.fn.system(InitLua.open_on_gui .. ' http://localhost:' .. port)
end

vim.keymap.set('n', '<localleader>r', function()
  vim.cmd('PrevimOpen')
end, { buffer = true, silent = true })

vim.keymap.set('n', '<localleader><localleader>d', function()
  vim.cmd('write')
  vim.cmd('!doctoc ' .. vim.fn.expand('%'))
  vim.cmd('edit ' .. vim.fn.expand('%'))
end, { buffer = true, silent = true })

vim.keymap.set('n', '<localleader><localleader>r', start_grip, { buffer = true, silent = true })

-- TODO: Do 'gg' after glow finished
vim.keymap.set('n', '<localleader><localleader>R', function()
  vim.fn.jobstart('glow ' .. vim.fn.fnameescape(vim.fn.expand('%:p')), {
    term = true,
    vertical = true,
  })
end, { buffer = true, silent = true })

vim.keymap.set('n', '<C-k><C-f>', '<Cmd>Telescope lsp_document_symbols<CR>', { buffer = true, silent = true })
vim.keymap.set('n', '<C-g>e', '<Cmd>FeMaco<CR>', { buffer = true, silent = true })
