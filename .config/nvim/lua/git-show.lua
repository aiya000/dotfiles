---TODO: `./git-log.lua`と共通化できる気がする

local s = require('utils.functions').s

local M = {}

local function put(args)
  local args = type(args) == 'table' and args or vim.fn.split(args, ' ') -- TODO: `./git-log.lua`と同じ
  local cmd = vim.fn.extendnew({ 'git', 'show' }, args)

  local result = vim.system(cmd):wait()
  if result.code ~= 0 then
    error(result.stderr)
  end

  vim.fn.setreg('z', result.stdout)
  vim.cmd('put!=@z')
  vim.cmd('normal! G"zddgg')
end

---@param args string | string[]
function M.open_buffer(args)
  vim.cmd('enew!')
  vim.bo.buftype = 'nofile'
  put(args)
  vim.bo.filetype = 'git-show'
  vim.wo.foldmethod = 'expr'
  vim.wo.foldexpr = 'v:lua.git_show_fold_expr(v:lnum)'
end

---Alias for `open_buffer()`
---@param args string | string[]
function M.git_show(args)
  M.open_buffer(args)
end

---@param lnum integer
function _G.git_show_fold_expr(lnum)
  local current_line = vim.fn.getline(lnum)
  local next_line = vim.fn.getline(lnum + 1)

  if current_line:match('^@@') then
    return '>1'
  elseif next_line:match('^diff') or next_line:match('^@@') then
    return '<1'
  else
    return '='
  end
end

return M
