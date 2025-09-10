local M = {}

local function read_git_show(args)
  vim.cmd('put!=system(\\'git show \\' .. ' .. vim.fn.string(args) .. ')')
  vim.cmd('normal! gg')
end

function M.git_show(args)
  vim.cmd('enew!')
  vim.bo.buftype = 'nofile'
  read_git_show(args)
  vim.bo.filetype = 'git-show'
  vim.wo.foldmethod = 'expr'
  vim.wo.foldexpr = 'v:lua.git_show_fold_expr(v:lnum)'
end

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