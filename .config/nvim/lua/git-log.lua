-- Inspired by ujihisa's vimrc
-- And deris's code (http://deris.hatenablog.jp/entry/2013/05/10/003430)

local s = require('utils.functions').s

local M = {}

---@param args string | string[]
local function put(args)
  local args = type(args) == 'table'
    and args
    or vim.fn.split(args, ' ') -- TODO: `:GitLog --oneline --pretty="%h %ad %s"`のように、オプション中の文字列にもスペースが入ってると壊れると思うので、パースする
  local cmd = vim.fn.extendnew({'git', 'log'}, args)

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
  vim.b.gitlog_args = args
  put(args)
  vim.bo.filetype = 'git-log'
  vim.wo.foldmethod = 'expr'
  vim.wo.foldexpr = 'v:lua.gitlog_fold_expr(v:lnum)'
  vim.wo.foldtext = 'v:lua.gitlog_fold_text()'
end

---Alias for `open_buffer()`
---@param args string | string[]
function M.git_log(args)
  M.open_buffer(args)
end

function _G.gitlog_fold_expr(lnum)
  local current_line = vim.fn.getline(lnum)
  local next_line = vim.fn.getline(lnum + 1)

  if current_line:match('^commit') then
    return '>1'
  elseif next_line:match('^commit') then
    return '<1'
  else
    return '='
  end
end

function _G.gitlog_fold_text()
  local month_map = {
    Jan = '01', Feb = '02', Mar = '03', Apr = '04',
    May = '05', Jun = '06', Jul = '07', Aug = '08',
    Sep = '09', Oct = '10', Nov = '11', Dec = '12',
  }

  local foldstart = vim.v.foldstart
  if not vim.fn.getline(foldstart):match('^commit') then
    return vim.fn.getline(foldstart)
  end

  local author_lnum
  if vim.fn.getline(foldstart + 1):match('^Author:') then
    author_lnum = foldstart + 1
  elseif vim.fn.getline(foldstart + 2):match('^Author:') then
    -- commitの次の行がMerge:の場合があるので
    author_lnum = foldstart + 2
  else
    -- commitの下2行がどちらもAuthor:で始まらなければ諦めて終了
    return vim.fn.getline(foldstart)
  end

  local commit_lnum
  if vim.fn.getline(author_lnum - 1):match('^commit') then
    commit_lnum = author_lnum - 1
  else
    -- commitの次の行がMerge:の場合があるので
    commit_lnum = author_lnum - 2
  end

  local date_lnum = author_lnum + 1
  local message_lnum = date_lnum + 2

  local commit = vim.fn.getline(commit_lnum):match('commit (.*)'):sub(1, 7) -- reflogと同じ文字数で表示
  local author = vim.fn.getline(author_lnum):match('Author: (.-) <')
  local date_line = vim.fn.getline(date_lnum)
  local message = vim.fn.getline(message_lnum):sub(5) -- 先頭の4文字のインデントを削除

  -- Date parsing: "Date:   Wed Mar 15 14:30:45 2023 +0900" format
  local month, day, time, year = date_line:match(' (%a%a%a) (%d+) (%d%d:%d%d):%d%d (%d%d%d%d)')

  if not month then
    return vim.fn.getline(foldstart)
  end

  local day_padded = string.format('%02d', tonumber(day))
  local datestr = string.format('%s-%s-%s', year, month_map[month], day_padded)

  return string.format('%s %s %s %s %s', commit, datestr, time, author, message)
end

return M
