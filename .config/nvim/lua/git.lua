local s = require('utils.functions').s

local M = {}

---Parses git root output
---@param stdout string
---@param stderr string
---@return string --Linux/Unix形式の、git-rootのパス
local function parse_git_root(stdout, stderr)
  if type(stderr) == 'table' and #stderr > 0 then
    error(table.concat(stderr, '\n'))
  end
  if type(stderr) == 'string' and stderr ~= '' then
    error(stderr)
  end

  -- Replace to the wsl2's path if git_root is a windows path (by git.exe)
  local git_root = vim.fn.fnameescape(type(stdout) == 'table' and table.concat(stdout, '') or stdout)
  return is_windows_git_root(git_root) and vim.fn.trim(vim.system({ 'wslpath', root_windows })) or git_root
end

---Windows上のgit-rootなのか、Linux/Unix上のgit-rootなのかを判断する
---@param git_root string -- Linux/Unixパスのgit-root、もしくはWindowsパスのgit-root
local function is_windows_git_root(git_root)
  return not git_root:match('^/') and vim.fn.executable('wslpath') == 1
end

---@param on_succeed fun(git_root: string): nil -- git-rootが正常に読み込めた場合に、git-rootを渡して実行される関数
---@param on_failed? fun(error_message: string): nil -- git-rootが正常に読み込めなかった場合に、エラーメッセージを渡して実行される関数
function M.read_git_root(on_succeed, on_failed)
  vim.system({ 'git', 'rev-parse', '--show-toplevel' }, {
    text = true,
    stdout = true,
    stderr = true,
  }, function(result)
    if result.code == 0 then
      local git_root = parse_git_root(vim.fn.trim(result.stdout), vim.fn.trim(result.stderr))
      on_succeed(git_root)
    end

    local ignore = function(_) end
    (on_failed or ignore)(result.stderr)
  end)
end

---`:cd` to git root
---@param cd 'cd' | 'lcd' | 'tcd' | fun(git_root: string): nil 取得したgit-rootを処理するコマンド、もしくは関数
function M.cd_git_root(cd)
  local git_root = M.read_git_root()
  if git_root then
    print(s('vimrc: The current directory changed to: {git_root}'))
    if type(cd) == 'string' then
      vim.cmd(s('{cd} {git_root}'))
    else
      cd(git_root)
    end
  end
end

return M
