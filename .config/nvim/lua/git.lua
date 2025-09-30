local fn = require('utils.functions')
local c = require('chotto')

local M = {}

---Windows上のgit-rootなのか、Linux/Unix上のgit-rootなのかを判断する
---@param git_root string -- Linux/Unixパスのgit-root、もしくはWindowsパスのgit-root
local function is_windows_git_root(git_root)
  return not git_root:match('^/') and vim.fn.executable('wslpath') == 1
end

local function convert_window_git_root_to_wsl_git_root(window_git_root)
  local result = vim.system({ 'wslpath', window_git_root }):wait()
  if result.code ~= 0 then
    error('Failed to convert Windows git-root to WSL git-root: ' .. fn.to_pretty_string(result))
  end
  return fn.trim(result.stdout)
end

---Parses git root output
---@param stdout string
---@param stderr string
---@return string --Linux/Unix形式の、git-rootのパス
local function parse_git_root(stdout, stderr)
  if not c.string():safe_parse(stdout) or not c.string():safe_parse(stderr) then
    error('stdout and stderr must be string: ' .. fn.to_pretty_string({ stdout = stdout, stderr = stderr }))
  end
  local git_root = vim.fn.fnameescape(stdout)

  local function identity(x)
    return x
  end

  -- Resolve it is a path of Linux/Unix (WSL) or a path of Windows
  local convert = is_windows_git_root(git_root) and convert_window_git_root_to_wsl_git_root or identity
  return convert(git_root)
end

---@param on_succeed fun(git_root: string): nil --git-rootが正常に読み込めた場合に、git-rootを渡して実行される関数
---@param on_failed? fun(error_message: string): nil --git-rootが正常に読み込めなかった場合に、エラーメッセージを渡して実行される関数
---TODO: もうこれ同期関数でいいじゃん
function M.read_git_root(on_succeed, on_failed)
  vim.system({ 'git', 'rev-parse', '--show-toplevel' }, {
    text = true,
  }, function(result)
    if result.code == 0 then
      local git_root = parse_git_root(fn.trim(result.stdout), fn.trim(result.stderr))
      on_succeed(git_root)
    end

    local ignore = function(_) end
    (on_failed or ignore)(result.stderr)
  end)
end

---`:cd` to git root
---@param cd 'cd' | 'lcd' | 'tcd' | fun(git_root: string): nil 取得したgit-rootを処理するコマンド、もしくは関数
function M.cd_git_root(cd)
  M.read_git_root(function(git_root)
    print('The current directory changed to: ' .. git_root)
    if type(cd) == 'string' then
      vim.cmd(cd .. ' ' .. git_root)
    else
      cd(git_root)
    end
  end)
end

---@param cmd_name string
---@param args string
function M.execute_cmd_at_git_root(cmd_name, args)
  local git_root = InitLua.git_root
  if git_root == nil then
    vim.notify('Git root not found.', vim.log.levels.ERROR)
    return
  end

  local current_dir = vim.fn.getcwd()
  vim.cmd('lcd ' .. git_root)
  vim.cmd(cmd_name .. ' ' .. args)
  vim.cmd('lcd ' .. current_dir)
end

return M
