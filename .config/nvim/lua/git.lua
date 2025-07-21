local M = {}

-- Parse git root output
local function parse_git_root(cont, stdout, stderr)
  if type(stderr) == 'table' and #stderr > 0 then
    error(table.concat(stderr, '\n'))
  end
  if type(stderr) == 'string' and stderr ~= '' then
    error(stderr)
  end

  local stdout_str = type(stdout) == 'table' and table.concat(stdout, '') or stdout

  -- Replace to the wsl2's path if git_root is a windows path (by git.exe)
  local git_root = M.apply_if(vim.fn.fnameescape(stdout_str),
    function(root) return not root:match('^/') and vim.fn.executable('wslpath') == 1 end,
    function(root_windows)
      return vim.fn.fnameescape(vim.fn.system('wslpath "' .. root_windows .. '"')):sub(1, -3)
    end
  )

  return cont(git_root)
end

function M.read_git_root(cont)
  -- Use vim.system for Neovim job handling
  vim.system({'git', 'rev-parse', '--show-toplevel'}, {
    stdout = true,
    stderr = true,
  }, function(result)
    if result.code == 0 then
      parse_git_root(cont, result.stdout, result.stderr)
    else
      cont(nil)
    end
  end)
end

local function set_git_root_to_g_vimrc(git_root)
  if git_root then
    vim.schedule(function()
      print('vimrc: a git root detected: ' .. git_root)
      vim.g.vimrc.git_root = git_root
    end)
  end
end

function M.read_git_root_to_set_g_vimrc_async()
  M.read_git_root(set_git_root_to_g_vimrc)
end

function M.read_git_root_sync()
  local result = vim.fn.system('git rev-parse --show-toplevel')
  if vim.v.shell_error ~= 0 then
    error('Failed to read a git root directory')
  end
  return result
end


-- cd to git root
function M.cd_git_root(cd)
  M.read_git_root(function(git_root)
    if git_root then
      print('vimrc: The current directory changed to: ' .. git_root)
      if type(cd) == 'string' then
        vim.cmd(cd .. ' ' .. git_root)
      else
        cd(git_root)
      end
    end
  end)
end

return M
