---Node.js project utilities

local M = {}

---Finds a node root directory.
---This means 'Find a project root directory or a (bun) workspace directory'.
---@return string | nil --nil if no package.json found.
function M.read_node_root_dir(base_dir)
  local realpath_cmd = vim.fn.system('realpath ' .. vim.fn.fnameescape(base_dir))
  local base_dir_real = realpath_cmd:sub(1, -2) -- Remove trailing newline

  if base_dir_real == '/' then
    return nil
  end

  local package_json_path = base_dir_real .. '/package.json'
  if vim.fn.filereadable(package_json_path) == 0 then
    return M.read_node_root_dir(base_dir_real .. '/..')
  end

  return base_dir_real
end

---@param base_dir string
---@return 'bun' | 'yarn' | 'npm' | nil
function M.check_node_project_manager(base_dir)
  local node_root = M.read_node_root_dir(base_dir)
  if node_root == nil then
    return nil
  end

  if vim.fn.filereadable(node_root .. '/bun.lockb') == 1 or vim.fn.filereadable(node_root .. '/bun.lock') == 1 then
    return 'bun'
  elseif vim.fn.filereadable(node_root .. '/yarn.lock') == 1 then
    return 'yarn'
  else
    return 'npm'
  end
end

---:cd to the directory that have a package.json, closest to the current child directory.
---NOTE:
---In some cases, this does not mean the npm root or bun root.
---If you hit a child workspace of npm workspaces or bun workspaces, :cd there.
---
---@param cd_type ':cd' | ':lcd' | ':tcd'
---@param base_dir string
function M.cd_node_root(cd_type, base_dir)
  base_dir = base_dir or vim.fn.expand('%:p:h')
  local node_dir = M.read_node_root_dir(base_dir)

  if node_dir == nil then
    print('No node directory found')
    return
  end

  vim.cmd(cd_type .. ' ' .. vim.fn.fnameescape(node_dir))
end

return M
