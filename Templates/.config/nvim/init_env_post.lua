local s = require('utils.functions').s

---自分が開発している前提のとき用
---@param path string
local function add_to_runtime_path(path)
  vim.opt.runtimepath = path .. ',' .. vim.opt.runtimepath

  -- 開発がWIPだとdocがなかったりするので
  local doc = path .. '/doc'
  if vim.fn.isdirectory(doc) then
    vim.cmd('execute :helptags ' .. doc)
  end
end

---PR出すときだけ有効にする（dein.tomlには書いておくとき）用
---@param worktree? string
local function use_locally_instead(name, worktree)
  vim.call('dein#disable', name)
  local worktree_dir = worktree == nil and '' or s'/{worktree}'

  local plugin_dir = vim.fn.expand(
    s'~/Repository/{name}{worktree_dir}', {
      name = name,
    }
  )
  vim.opt.runtimepath = plugin_dir .. ',' .. vim.opt.runtimepath
end

-- call s:use_locally_instead('vim-gin')
use_locally_instead('vim-scratch-buffer')
use_locally_instead('vim-write-sync')
