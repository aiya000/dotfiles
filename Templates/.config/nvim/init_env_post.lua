local function s(text, env)
  return text:gsub('{([^}]+)}', function(expr)
    local f = load('return'  .. expr, nil, nil, env)
    return f and f() or '{' .. expr .. '}'
  end)
end

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

add_to_runtime_path(vim.fn.expand('~/git/vital.vim'))
-- add_to_runtime_path(vim.fn.expand('~/git/scratch.vim'))

---PR出すときだけ有効にする（dein.tomlには書いておくとき）用
---@param worktree? string
local function use_locally_instead(name, worktree)
  vim.call('dein#disable', name)

  local plugin_dir = vim.fn.expand(
    s('~/Repository/{name}{worktree}', {
      name = name,
      worktree = worktree == nil and '' or ('/' .. worktree)
    })
  )
  vim.opt.runtimepath = plugin_dir .. ',' .. vim.opt.runtimepath
end

-- call s:use_locally_instead('vim-gin')
use_locally_instead('vim-scratch-buffer')
use_locally_instead('vim-write-sync')
