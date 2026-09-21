---nvim-treesitterのmasterブランチにあった`nvim-treesitter.query`のうち、
---nvim-FeMaco.luaが使う`get_matches()`だけを、Neovim標準のtreesitter APIで組み直したもの。
---
---mainブランチでは`lua/nvim-treesitter/query.lua`ごと消えたので、
---FeMacoの`require('nvim-treesitter.query')`をこのモジュールに差し替えて使う。
---差し替えはplugins.luaのFeMacoのspecの`init`で、`package.preload`に登録している。
---
---返す形はmasterのものに合わせてある:
---`@injection.content`のようなドット区切りのキャプチャは
---`match.injection.content.node` / `.metadata`にネストして入り、
---`(#set! injection.language "html")`のような指定は`match.injection.language`に文字列で入る。

local M = {}

---ドット区切りの名前をたどって、ネストしたテーブルに値を入れる
---@param object table
---@param path string[]
---@param value any
local function insert_to_path(object, path, value)
  local current = object

  for index = 1, #path - 1 do
    if current[path[index]] == nil then
      current[path[index]] = {}
    end
    current = current[path[index]]
  end

  current[path[#path]] = value
end

---@param name string
---@return string[]
local function split_by_dot(name)
  local result = {}

  for part in name:gmatch('([^.]+)') do
    table.insert(result, part)
  end

  return result
end

---@param bufnr integer?
---@param query_group string
---@return table[]
function M.get_matches(bufnr, query_group)
  if bufnr == nil or bufnr == 0 then
    bufnr = vim.api.nvim_get_current_buf()
  end

  local ok, parser = pcall(vim.treesitter.get_parser, bufnr)
  if ok == false or parser == nil then
    return {}
  end

  local query = vim.treesitter.query.get(parser:lang(), query_group)
  if query == nil then
    return {}
  end

  local matches = {}

  for _, tree in ipairs(parser:parse()) do
    for _, match, metadata in query:iter_matches(tree:root(), bufnr, 0, -1) do
      local prepared = {}

      -- Neovim 0.11以降、match[id]はTSNodeではなくTSNodeのリストなので、最後の1つを取る
      for id, nodes in pairs(match) do
        local name = query.captures[id]
        if name ~= nil then
          local node = nodes
          if type(nodes) == 'table' then
            node = nodes[#nodes]
          end

          insert_to_path(prepared, split_by_dot(('%s.node'):format(name)), node)
          insert_to_path(prepared, split_by_dot(('%s.metadata'):format(name)), metadata[id])
        end
      end

      -- `(#set! injection.language "html")`のような、文字列で直接指定された言語
      for key, value in pairs(metadata) do
        if type(key) == 'string' and type(value) == 'string' then
          insert_to_path(prepared, split_by_dot(key), value)
        end
      end

      table.insert(matches, prepared)
    end
  end

  return matches
end

return M
