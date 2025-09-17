local list = {}

-- Load all Haskell snippet modules
list.concat = function(...)
  local result = {}
  for _, snippets in ipairs({...}) do
    vim.list_extend(result, snippets)
  end
  return result
end

return list.concat(
  require("snippets.haskell.eta"),
  require("snippets.haskell.haskell"),
  require("snippets.haskell.hlint"),
  require("snippets.haskell.pragma"),
  require("snippets.haskell.rio"),
  require("snippets.haskell.type_families")
)