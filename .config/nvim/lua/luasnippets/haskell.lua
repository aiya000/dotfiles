local list = require('utils.list')

-- LuaSnip from_lua loader用の正しい形式
return {
  snippets = list.concat(
    require("luasnippets.haskell.eta"),
    require("luasnippets.haskell.haskell"),
    require("luasnippets.haskell.hlint"),
    require("luasnippets.haskell.pragma"),
    require("luasnippets.haskell.rio"),
    require("luasnippets.haskell.type_families")
  ),
  autosnippets = {}
}