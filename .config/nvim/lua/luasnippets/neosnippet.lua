-- Load all neosnippet meta snippets
local list = require('utils.list')

return {
  snippets = list.concat(
    require("luasnippets.neosnippet.neosnippet"),
    require("luasnippets.neosnippet.c_like"),
    require("luasnippets.neosnippet.html_like"),
    require("luasnippets.neosnippet.javascript_like"),
    require("luasnippets.neosnippet.haskell"),
    require("luasnippets.neosnippet.idris")
  ),
  autosnippets = {}
}