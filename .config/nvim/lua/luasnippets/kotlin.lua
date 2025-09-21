local list = require('utils.list')

-- LuaSnip from_lua loader用の正しい形式
return {
  snippets = list.concat(
    require('luasnippets.kotlin.android').snippets,
    require('luasnippets.kotlin.gradle').snippets,
    require('luasnippets.kotlin.kdoc').snippets,
    require('luasnippets.kotlin.kotlin').snippets,
    require('luasnippets.kotlin.ktlint').snippets
  ),
  autosnippets = {}
}