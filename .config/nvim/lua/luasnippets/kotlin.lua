local list = require('utils.list')

-- LuaSnip from_lua loader用の正しい形式
return {
  snippets = list.concat(
    require('luasnippets.kotlin.android'),
    require('luasnippets.kotlin.gradle'),
    require('luasnippets.kotlin.kdoc'),
    require('luasnippets.kotlin.kotlin'),
    require('luasnippets.kotlin.ktlint')
  ),
  autosnippets = {}
}