local list = require('utils.list')

-- LuaSnip from_lua loader用の正しい形式
return {
  snippets = list.concat(
    require('luasnippets.java.android').snippets,
    require('luasnippets.java.java').snippets,
    require('luasnippets.java.javadoc').snippets,
    require('luasnippets.java.javafx').snippets
  ),
  autosnippets = {},
}
