local list = require('utils.list')

-- LuaSnip from_lua loader用の正しい形式
return {
  snippets = list.concat(
    require('luasnippets.java.android'),
    require('luasnippets.java.java'),
    require('luasnippets.java.javadoc'),
    require('luasnippets.java.javafx')
  ),
  autosnippets = {}
}