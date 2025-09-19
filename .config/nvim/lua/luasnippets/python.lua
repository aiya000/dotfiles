local list = require('utils.list')

-- LuaSnip from_lua loader用の正しい形式
return {
  snippets = list.concat(
    require('luasnippets.python.python'),
    require('luasnippets.python.docstring'),
    require('luasnippets.python.mypy')
  ),
  autosnippets = {}
}