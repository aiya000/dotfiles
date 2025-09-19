local list = require('utils.list')

-- LuaSnip from_lua loader用の正しい形式
return {
  snippets = list.concat(
    require('luasnippets.tex.math'),
    require('luasnippets.tex.tex')
  ),
  autosnippets = {}
}