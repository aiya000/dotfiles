local list = require('utils.list')

-- LuaSnip from_lua loader用の正しい形式
return {
  snippets = list.concat(
    require('luasnippets.ruby.ruby'),
    require('luasnippets.ruby.yard')
  ),
  autosnippets = {}
}