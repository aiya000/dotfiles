local list = require('utils.list')

-- LuaSnip from_lua loader用の正しい形式
return {
  snippets = list.concat(require('luasnippets.hamlet.hamlet'), require('luasnippets.hamlet.attr')),
  autosnippets = {},
}
