local list = require('utils.list')

-- LuaSnip from_lua loader用の正しい形式
return {
  snippets = list.concat(
    require('luasnippets.yaml.yaml'),
    require('luasnippets.yaml.haskell')
  ),
  autosnippets = {}
}