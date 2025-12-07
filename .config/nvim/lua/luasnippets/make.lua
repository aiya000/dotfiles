local list = require('utils.list')

local list = require('utils.list')

return {
  snippets = list.concat(
    require('luasnippets.make.make'),
    require('luasnippets.sh').snippets
  ),
  autosnippets = {},
}
