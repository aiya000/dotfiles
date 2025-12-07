local list = require('utils.list')

local list = require('utils.list')

return {
  snippets = list.concat(
    require('luasnippets.make'),
    require('luasnippets.sh').snippets,
  ),
  autosnippets = {},
}
