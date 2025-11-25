local list = require('utils.list')

return {
  snippets = list.concat(
    require('luasnippets.all.all'),
    require('luasnippets.all.kaomoji'),
    require('luasnip-emoji')
  ),
  autosnippets = {},
}
