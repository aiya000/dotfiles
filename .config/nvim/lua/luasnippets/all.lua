local list = require('utils.list')

return {
  snippets = list.concat(
    require('luasnippets.all.all').snippets,
    require('luasnippets.all.kaomoji').snippets,
    require('luasnip-emoji')
  ),
  autosnippets = {},
}
