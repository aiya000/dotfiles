local list = require('utils.list')

return {
  snippets = list.concat(
    require('luasnippets.xml.xml').snippets,
    require('luasnippets.xml.android').snippets,
    require('luasnippets.xml.csproj').snippets
  ),
  autosnippets = {}
}