local list = require('utils.list')

return {
  snippets = list.concat(
    require('luasnippets.typescript').snippets,
    require('luasnippets.html.html').snippets,
    require('luasnippets.vue.vue')
  ),
  autosnippets = {},
}
