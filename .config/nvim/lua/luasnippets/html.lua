-- Combine all HTML snippet modules
local list = require('utils.list')

return {
  snippets = list.concat(
    require('luasnippets.html.bootstrap4').snippets,
    require('luasnippets.html.attr').snippets,
    require('luasnippets.html.vue').snippets,
    require('luasnippets.html.html').snippets,
    require('luasnippets.html.nativescript').snippets,
    require('luasnippets.html.template').snippets
  ),
  autosnippets = {}
}