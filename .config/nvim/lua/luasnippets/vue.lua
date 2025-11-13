local list = require('utils.list')
local typescript = require('luasnippets.typescript')

return {
  snippets = list.concat(
    typescript.snippets,
    require('luasnippets.vue.vue')
  ),
  autosnippets = {},
}
