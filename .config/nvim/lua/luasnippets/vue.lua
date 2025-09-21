local list = require('utils.list')

return {
  snippets = list.concat(
    require('luasnippets.vue.vue')
  ),
  autosnippets = {}
}