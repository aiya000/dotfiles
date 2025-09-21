-- Combine all CSS snippet modules
local list = require('utils.list')

return {
  snippets = list.concat(
    require('luasnippets.css.css').snippets,
    require('luasnippets.css.tailwind').snippets
  ),
  autosnippets = {}
}