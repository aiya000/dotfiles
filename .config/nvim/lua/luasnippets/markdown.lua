-- Combine all Markdown snippet modules
local list = require('utils.list')

return {
  snippets = list.concat(
    require('luasnippets.markdown.markdown'),
    require('luasnippets.markdown.blocks'),
    require('luasnippets.markdown.html'),
    require('luasnippets.markdown.html-attr'),
    require('luasnippets.markdown.github'),
    require('luasnippets.markdown.zenn')
  ),
  autosnippets = {},
}
