-- Combine all Markdown snippet modules
local list = require('utils.list')

return {
  snippets = list.concat(
    require('luasnippets.markdown.markdown').snippets,
    require('luasnippets.markdown.blocks').snippets,
    require('luasnippets.markdown.html').snippets,
    require('luasnippets.markdown.subjects').snippets,
    require('luasnippets.markdown.markdown-pp').snippets,
    require('luasnippets.markdown.html-attr').snippets,
    require('luasnippets.markdown.github'),
    require('luasnippets.markdown.zenn').snippets
  ),
  autosnippets = {},
}
