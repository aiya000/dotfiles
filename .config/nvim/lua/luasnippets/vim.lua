local list = require('utils.list')

-- LuaSnip from_lua loader用の正しい形式
return {
  snippets = list.concat(
    require('luasnippets.vim.vim-check').snippets,
    require('luasnippets.vim.vital').snippets,
    require('luasnippets.vim.vim').snippets,
    require('luasnippets.vim.plugins').snippets
  ),
  autosnippets = {},
}
