local list = require('utils.list')

-- LuaSnip from_lua loader用の正しい形式
return {
  snippets = list.concat(
    require('luasnippets.vim.vim-check'),
    require('luasnippets.vim.vital'),
    require('luasnippets.vim.vim'),
    require('luasnippets.vim.plugins')
  ),
  autosnippets = {}
}