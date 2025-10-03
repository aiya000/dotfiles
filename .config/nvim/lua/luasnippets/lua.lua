local list = require('utils.list')

-- LuaSnip from_lua loader用の正しい形式
return {
  snippets = list.concat(
    require('luasnippets.lua.lua'),
    require('luasnippets.lua.lsp'),
    require('luasnippets.lua.luaCATS'),
    require('luasnippets.lua.neovim'),
    require('luasnippets.lua.templates'),
    require('luasnippets.lua.luasnip')
  ),
  autosnippets = {},
}
