local list = require('utils.list')

-- LuaSnip from_lua loader用の正しい形式
return {
  snippets = list.concat(
    require('luasnippets.typescript.gas'),
    require('luasnippets.typescript.zod'),
    require('luasnippets.typescript.playwright'),
    require('luasnippets.typescript.typedoc'),
    require('luasnippets.typescript.jest'),
    require('luasnippets.javascript.eslint'),
    require('luasnippets.javascript.deno-lint'),
    require('luasnippets.typescript.typescript')
  ),
  autosnippets = {},
}
