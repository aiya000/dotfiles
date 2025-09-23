local list = require('utils.list')

-- LuaSnip from_lua loader用の正しい形式
return {
  snippets = list.concat(
    require('luasnippets.typescript.gas'),
    require('luasnippets.typescript.typo'),
    require('luasnippets.typescript.zod'),
    require('luasnippets.typescript.log4js'),
    require('luasnippets.typescript.playwright'),
    require('luasnippets.typescript.tslint'),
    require('luasnippets.typescript.typedoc'),
    require('luasnippets.typescript.vue'),
    require('luasnippets.typescript.jest'),
    require('luasnippets.javascript.eslint'),
    require('luasnippets.javascript.deno-lint'),
    require('luasnippets.typescript.typescript')
  ),
  autosnippets = {},
}
