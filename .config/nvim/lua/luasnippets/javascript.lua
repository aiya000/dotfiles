local list = require('utils.list')

-- LuaSnip from_lua loader用の正しい形式
return {
  snippets = list.concat(
    require('luasnippets.javascript.eslint'),
    require('luasnippets.javascript.deno-lint'),
    require('luasnippets.javascript.javascript'),
    require('luasnippets.javascript.surfingkeys'),
    require('luasnippets.javascript.template'),
    require('luasnippets.javascript.ts-check'),
    require('luasnippets.javascript.reactnative')
  ),
  autosnippets = {}
}