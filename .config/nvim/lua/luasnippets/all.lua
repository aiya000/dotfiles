local list = require('utils.list')
local is_luasnip_emoji_ok, luasnip_emoji = pcall(require, 'luasnip-emoji')

return {
  snippets = list.concat(
    require('luasnippets.all.all'),
    require('luasnippets.all.kaomoji'),
    is_luasnip_emoji_ok and luasnip_emoji or {}
  ),
  autosnippets = {},
}
