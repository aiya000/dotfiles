local ls = require('luasnip')
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node

local unity_snippets = {
  s("serialize_field", t("[SerializeField]")),

  s("menu_item", {
    t('[MenuItem("'), i(1, "Assets"), t("/"), i(2, "MenuName"), t('")]')
  })
}

return {
  snippets = unity_snippets,
  autosnippets = {}
}