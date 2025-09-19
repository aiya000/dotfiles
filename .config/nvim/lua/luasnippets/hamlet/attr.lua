local ls = require("luasnip")
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node

local M = {
  s('class', {
    t('class="'), i(1), t('"'), i(0)
  }),
}

return M