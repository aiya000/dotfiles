local ls = require("luasnip")
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node

local M = {
  s('question', {
    t('===[question] '), i(1, 'caption'), t('\n'),
    i(0), t('\n'),
    t('===[/question]')
  })
}

return M