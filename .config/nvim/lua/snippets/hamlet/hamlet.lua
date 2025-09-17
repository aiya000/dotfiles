local ls = require("luasnip")
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node

local M = {
  -- Heading elements
  s('h1', {
    t('<h1'), i(1), t('>'), i(0)
  }),

  s('h2', {
    t('<h2'), i(1), t('>'), i(0)
  }),

  s('h3', {
    t('<h3'), i(1), t('>'), i(0)
  }),

  s('h4', {
    t('<h4'), i(1), t('>'), i(0)
  }),

  s('h5', {
    t('<h5'), i(1), t('>'), i(0)
  }),

  -- List elements
  s('ul', {
    t('<ul'), i(1), t('>'), i(0)
  }),

  s('li', {
    t('<li'), i(1), t('>'), i(0)
  }),
}

return M