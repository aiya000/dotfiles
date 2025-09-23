local fmt = require('luasnip.extras.fmt').fmt
local list = require('utils.list')
local ls = require('luasnip')
local utils = require('utils.luasnip')

local sm = utils.sm
local s = ls.snippet
local i = ls.insert_node
local t = ls.text_node

local snippets = {}

vim.list_extend(
  snippets,
  sm(
    { 'comment', 'com' },
    fmt('"//": "{}",', {
      i(1, ''),
    })
  )
)

return {
  snippets = snippets,
  autosnippets = {},
}
