local fmt = require('luasnip.extras.fmt').fmt
local list = require('utils.list')
local ls = require('luasnip')
local utils = require('utils.luasnip')

local sm = utils.sm
local s = ls.snippet
local i = ls.insert_node
local t = ls.text_node

local android_snippets = {}

vim.list_extend(android_snippets, sm({'poi_log', 'poi'}, fmt([[
  Log.d("poi", {message})
]], {
  message = i(1, ''),
})))

table.insert(android_snippets, s('toast_makeText_show', fmt([[
  Toast.makeText({context}, {text}, Toast.{duration}).show()
]], {
  context = i(1, 'this'),
  text = i(2, 'text'),
  duration = i(3, 'LENGTH_SHORT'),
})))

return { snippets = android_snippets, autosnippets = {} }