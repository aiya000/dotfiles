local fmt = require('luasnip.extras.fmt').fmt
local list = require('utils.list')
local ls = require('luasnip')
local utils = require('utils.luasnip')

local i = ls.insert_node
local s = ls.snippet
local sm = utils.sm
local t = ls.text_node

local snippets = {}

vim.list_extend(
  snippets,
  sm(
    { 'function', 'func', 'fun' },
    fmt([[
      function {}({}): {}
          {}
      endfunction
    ]], {
      i(1, 'name'),
      i(2, '#:args'),
      i(3, 'Type'),
      i(4, ''),
    }
  ))
)

return {
  snippets = snippets,
  autosnippets = {},
}
