local fmt = require('luasnip.extras.fmt').fmt
local list = require('utils.list')
local ls = require('luasnip')
local utils = require('utils.luasnip')

local sm = utils.sm
local s = ls.snippet
local i = ls.insert_node
local t = ls.text_node

local gradle_snippets = {}

table.insert(
  gradle_snippets,
  s(
    'suppress_lint',
    fmt(
      [[
  @SuppressLint("{lint_rule}")
]],
      {
        lint_rule = i(1, 'SetJavaScriptEnabled'),
      }
    )
  )
)

return { snippets = gradle_snippets, autosnippets = {} }
