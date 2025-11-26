local ls = require('luasnip')
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node
local fmt = require('luasnip.extras.fmt').fmt
local sm = require('utils.luasnip').sm

-- Convert to proper array structure for LuaSnip compatibility
local docstring_snippets = {}

table.insert(docstring_snippets, s('doc', fmt('"""{}"""', { i(1, '#:here') })))

vim.list_extend(
  docstring_snippets,
  sm({'doc_attributes', 'attrs'}, t([[Attributes
----------]]))
)

vim.list_extend(
  docstring_snippets,
  sm({'doc_attribute', 'doc_attr', 'attr', 'doc_parameter', 'doc_param', 'param'}, fmt([[{} : {}
    {}]], { i(1, 'field_name'), i(2, 'type'), i(3, 'description') }))
)

vim.list_extend(
  docstring_snippets,
  sm({'doc_methods', 'methods'}, t([[Methods
-------]]))
)

table.insert(
  docstring_snippets,
  s(
    'doc_method',
    fmt(
      [[{}({})
    {}]],
      { i(1, 'method_name'), i(2, '#:args'), i(3, 'description') }
    )
  )
)

vim.list_extend(
  docstring_snippets,
  sm({'doc_parameters', 'parameters', 'params'}, t([[Parameters
----------]]))
)

return { snippets = docstring_snippets, autosnippets = {} }
