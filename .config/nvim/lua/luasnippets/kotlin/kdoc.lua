local fmt = require('luasnip.extras.fmt').fmt
local list = require('utils.list')
local ls = require('luasnip')
local utils = require('utils.luasnip')

local sm = utils.sm
local s = ls.snippet
local i = ls.insert_node
local t = ls.text_node

local kdoc_snippets = {}

vim.list_extend(
  kdoc_snippets,
  sm(
    { 'kdoc_comment_block', 'document_comment_block', 'doc' },
    fmt(
      [[
  /**
   * {content}
   */
]],
      {
        content = i(1, ''),
      }
    )
  )
)

vim.list_extend(
  kdoc_snippets,
  sm(
    { 'kdoc_param', 'param' },
    fmt(
      [[
  @param {param_name} {description}
]],
      {
        param_name = i(1, 'paramName'),
        description = i(2, 'description'),
      }
    )
  )
)

vim.list_extend(kdoc_snippets, sm({ 'kdoc_property', 'property' }, t('@property')))

table.insert(kdoc_snippets, s('kdoc_constructor', t('@constructor')))

vim.list_extend(
  kdoc_snippets,
  sm(
    { 'kdoc_see', 'see' },
    fmt(
      [[
  @see {identifier}
]],
      {
        identifier = i(1, 'identifier'),
      }
    )
  )
)

vim.list_extend(
  kdoc_snippets,
  sm(
    { 'kdoc_throws', 'throws' },
    fmt(
      [[
  @throws {exception_name} {when}
]],
      {
        exception_name = i(1, 'ExceptionName'),
        when = i(2, 'when'),
      }
    )
  )
)

return { snippets = kdoc_snippets, autosnippets = {} }
