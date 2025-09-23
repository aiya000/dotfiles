local fmt = require('luasnip.extras.fmt').fmt
local list = require('utils.list')
local ls = require('luasnip')
local utils = require('utils.luasnip')

local sm = utils.sm
local s = ls.snippet
local i = ls.insert_node
local t = ls.text_node
local f = ls.function_node

local snippets = {}

table.insert(
  snippets,
  s(
    'date',
    f(function()
      return os.date('%Y-%m-%d')
    end)
  )
)

vim.list_extend(
  snippets,
  sm(
    { 'braces', 'bra', 'block', 'bl' },
    fmt('{{{}}}', {
      i(1, 'here'),
    })
  )
)

vim.list_extend(
  snippets,
  sm(
    { 'brackets', 'ba' },
    fmt('[{}]', {
      i(1, 'here'),
    })
  )
)

vim.list_extend(
  snippets,
  sm(
    { 'parens', 'pa' },
    fmt('({})', {
      i(1, 'here'),
    })
  )
)

vim.list_extend(
  snippets,
  sm(
    { 'angles', 'an' },
    fmt('<{}>', {
      i(1, 'here'),
    })
  )
)

vim.list_extend(
  snippets,
  sm(
    { 'back_quotations', 'b' },
    fmt('`{}`', {
      i(1, 'here'),
    })
  )
)

table.insert(snippets, s('to', t('=>')))

table.insert(snippets, s('bar', t('|')))

-- Re:VIEW
table.insert(
  snippets,
  s(
    'range_surround',
    fmt(
      [[
  -- #@@range_begin({range_name})
  -- #@@range_end({range_name})
]],
      {
        range_name = i(1, 'range_name'),
      }
    )
  )
)

table.insert(
  snippets,
  s(
    'range_begin',
    fmt('-- #@@range_begin({})', {
      i(1, 'range_name'),
    })
  )
)

table.insert(
  snippets,
  s(
    'range_end',
    fmt('-- #@@range_end({})', {
      i(1, 'range_name'),
    })
  )
)

-- words
table.insert(snippets, s('readme', t('README')))

return {
  snippets = snippets,
  autosnippets = {},
}
