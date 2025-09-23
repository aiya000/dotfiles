local fmt = require('luasnip.extras.fmt').fmt
local list = require('utils.list')
local ls = require('luasnip')
local utils = require('utils.luasnip')

local sm = utils.sm
local s = ls.snippet
local i = ls.insert_node
local t = ls.text_node

local math_snippets = {}

-- Logical operators
table.insert(math_snippets, s('forall', t('\\forall')))
table.insert(math_snippets, s('exists', t('\\exists')))

-- Set operators
table.insert(math_snippets, s('subset', t('\\subset')))
table.insert(math_snippets, s('in', t('\\in')))
table.insert(math_snippets, s('ni', t('\\ni')))
table.insert(math_snippets, s('mid', t('\\mid')))

-- Arrows
table.insert(math_snippets, s('rightarrow', t('\\rightarrow')))
table.insert(math_snippets, s('Longrightarrow', t('\\Longrightarrow')))
table.insert(math_snippets, s('mapsto', t('\\mapsto')))

-- Special formatting
vim.list_extend(
  math_snippets,
  sm(
    { 'comma', 'com' },
    fmt('{{{}}}', {
      i(1, ','),
    })
  )
)

table.insert(math_snippets, s('mathrm', t('\\mathrm{m/s}')))

-- Fractions and operators
vim.list_extend(
  math_snippets,
  sm(
    { 'fraction', 'frac' },
    fmt('\\frac{{{}}}{{{}}}{}', {
      i(1, 'numerator'),
      i(2, 'denominator'),
      i(3, ''),
    })
  )
)

table.insert(
  math_snippets,
  s(
    'sum',
    fmt('\\sum_{{{}}}^{{{}}}{}', {
      i(1, 'under'),
      i(2, 'over'),
      i(3, ''),
    })
  )
)

return { snippets = math_snippets, autosnippets = {} }
