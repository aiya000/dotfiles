local fmt = require('luasnip.extras.fmt').fmt
local list = require('utils.list')
local ls = require('luasnip')
local utils = require('utils.luasnip')

local sm = utils.sm
local s = ls.snippet
local i = ls.insert_node
local t = ls.text_node

return {
  -- Logical operators
  s('forall', t('\\forall')),
  s('exists', t('\\exists')),

  -- Set operators
  s('subset', t('\\subset')),
  s('in', t('\\in')),
  s('ni', t('\\ni')),
  s('mid', t('\\mid')),

  -- Arrows
  s('rightarrow', t('\\rightarrow')),
  s('Longrightarrow', t('\\Longrightarrow')),
  s('mapsto', t('\\mapsto')),

  -- Special formatting
  sm({ 'comma', 'com' }, fmt('{{{}}}', {
    i(1, ','),
  })),

  s('mathrm', t('\\mathrm{m/s}')),

  -- Fractions and operators
  sm({ 'fraction', 'frac' }, fmt('\\frac{{{}}}{{{}}}{}', {
    i(1, 'numerator'),
    i(2, 'denominator'),
    i(3, ''),
  })),

  s('sum', fmt('\\sum_{{{}}}^{{{}}}{}', {
    i(1, 'under'),
    i(2, 'over'),
    i(3, ''),
  })),
}