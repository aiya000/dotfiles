local ls = require('luasnip')
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node
local fmt = require('luasnip.extras.fmt').fmt
local sm = ls.snippet_from_nodes
local list = require('luasnip.util.util').list
local types = require('luasnip.util.types')

local M = {
  -- Surrogate tags
  s('A_DataContext_surround', {
    t('<'),
    i(1),
    t('.DataContext>'),
    i(0),
    t('</'),
    i(1),
    t('.DataContext>'),
  }),

  s('A_Resources_surround', {
    t('<'),
    i(1),
    t('.Resources>'),
    i(0),
    t('</'),
    i(1),
    t('.Resources>'),
  }),

  s('A_View_surrond', {
    t('<'),
    i(1, 'Component'),
    t('.View>'),
    i(0),
    t('</'),
    i(1),
    t('.View>'),
  }),

  s('A_CommandBindings_surround', {
    t('<'),
    i(1),
    t('.CommandBindings>'),
    i(0),
    t('</'),
    i(1),
    t('.CommandBindings>'),
  }),

  s('A_CommandParameter_surrond', {
    t('<'),
    i(1, 'Component'),
    t('.CommandParameter>'),
    i(0),
    t('</'),
    i(1),
    t('.CommandParameter>'),
  }),

  s('A_Bindings_surrond', {
    t('<'),
    i(1, 'Component'),
    t('.Bindings>'),
    i(0),
    t('</'),
    i(1),
    t('.Bindings>'),
  }),
}

return M
