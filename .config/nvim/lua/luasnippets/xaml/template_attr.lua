local ls = require('luasnip')
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node
local fmt = require('luasnip.extras.fmt').fmt
local sm = ls.snippet_from_nodes
local list = require('luasnip.util.util').list
local types = require('luasnip.util.types')

local M = {
  s('relativesource_binding', {
    t('{Binding RelativeSource={RelativeSource AncestorType='),
    i(1, 'DataGrid'),
    t('}, Path=DataContext.'),
    i(1, 'Method'),
    t('}'),
    i(0),
  }),

  s('default_data_grid_backcolor', {
    t('Background="#FFA4A4A4"'),
    i(0),
  }),

  s('default_grid_backcolor', {
    t('Background="#FFE5E5E5"'),
    i(0),
  }),
}

return M
