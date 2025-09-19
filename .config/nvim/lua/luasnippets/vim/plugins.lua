local fmt = require('luasnip.extras.fmt').fmt
local list = require('utils.list')
local ls = require('luasnip')
local utils = require('utils.luasnip')

local sm = utils.sm
local s = ls.snippet
local i = ls.insert_node
local t = ls.text_node

return {
  -- Themis (Vim testing framework) functions
  sm({ 'themis_log', 'log' }, fmt('call themis#log({})', {
    i(1, 'here'),
  })),

  sm({ 'themis_poi', 'tpoi' }, fmt('call themis#log($\'poi: {{{}}}\')', {
    i(1, 'here'),
  })),
}