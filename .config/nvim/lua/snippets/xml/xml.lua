local fmt = require('luasnip.extras.fmt').fmt
local list = require('utils.list')
local ls = require('luasnip')
local utils = require('utils.luasnip')

local sm = utils.sm
local s = ls.snippet
local i = ls.insert_node
local t = ls.text_node

return {
  -- XML header
  s('xml_format', t('<?xml version="1.0" encoding="utf-8"?>')),

  -- Basic XML constructs
  sm({ 'comment', 'com' }, fmt('<!-- {} -->', {
    i(1, ''),
  })),

  sm({ 'single', 'sin' }, fmt('<{} />', {
    i(1, 'name'),
  })),

  sm({ 'surround', 'sur' }, fmt('<{}>{}<//{}>', {
    i(1, 'name'),
    i(2, 'here'),
    i(3, ''),
  })),
}