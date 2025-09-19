local fmt = require('luasnip.extras.fmt').fmt
local list = require('utils.list')
local ls = require('luasnip')
local utils = require('utils.luasnip')

local sm = utils.sm
local s = ls.snippet
local i = ls.insert_node
local t = ls.text_node
local f = ls.function_node

return {
  s('date', f(function()
    return os.date('%Y-%m-%d')
  end)),

  sm({ 'braces', 'bra', 'block', 'bl' }, fmt('{{{}}}', {
    i(1, 'here'),
  })),

  sm({ 'brackets', 'ba' }, fmt('[{}]', {
    i(1, 'here'),
  })),

  sm({ 'parens', 'pa' }, fmt('({})', {
    i(1, 'here'),
  })),

  sm({ 'angles', 'an' }, fmt('<{}>', {
    i(1, 'here'),
  })),

  sm({ 'back_quotations', 'b' }, fmt('`{}`', {
    i(1, 'here'),
  })),

  s('to', t('=>')),

  s('bar', t('|')),

  -- Re:VIEW
  s('range_surround', fmt([[
    -- #@@range_begin({range_name})
    -- #@@range_end({range_name})
  ]], {
    range_name = i(1, 'range_name'),
  })),

  s('range_begin', fmt('-- #@@range_begin({})', {
    i(1, 'range_name'),
  })),

  s('range_end', fmt('-- #@@range_end({})', {
    i(1, 'range_name'),
  })),

  -- words
  s('readme', t('README')),
}