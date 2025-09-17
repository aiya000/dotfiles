local fmt = require('luasnip.extras.fmt').fmt
local list = require('utils.list')
local ls = require('luasnip')
local utils = require('utils.luasnip')

local sm = utils.sm
local s = ls.snippet
local i = ls.insert_node
local t = ls.text_node

return {
  s('doc', fmt([[
    /**
     * {}
     */
  ]], {
    i(1, ''),
  })),

  sm({'javadoc_param', 'param'}, fmt('@param {name} {description}', {
    name = i(1, ''),
    description = i(2, ''),
  })),

  s('javadoc_return', fmt('@return {description}', {
    description = i(1, ''),
  })),

  sm({'javadoc_see', 'see'}, fmt('@see {reference}', {
    reference = i(1, ''),
  })),
}