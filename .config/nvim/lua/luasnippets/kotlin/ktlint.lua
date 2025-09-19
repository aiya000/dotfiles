local fmt = require('luasnip.extras.fmt').fmt
local list = require('utils.list')
local ls = require('luasnip')
local utils = require('utils.luasnip')

local sm = utils.sm
local s = ls.snippet
local i = ls.insert_node
local t = ls.text_node

return {
  s('ktlint_disable', fmt([[
    // ktlint-disable {rule}
  ]], {
    rule = i(1, ''),
  })),

  s('ktlint_disable_all', t('// ktlint-disable')),

  s('ktlint_disable_no_wildcard_imports', t('// ktlint-disable no-wildcard-imports')),
}