local fmt = require('luasnip.extras.fmt').fmt
local list = require('utils.list')
local ls = require('luasnip')
local utils = require('utils.luasnip')

local sm = utils.sm
local s = ls.snippet
local i = ls.insert_node
local t = ls.text_node

return {
  s('import', fmt('import {} ({})', {
    i(1),
    i(2, ''),
  })),

  sm({ 'import_qualified', 'imq' }, fmt('import qualified {} as {}', {
    i(1),
    i(2, ''),
  })),
}