local fmt = require('luasnip.extras.fmt').fmt
local list = require('utils.list')
local ls = require('luasnip')
local utils = require('utils.luasnip')

local sm = utils.sm
local s = ls.snippet
local i = ls.insert_node
local t = ls.text_node

return {
  -- Vital.vim patterns
  s('vital_new', fmt('let {} = vital#{}#new()', {
    i(1, 's:V'),
    i(2, 'vital'),
  })),

  sm({ 'import', 'imp' }, fmt('let {} = {}.import(\'{}\')', {
    i(1, 'Name'),
    i(2, 's:V'),
    i(3, 'Module'),
  })),

  s('import_singleton', fmt('let {} = vital#{}#import(\'{}\')', {
    i(1, 'Name'),
    i(2, 'vital'),
    i(3, 'Module'),
  })),

  s('echo_error', fmt('call s:{}.error(\'{}\')', {
    i(1, 'Msg'),
    i(2, 'here'),
  })),
}