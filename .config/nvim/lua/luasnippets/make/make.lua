local list = require('utils.list')
local ls = require('luasnip')
local fmt = require('luasnip.extras.fmt').fmt
local sm = require('utils.luasnip').sm
local s = ls.snippet
local i = ls.insert_node
local t = ls.text_node
local f = ls.function_node

return list.concat(
  {
    s(
      'define-install',
      fmt([[
        install-{command}:
        	which {command} || {how_to_install} {command}
      ]], {
        command = i(1, 'command'),
        how_to_install = i(2, 'how_to_install'),
      })
    ),
  }
)
