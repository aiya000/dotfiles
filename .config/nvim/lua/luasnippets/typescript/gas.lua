local ls = require('luasnip')
local fmt = require('luasnip.extras.fmt').fmt

local s = ls.snippet
local i = ls.insert_node

return {
  s(
    'logger_log',
    fmt(
      [[
    Logger.log({})
  ]],
      {
        i(1, 'here'),
      }
    )
  ),
}
