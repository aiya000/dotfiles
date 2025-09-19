local fmt = require('luasnip.extras.fmt').fmt
local ls = require('luasnip')

local s = ls.snippet

return {
  s('unk', fmt([[
    unknown
  ]], {}))
}