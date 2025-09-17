local fmt = require('luasnip.extras.fmt').fmt
local list = require('utils.list')
local ls = require('luasnip')
local utils = require('utils.luasnip')

local sm = utils.sm
local s = ls.snippet
local i = ls.insert_node
local t = ls.text_node

-- https://github.com/aiya000/Surfingkeys.js

return {
  s('safeRun', fmt('safeRun(() => {})', {
    i(1, ''),
  })),
}