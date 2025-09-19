local fmt = require('luasnip.extras.fmt').fmt
local list = require('utils.list')
local ls = require('luasnip')
local utils = require('utils.luasnip')

local sm = utils.sm
local s = ls.snippet
local i = ls.insert_node
local t = ls.text_node

return {
  snippets = {
    sm({ 'template_plugins', 'plugins', 'plug' }, fmt([[
      [[plugins]]
      repo = '{}'
    ]], {
      i(1, ''),
    })),

    s('rev', fmt("rev = '{}'", {
      i(1, 'here'),
    })),
  },
  autosnippets = {}
}