local ls = require('luasnip')
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node
local fmt = require('luasnip.extras.fmt').fmt

return {
  snippets = {
    s('hello', t('Hello from simple lua!')),
    s('simpletest', fmt('Simple test: {}', { i(1, 'works') })),
  },
  autosnippets = {},
}
