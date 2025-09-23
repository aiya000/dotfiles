local ls = require('luasnip')
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node

local pragma_snippets = {
  s('pragma_restore', {
    t('#pragma warning restore '),
    i(1, '#:errno'),
  }),
}

return {
  snippets = pragma_snippets,
  autosnippets = {},
}
